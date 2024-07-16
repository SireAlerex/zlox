const std = @import("std");
const print = std.debug.print;
const assert = std.debug.assert;
const chunk_mod = @import("chunk.zig");
const Chunk = chunk_mod.Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const Compiler = @import("compiler.zig").Compiler;
const Obj = @import("object.zig").Obj;
const ObjType = @import("object.zig").ObjType;
const ObjString = @import("object.zig").ObjString;
const Table = @import("table.zig").Table;

const config = @import("main.zig").config;

pub const VMError = error{
    CompileError,
    RuntimeError,
};

pub const VM = struct {
    chunk: *Chunk,
    ip: [*]u8,
    stack: [config.STACK_SIZE]Value = [_]Value{.uninit} ** config.STACK_SIZE,
    stack_top: [*]Value,
    strings: Table,
    globals: Table,
    objects: ?*Obj = null,
    allocator: std.mem.Allocator,

    // const values
    const FALSE = Value{ .boolean = false };
    const TRUE = Value{ .boolean = true };
    const NIL = Value.nil;

    /// Must call init after creating VM
    pub fn create() VM {
        return VM{ .chunk = undefined, .ip = undefined, .stack_top = undefined, .strings = .{}, .globals = .{}, .allocator = undefined };
    }

    pub fn init(self: *VM, allocator: *const std.mem.Allocator) !void {
        self.reset_stack();
        try self.strings.init(allocator);
        try self.globals.init(allocator);
        self.allocator = allocator.*;
    }

    pub fn destroy(self: *VM) void {
        var current_obj: ?*Obj = self.objects;
        while (current_obj != null) {
            const next = current_obj.?.next;
            current_obj.?.destroy(&self.allocator);
            current_obj = next;
        }

        self.strings.free(&self.allocator);
        self.globals.free(&self.allocator);
    }

    fn init_with_chunk(self: *VM, chunk: *Chunk) void {
        self.chunk = chunk;
        self.ip = self.chunk.code.items.ptr;
    }

    pub fn interpret(self: *VM, allocator: *const std.mem.Allocator, source: *[]const u8, file: ?[]const u8) !void {
        const chunk = try Chunk.init(allocator);
        defer chunk.destroy();

        if (!(try Compiler.compile(source, chunk, self))) {
            return VMError.CompileError;
        }

        self.init_with_chunk(chunk);

        const result = self.run(file);
        if (comptime config.DEBUG_MODE) print("Instruction count: {d}\n", .{instructions});

        return result;
    }

    var instructions: u64 = 0;

    fn run(self: *VM, file: ?[]const u8) !void {
        if (comptime config.DEBUG_MODE) instructions = 0;

        while (true) {
            if (comptime config.DEBUG_MODE) {
                instructions += 1;
                print("          ", .{});
                var slot: [*]Value = &self.stack;
                if (slot == self.stack_top) {
                    print("<empty stack>", .{});
                }

                while (slot != self.stack_top) : (slot += 1) {
                    print("[ ", .{});
                    try slot[0].show();
                    print(" ]", .{});
                }
                print("\n", .{});

                _ = self.chunk.disassemble_instruction(@truncate(@intFromPtr(self.ip) - @intFromPtr(self.chunk.code.items.ptr)));
            }

            const instruction: OpCode = @enumFromInt(self.ip[0]);
            self.ip += 1;

            switch (instruction) {
                .Return => {
                    return;
                },
                .Constant => {
                    self.constant(u8, read_u8);
                },
                .ConstantLong => {
                    self.constant(u16, read_u16);
                },
                .Negate => {
                    var last_slot = self.peek_mut(0);
                    if (last_slot.* != .number) {
                        if (comptime config.TEST_MODE) {
                            self.runtime_error("Operand must be a number.", .{}, file);
                        } else self.runtime_error("Negate operand must be a number, got {s}", .{last_slot.get_type()}, file);
                        return VMError.RuntimeError;
                    }
                    last_slot.negate_mut();
                },
                .Add => {
                    const right = self.pop();
                    var left = self.pop();

                    if (!((right == .number and left == .number) or (right.is_obj_type(ObjType.String) and left.is_obj_type(ObjType.String)))) {
                        if (comptime config.TEST_MODE) {
                            self.runtime_error("Operands must be two numbers or two strings.", .{}, file);
                        } else self.runtime_error("Add operands must be two numbers or two strings, got {s} + {s}", .{ left.get_type(), right.get_type() }, file);
                        return VMError.RuntimeError;
                    }

                    const sum = Value.add(&self.allocator, left, right, self) catch {
                        self.runtime_error("Out Of Memory during string concatenation", .{}, file);
                        return VMError.RuntimeError;
                    };
                    self.push(sum);
                },
                .Sub => {
                    try self.binary_op(Value.sub_mut, "Sub operands must be number, got {s} - {s}", file);
                },
                .Mul => {
                    try self.binary_op(Value.mul_mut, "Mul operands must be number, got {s} * {s}", file);
                },
                .Div => {
                    try self.binary_op(Value.div_mut, "Div operands must be number, got {s} / {s}", file);
                },
                .False => self.push(FALSE),
                .True => self.push(TRUE),
                .Nil => self.push(NIL),
                .Not => self.peek_mut(0).not_mut(),
                .Equal => {
                    const right = self.pop();
                    self.peek_mut(0).eq_mut(right);
                },
                .NotEqual => {
                    const right = self.pop();
                    self.peek_mut(0).not_eq_mut(right);
                },
                .Greater => {
                    try self.bool_op(Value.greater_mut, "Greater operands must be number, got {s} > {s}", file);
                },
                .GreaterEqual => {
                    try self.bool_op(Value.greater_eq_mut, "GreaterEqual operands must be number, got {s} >= {s}", file);
                },
                .Less => {
                    try self.bool_op(Value.less_mut, "Less operands must be number, got {s} < {s}", file);
                },
                .LessEqual => {
                    try self.bool_op(Value.less_eq_mut, "LessEqual operands must be number, got {s} <= {s}", file);
                },
                .Print => {
                    try self.pop().show();
                    _ = try std.io.getStdOut().write("\n");
                },
                .Pop => _ = self.pop(),
                .DefineGlobal => {
                    self.define_global(read_string_u8);
                },
                .DefineGlobalLong => {
                    self.define_global(read_string_u16);
                },
                .GetGlobal => {
                    try self.get_global(read_string_u8, file);
                },
                .GetGlobalLong => {
                    try self.get_global(read_string_u16, file);
                },
                .SetGlobal => {
                    try self.set_global(read_string_u8, file);
                },
                .SetGlobalLong => {
                    try self.set_global(read_string_u16, file);
                },
                .GetLocal => {
                    self.get_local(u8, read_u8);
                },
                .GetLocalLong => {
                    self.get_local(u16, read_u16);
                },
                .SetLocal => {
                    self.set_local(u8, read_u8);
                },
                .SetLocalLong => {
                    self.set_local(u16, read_u16);
                },
                .Jump => {
                    const offset = self.read_u16();
                    self.ip += offset;
                },
                .JumpIfFalse => {
                    const offset = self.read_u16();
                    if (self.peek(0).is_falsey()) self.ip += offset;
                },
                .Loop => {
                    const offset = self.read_u16();
                    self.ip -= offset;
                },
                else => unreachable,
            }
        }

        return;
    }

    fn constant(self: *VM, T: type, read: fn (*VM) T) void {
        const value = self.chunk.get_constant(read(self));
        self.push(value);
    }

    fn bool_op(self: *VM, mutate: fn (*Value, Value) void, comptime message: []const u8, file: ?[]const u8) !void {
        const right = self.pop();
        var left = self.peek_mut(0);

        if (right != .number or left.* != .number) {
            if (comptime config.TEST_MODE) {
                self.runtime_error("Operands must be numbers.", .{}, file);
            } else self.runtime_error(message, .{ left.get_type(), right.get_type() }, file);
            return VMError.RuntimeError;
        }

        mutate(left, right);
    }

    fn binary_op(self: *VM, mutate: fn (*Value, Value) void, comptime message: []const u8, file: ?[]const u8) !void {
        const right = self.pop();
        var left = self.peek_mut(0);

        if (right != .number or left.* != .number) {
            if (comptime config.TEST_MODE) {
                self.runtime_error("Operands must be numbers.", .{}, file);
            } else self.runtime_error(message, .{ left.get_type(), right.get_type() }, file);
            return VMError.RuntimeError;
        }

        mutate(left, right);
    }

    fn define_global(self: *VM, read: fn (*VM) *ObjString) void {
        const name = read(self);
        _ = self.globals.insert(&self.allocator, name, self.peek(0));
        // pop only after being added to avoid problem with gc
        _ = self.pop();
    }

    fn get_global(self: *VM, read: fn (*VM) *ObjString, file: ?[]const u8) !void {
        const name = read(self);
        if (self.globals.get(name)) |value| {
            self.push(value);
        } else {
            self.runtime_error("Undefined variable '{s}'.", .{name.slice()}, file);
            return VMError.RuntimeError;
        }
    }

    fn set_global(self: *VM, read: fn (*VM) *ObjString, file: ?[]const u8) !void {
        const name = read(self);

        if (self.globals.insert(&self.allocator, name, self.peek(0))) {
            // delete if variable key didn't exist
            const ret = self.globals.delete(name);
            self.runtime_error("Undefined variable '{s}'.", .{name.slice()}, file);
            if (!ret) self.runtime_error("(also had a problem during global deletion)", .{}, file);
            return VMError.RuntimeError;
        }
    }

    fn get_local(self: *VM, T: type, read: fn (*VM) T) void {
        const slot = read(self);
        self.push(self.stack[slot]);
    }

    fn set_local(self: *VM, T: type, read: fn (*VM) T) void {
        const slot = read(self);
        self.stack[slot] = self.peek(0);
    }

    inline fn peek(self: *const VM, offset: usize) Value {
        return (self.stack_top - 1 - offset)[0];
    }

    inline fn peek_mut(self: *const VM, offset: usize) *Value {
        return &(self.stack_top - 1 - offset)[0];
    }

    fn read_string_u8(self: *VM) *ObjString {
        return self.chunk.get_constant(self.read_u8()).obj.as(ObjString);
    }

    fn read_string_u16(self: *VM) *ObjString {
        return self.chunk.get_constant(self.read_u16()).obj.as(ObjString);
    }

    inline fn push(self: *VM, value: Value) void {
        // asserting current_config.STACK_SIZE + sizeOf(Value) <= max_config.STACK_SIZE
        if (comptime config.DEBUG_MODE) {
            if (@intFromPtr(self.stack_top) - @intFromPtr(&self.stack) + @sizeOf(Value) > @sizeOf([config.STACK_SIZE]Value)) {
                std.debug.panic("stack overflow (stack size = {d})", .{config.STACK_SIZE});
            }
        }

        self.stack_top[0] = value;
        self.stack_top += 1;
    }

    inline fn pop(self: *VM) Value {
        self.stack_top -= 1;
        return self.stack_top[0];
    }

    fn read_u8(self: *VM) u8 {
        const byte = self.ip[0];
        self.ip += 1;
        return byte;
    }

    fn read_u16(self: *VM) u16 {
        const hi: u16 = @as(u16, self.ip[0]) << 8;
        const lo: u16 = @as(u16, self.ip[1]);
        self.ip += 2;
        return hi | lo;
    }

    pub fn reset_stack(self: *VM) void {
        self.stack_top = &self.stack;
    }

    fn runtime_error(self: *VM, comptime fmt: []const u8, args: anytype, file: ?[]const u8) void {
        print(fmt, args);
        print("\n", .{});

        const instruction = @intFromPtr(self.ip) - @intFromPtr(self.chunk.code.items.ptr) - 1;
        const line = self.chunk.get_line(@truncate(instruction)).line;
        const source = if (file != null and !config.TEST_MODE) file.? else "script";
        print("[line {d}] in {s}\n", .{ line, source });
        self.reset_stack();
    }
};
