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

const DEBUG_MODE = @import("main.zig").config.DEBUG_MODE;
const STACK_SIZE = @import("main.zig").config.STACK_SIZE;

pub const VMError = error{
    CompileError,
    RuntimeError,
};

pub const VM = struct {
    chunk: *Chunk,
    ip: [*]u8,
    stack: [STACK_SIZE]Value = [_]Value{.uninit} ** STACK_SIZE,
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

        return result;
    }

    fn run(self: *VM, file: ?[]const u8) !void {
        while (true) {
            if (comptime DEBUG_MODE) {
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
                    const value = self.chunk.get_constant(self.read_u8());
                    self.push(value);
                },
                .ConstantLong => {
                    const value = self.chunk.get_constant(self.read_u16());
                    self.push(value);
                },
                .Negate => {
                    self.stack_top -= 1;

                    if (self.stack_top[0] != .number) {
                        self.runtime_error("Negate operand must be a number, got {s}", .{self.stack_top[0].get_type()}, file);
                        return VMError.RuntimeError;
                    }
                    self.stack_top[0].negate();

                    self.stack_top += 1;
                },
                .Add => {
                    const right = self.pop();
                    var left = self.pop();

                    if (!((right == .number and left == .number) or (right.is_obj_type(ObjType.String) and left.is_obj_type(ObjType.String)))) {
                        self.runtime_error("Add operands must be two numbers or two strings, got {s} + {s}", .{ left.get_type(), right.get_type() }, file);
                        return VMError.RuntimeError;
                    }

                    self.push(Value.add(&self.allocator, left, right, self));
                },
                .Sub => {
                    const right = self.pop();
                    var left = self.pop();

                    if (right != .number or left != .number) {
                        self.runtime_error("Sub operands must be number, got {s} - {s}", .{ left.get_type(), right.get_type() }, file);
                        return VMError.RuntimeError;
                    }

                    left.sub(right);
                    self.push(left);
                },
                .Mul => {
                    const right = self.pop();
                    var left = self.pop();

                    if (right != .number or left != .number) {
                        self.runtime_error("Mul operands must be number, got {s} * {s}", .{ left.get_type(), right.get_type() }, file);
                        return VMError.RuntimeError;
                    }

                    left.mul(right);
                    self.push(left);
                },
                .Div => {
                    const right = self.pop();
                    var left = self.pop();

                    if (right != .number or left != .number) {
                        self.runtime_error("Div operands must be number, got {s} / {s}", .{ left.get_type(), right.get_type() }, file);
                        return VMError.RuntimeError;
                    }

                    left.div(right);
                    self.push(left);
                },
                .False => self.push(FALSE),
                .True => self.push(TRUE),
                .Nil => self.push(NIL),
                .Not => self.push(self.pop().is_falsey()),
                .Equal => {
                    const right = self.pop();
                    self.push(Value{ .boolean = self.pop().eq(right) });
                },
                .NotEqual => {
                    const right = self.pop();
                    self.push(Value{ .boolean = !self.pop().eq(right) });
                },
                .Greater => {
                    const right = self.pop();
                    var left = self.pop();

                    if (right != .number or left != .number) {
                        self.runtime_error("Greater operands must be number, got {s} > {s}", .{ left.get_type(), right.get_type() }, file);
                        return VMError.RuntimeError;
                    }

                    self.push(Value{ .boolean = left.greater(right) });
                },
                .GreaterEqual => {
                    const right = self.pop();
                    var left = self.pop();

                    if (right != .number or left != .number) {
                        self.runtime_error("GreaterEqual operands must be number, got {s} >= {s}", .{ left.get_type(), right.get_type() }, file);
                        return VMError.RuntimeError;
                    }

                    self.push(Value{ .boolean = left.greater_eq(right) });
                },
                .Less => {
                    const right = self.pop();
                    var left = self.pop();

                    if (right != .number or left != .number) {
                        self.runtime_error("Less operands must be number, got {s} < {s}", .{ left.get_type(), right.get_type() }, file);
                        return VMError.RuntimeError;
                    }

                    self.push(Value{ .boolean = left.less(right) });
                },
                .LessEqual => {
                    const right = self.pop();
                    var left = self.pop();

                    if (right != .number or left != .number) {
                        self.runtime_error("LessEqual operands must be number, got {s} <= {s}", .{ left.get_type(), right.get_type() }, file);
                        return VMError.RuntimeError;
                    }

                    self.push(Value{ .boolean = left.less_eq(right) });
                },
                .Print => {
                    try self.pop().show();
                    _ = try std.io.getStdOut().write("\n");
                },
                .Pop => _ = self.pop(),
                .DefineGlobal => {
                    const name = self.read_string_u8();
                    _ = self.globals.insert(&self.allocator, name, self.peek(0));
                    // pop only after being added to avoid problem with gc
                    _ = self.pop();
                },
                .GetGlobal => {
                    const name = self.read_string_u8();
                    if (self.globals.get(name)) |value| {
                        self.push(value);
                    } else {
                        self.runtime_error("Undefined variable '{s}'", .{name.slice()}, file);
                        return VMError.RuntimeError;
                    }
                },
                .SetGlobal => {
                    const name = self.read_string_u8();

                    if (self.globals.insert(&self.allocator, name, self.peek(0))) {
                        // delete if variable key didn't exist
                        _ = self.globals.delete(name); // TODO: add check for deletion
                        self.runtime_error("Undefined variable '{s}'", .{name.slice()}, file);
                        return VMError.RuntimeError;
                    }
                },
                else => unreachable,
            }
        }

        return;
    }

    inline fn peek(self: *const VM, offset: usize) Value {
        return (self.stack_top - 1 - offset)[0];
    }

    fn read_string_u8(self: *VM) *ObjString {
        return self.chunk.get_constant(self.read_u8()).obj.as(ObjString);
    }

    inline fn push(self: *VM, value: Value) void {
        // asserting current_stack_size + sizeOf(Value) <= max_stack_size
        if (comptime DEBUG_MODE) {
            if (@intFromPtr(self.stack_top) - @intFromPtr(&self.stack) + @sizeOf(Value) > @sizeOf([STACK_SIZE]Value)) {
                std.debug.panic("stack overflow (stack size = {d})", .{STACK_SIZE});
            }
        }

        self.stack_top[0] = value;
        self.stack_top += 1;
    }

    inline fn pop(self: *VM) Value {
        self.stack_top -= 1;
        return self.stack_top[0];
    }

    inline fn read_u8(self: *VM) u8 {
        const byte = self.ip[0];
        self.ip += 1;
        return byte;
    }

    inline fn read_u16(self: *VM) u16 {
        const hi: u16 = @as(u16, self.ip[0]) << 8;
        const lo: u16 = @as(u16, self.ip[1]);
        self.ip += 2;
        return hi | lo;
    }

    pub fn reset_stack(self: *VM) void {
        self.stack_top = &self.stack;
    }

    fn runtime_error(self: *VM, comptime fmt: []const u8, args: anytype, maybe_file: ?[]const u8) void {
        print(fmt, args);
        print("\n", .{});

        const instruction = @intFromPtr(self.ip) - @intFromPtr(self.chunk.code.items.ptr) - 1;
        const line = self.chunk.get_line(@truncate(instruction)).line;
        const source = if (maybe_file) |file| file else "script";
        print("[line {d}] in {s}\n", .{ line, source });
        self.reset_stack();
    }
};
