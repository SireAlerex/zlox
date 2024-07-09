const std = @import("std");
const print = std.debug.print;
const assert = std.debug.assert;
const chunk_mod = @import("chunk.zig");
const Chunk = chunk_mod.Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;
const Compiler = @import("compiler.zig").Compiler;

const DEBUG_MODE = @import("main.zig").config.DEBUG_MODE;
const STACK_SIZE = @import("main.zig").config.STACK_SIZE;

const VMError = error{
    CompileError,
    RuntimeError,
};

pub const VM = struct {
    chunk: *Chunk,
    ip: [*]u8,
    stack: [STACK_SIZE]Value = [_]Value{.uninit} ** STACK_SIZE,
    stack_top: [*]Value,

    /// Must call reset_stack after creating VM
    pub fn create() VM {
        return VM{ .chunk = undefined, .ip = undefined, .stack_top = undefined };
    }

    fn init_with_chunk(self: *VM, chunk: *Chunk) void {
        self.chunk = chunk;
        self.ip = self.chunk.code.items.ptr;
    }

    pub fn interpret(self: *VM, allocator: *const std.mem.Allocator, source: *[]const u8) !void {
        const chunk = try Chunk.init(allocator);

        if (!(try Compiler.compile(source, chunk))) {
            chunk.destroy();
            return VMError.CompileError;
        }

        self.init_with_chunk(chunk);

        const result = self.run();
        chunk.destroy();

        return result;
    }

    fn run(self: *VM) VMError!void {
        while (true) {
            if (comptime DEBUG_MODE) {
                print("          ", .{});
                var slot: [*]Value = &self.stack;
                if (slot == self.stack_top) {
                    print("<empty stack>", .{});
                }

                while (slot != self.stack_top) : (slot += 1) {
                    print("[ ", .{});
                    slot[0].print();
                    print(" ]", .{});
                }
                print("\n", .{});

                _ = self.chunk.disassemble_instruction(@truncate(@intFromPtr(self.ip) - @intFromPtr(self.chunk.code.items.ptr)));
            }

            const instruction: OpCode = @enumFromInt(self.ip[0]);
            self.ip += 1;

            switch (instruction) {
                .Return => {
                    self.pop().print();
                    print("\n", .{});
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
                    self.stack_top[0].negate();
                    self.stack_top += 1;
                },
                .Add => {
                    const right = self.pop();
                    var left = self.pop();
                    left.add(right);
                    self.push(left);
                },
                .Sub => {
                    const right = self.pop();
                    var left = self.pop();
                    left.sub(right);
                    self.push(left);
                },
                .Mul => {
                    const right = self.pop();
                    var left = self.pop();
                    left.mul(right);
                    self.push(left);
                },
                .Div => {
                    const right = self.pop();
                    var left = self.pop();
                    left.div(right);
                    self.push(left);
                },
                else => unreachable,
            }
        }

        return;
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
};
