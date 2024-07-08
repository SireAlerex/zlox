const std = @import("std");
const print = std.debug.print;
const assert = std.debug.assert;
const chunk_mod = @import("chunk.zig");
const Chunk = chunk_mod.Chunk;
const OpCode = @import("chunk.zig").OpCode;
const Value = @import("value.zig").Value;

const DEBUG = @import("main.zig").DEBUG;
const STACK_MAX = 256;

const VMError = error{
    CompileError,
    RuntimeError,
};

pub const VM = struct {
    // TODO: use optionals after vm api is finished
    chunk: *Chunk,
    ip: [*]u8,
    stack: [STACK_MAX]Value = [_]Value{.uninit} ** STACK_MAX,
    stack_top: [*]Value,

    pub fn create() VM {
        return VM{ .chunk = undefined, .ip = undefined, .stack_top = undefined };
    }
    pub fn destroy(_: *VM) void {}

    pub fn interpret(self: *VM, chunk: *Chunk) VMError!void {
        self.chunk = chunk;
        self.ip = chunk.code.items.ptr;
        self.stack_top = &self.stack;
        try self.run();
    }

    fn run(self: *VM) VMError!void {
        while (true) {
            if (comptime DEBUG) {
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
        assert(@intFromPtr(self.stack_top) - @intFromPtr(&self.stack) + @sizeOf(Value) <= @sizeOf([STACK_MAX]Value));

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

    fn reset_stack(self: *VM) void {
        self.stack_top = &self.stack;
    }
};
