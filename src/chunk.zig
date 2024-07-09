const std = @import("std");
const print = std.debug.print;
const assert = std.debug.assert;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const Value = @import("value.zig").Value;

pub const OpCode = enum(u8) {
    Return,
    Constant,
    ConstantLong,
    Negate,
    Add,
    Sub,
    Mul,
    Div,
    _,
};

pub const Chunk = struct {
    code: ArrayListUnmanaged(u8),
    constants: ArrayListUnmanaged(Value),
    lines: ArrayListUnmanaged(Line),
    allocator: *const std.mem.Allocator,

    pub fn init(allocator: *const std.mem.Allocator) !*Chunk {
        var self = try allocator.create(Chunk);
        self.code = try ArrayListUnmanaged(u8).initCapacity(allocator.*, 4);
        self.constants = try ArrayListUnmanaged(Value).initCapacity(allocator.*, 4);
        self.lines = try ArrayListUnmanaged(Line).initCapacity(allocator.*, 4);
        self.allocator = allocator;
        return self;
    }

    pub fn destroy(self: *Chunk) void {
        self.code.deinit(self.allocator.*);
        self.constants.deinit(self.allocator.*);
        self.lines.deinit(self.allocator.*);
        self.allocator.destroy(self);
    }

    fn len(self: *Chunk) usize {
        return self.code.items.len;
    }

    fn constants_len(self: *Chunk) usize {
        return self.constants.items.len;
    }

    fn lines_len(self: *Chunk) usize {
        return self.lines.items.len;
    }

    pub fn write(self: *Chunk, byte: anytype, line: u32) !void {
        switch (@TypeOf(byte)) {
            inline comptime_int, u8 => try self.code.append(self.allocator, byte),
            usize => try self.code.append(self.allocator, @truncate(byte)),
            OpCode => try self.code.append(self.allocator, @intFromEnum(byte)),
            else => unreachable,
        }

        if (self.lines_len() != 0 and line == self.lines.items.ptr[self.lines_len() - 1].line) {
            // increment last line
            const last = &self.lines.items.ptr[self.lines_len() - 1];
            last.* = Line{ .line = line, .count = last.count + 1 };
        } else {
            // append a new line with a base count of 1
            try self.lines.append(self.allocator, Line{ .line = line, .count = 1 });
        }
    }

    pub fn write_constant(self: *Chunk, value: Value, line: u32) !void {
        try self.constants.append(self.allocator, value);
        const index = self.constants_len() - 1;
        // const index = 256;
        if (index < std.math.maxInt(u8)) {
            try self.write(OpCode.Constant, line);
            try self.write(index, line);
        } else if (index < std.math.maxInt(u16)) {
            try self.write(OpCode.ConstantLong, line);

            const lo: u8 = @truncate(index);
            const hi: u8 = @truncate(index >> 8);
            // writes high bits then low bits
            try self.write(hi, line);
            try self.write(lo, line);
        } else {
            unreachable;
        }
    }

    pub fn dissasemble_chunk(self: *Chunk, name: []const u8) void {
        print("== {s} ==\n", .{name});

        var offset: u32 = 0;
        while (offset < self.len()) {
            offset = self.disassemble_instruction(offset);
        }
    }

    pub fn disassemble_instruction(self: *Chunk, offset: u32) u32 {
        print("{:0>4} ", .{offset});
        if (offset > 0 and self.get_line(offset).line == self.get_line(offset - 1).line) {
            print("   | ", .{});
        } else {
            print("{: >4} ", .{self.get_line(offset).line});
        }

        const instruction: OpCode = @enumFromInt(self.get(offset));
        switch (instruction) {
            .Return => return simple_instruction("OP_RETURN", offset),
            .Constant => return self.constant_instruction("OP_CONSTANT", offset),
            .ConstantLong => return self.constant_long_instruction("OP_CONSTANT_LONG", offset),
            .Negate => return simple_instruction("OP_NEGATE", offset),
            .Add => return simple_instruction("OP_ADD", offset),
            .Sub => return simple_instruction("OP_SUB", offset),
            .Mul => return simple_instruction("OP_MUL", offset),
            .Div => return simple_instruction("OP_DIV", offset),
            else => {
                print("Unknown opcode {}\n", .{instruction});
                return offset + 1;
            },
        }
    }

    fn get(self: *Chunk, offset: u32) u8 {
        return self.code.items.ptr[offset];
    }

    pub fn get_constant(self: *Chunk, offset: u16) Value {
        assert(offset < self.constants_len());
        return self.constants.items.ptr[offset];
    }

    fn get_line(self: *Chunk, offset: u32) Line {
        var index = offset;
        for (self.lines.items) |line| {
            if (index < line.count) {
                return line;
            } else {
                index -= line.count;
            }
        }
        unreachable;
    }

    fn simple_instruction(name: []const u8, offset: u32) u32 {
        print("{s}\n", .{name});
        return offset + 1;
    }

    fn constant_instruction(self: *Chunk, name: []const u8, offset: u32) u32 {
        const byte = self.get(offset + 1);
        print("{s}{d: >8} -> '", .{ name, byte });
        self.get_constant(byte).print();
        print("'\n", .{});

        return offset + 2;
    }

    fn constant_long_instruction(self: *Chunk, name: []const u8, offset: u32) u32 {
        const hi = self.get(offset + 1);
        const lo = self.get(offset + 2);
        const index: u16 = (@as(u16, hi) << 8) | @as(u16, lo);
        print("{s}{d: >8} -> '", .{ name, index });
        self.get_constant(index).print();
        print("'\n", .{});

        return offset + 3;
    }

    pub fn show(self: *const Chunk) void {
        std.debug.print("chunk: {}\n", .{self});
    }
};

const Line = struct {
    line: u32,
    count: u32,
};
