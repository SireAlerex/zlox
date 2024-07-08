const std = @import("std");
const assert = std.debug.assert;

pub const Value = union(enum) {
    number: f64,
    nil,
    uninit,

    pub fn print(self: *const Value) void {
        switch (self.*) {
            .number => |x| std.debug.print("{d}", .{x}),
            .nil => std.debug.print("nil", .{}),
            .uninit => unreachable,
        }
    }
    pub fn new(input: anytype) Value {
        switch (@TypeOf(input)) {
            f64 => return Value{ .number = input },
            comptime_float => return Value{ .number = input },
            else => unreachable,
        }
    }

    pub inline fn negate(self: *Value) void {
        assert(self.* == .number);
        self.number = -self.number;
    }

    pub inline fn add(self: *Value, right: Value) void {
        assert(self.* == .number and right == .number);
        self.number += right.number;
    }
    pub inline fn sub(self: *Value, right: Value) void {
        assert(self.* == .number and right == .number);
        self.number -= right.number;
    }
    pub inline fn mul(self: *Value, right: Value) void {
        assert(self.* == .number and right == .number);
        self.number *= right.number;
    }

    pub inline fn div(self: *Value, right: Value) void {
        assert(self.* == .number and right == .number);
        self.number /= right.number;
    }
};
