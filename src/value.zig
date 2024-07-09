const std = @import("std");
const assert = std.debug.assert;

pub const Value = union(enum) {
    number: f64,
    boolean: bool,
    nil,
    uninit,

    pub fn show(self: *const Value) void {
        switch (self.*) {
            .number => |x| std.debug.print("{d}", .{x}),
            .boolean => |b| std.debug.print("{any}", .{b}),
            .nil => std.debug.print("nil", .{}),
            .uninit => unreachable,
        }
    }

    pub fn new(input: anytype) Value {
        return switch (@TypeOf(input)) {
            f64, comptime_float => Value{ .number = input },
            bool => Value{ .boolean = input },
            else => unreachable,
        };
    }

    pub fn is_falsey(self: *const Value) Value {
        return Value{ .boolean = self.* == .nil or (self.* == .boolean and !self.*.boolean) };
    }

    pub fn get_type(self: *const Value) []const u8 {
        return switch (self.*) {
            .number => "NUMBER",
            .boolean => "BOOLEAN",
            .nil => "NIL",
            .uninit => unreachable,
        };
    }

    pub fn eq(self: *const Value, right: Value) bool {
        return switch (right) {
            .nil => self.* == .nil,
            .number => |n| self.* == .number and self.number == n,
            .boolean => |b| self.* == .boolean and self.boolean == b,
            else => unreachable,
        };
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

    pub inline fn greater(self: *Value, right: Value) bool {
        assert(self.* == .number and right == .number);
        return self.number > right.number;
    }

    pub inline fn greater_eq(self: *Value, right: Value) bool {
        assert(self.* == .number and right == .number);
        return self.number >= right.number;
    }

    pub inline fn less(self: *Value, right: Value) bool {
        assert(self.* == .number and right == .number);
        return self.number < right.number;
    }

    pub inline fn less_eq(self: *Value, right: Value) bool {
        assert(self.* == .number and right == .number);
        return self.number <= right.number;
    }
};
