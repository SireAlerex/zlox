const std = @import("std");
const assert = std.debug.assert;
const Obj = @import("object.zig").Obj;
const ObjString = @import("object.zig").ObjString;
const ObjType = @import("object.zig").ObjType;
const VM = @import("vm.zig").VM;

pub const Value = union(enum) {
    number: f64,
    boolean: bool,
    obj: *Obj,
    nil,
    uninit,

    pub fn show(self: *const Value) !void {
        switch (self.*) {
            .number => |x| try std.io.getStdOut().writer().print("{d}", .{x}),
            .boolean => |b| try std.io.getStdOut().writer().print("{any}", .{b}),
            .nil => _ = try std.io.getStdOut().write("nil"),
            .obj => |obj| try obj.show(),
            else => unreachable,
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
            .obj => self.obj.show_type(),
            .nil => "NIL",
            else => unreachable,
        };
    }

    pub fn eq(self: Value, right: Value) bool {
        return switch (right) {
            .nil => self == .nil,
            .number => |n| self == .number and self.number == n,
            .boolean => |b| self == .boolean and self.boolean == b,
            .obj => |right_obj| self == .obj and self.obj == right_obj,
            else => unreachable,
        };
    }

    pub fn eq_mut(self: *Value, right: Value) void {
        self.* = Value{ .boolean = Value.eq(self.*, right) };
    }

    pub fn not_eq_mut(self: *Value, right: Value) void {
        self.* = Value{ .boolean = !Value.eq(self.*, right) };
    }

    pub inline fn negate_mut(self: *Value) void {
        self.number = -self.number;
    }

    pub fn add(allocator: *const std.mem.Allocator, left: Value, right: Value, vm: *VM) !Value {
        if (left == .number) {
            return Value{ .number = left.number + right.number };
        } else { // must be String
            const left_string = left.obj.as(ObjString);
            const right_string = right.obj.as(ObjString);

            const len = left_string.len + right_string.len;
            const chars = try allocator.alloc(u8, len);
            for (0..chars.len) |i| {
                if (i < left_string.len) {
                    chars[i] = left_string.chars[i];
                } else {
                    chars[i] = right_string.chars[i - left_string.len];
                }
            }

            const str = try ObjString.take(allocator, chars, vm);
            return Value{ .obj = @ptrCast(str) };
        }
    }

    pub inline fn sub(left: Value, right: Value) Value {
        return Value{ .number = left.number - right.number };
    }

    pub inline fn mul(left: Value, right: Value) Value {
        return Value{ .number = left.number * right.number };
    }

    pub inline fn div(left: Value, right: Value) Value {
        return Value{ .number = left.number / right.number };
    }

    pub inline fn greater(left: Value, right: Value) bool {
        return left.number > right.number;
    }

    pub inline fn greater_eq(left: Value, right: Value) bool {
        return left.number >= right.number;
    }

    pub inline fn less(left: Value, right: Value) bool {
        return left.number < right.number;
    }

    pub inline fn less_eq(left: Value, right: Value) bool {
        return left.number <= right.number;
    }

    pub fn sub_mut(self: *Value, right: Value) void {
        self.number -= right.number;
    }

    pub fn mul_mut(self: *Value, right: Value) void {
        self.number *= right.number;
    }

    pub fn div_mut(self: *Value, right: Value) void {
        self.number /= right.number;
    }

    pub fn greater_mut(self: *Value, right: Value) void {
        self.* = Value{ .boolean = self.number > right.number };
    }

    pub fn greater_eq_mut(self: *Value, right: Value) void {
        self.* = Value{ .boolean = self.number >= right.number };
    }

    pub fn less_mut(self: *Value, right: Value) void {
        self.* = Value{ .boolean = self.number < right.number };
    }

    pub fn less_eq_mut(self: *Value, right: Value) void {
        self.* = Value{ .boolean = self.number <= right.number };
    }

    pub fn is_obj_type(self: *const Value, kind: ObjType) bool {
        return self.* == .obj and self.obj.type == kind;
    }
};
