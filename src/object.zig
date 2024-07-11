const std = @import("std");
const Value = @import("value.zig").Value;
const VM = @import("vm.zig").VM;

pub const Obj = struct {
    type: ObjType,
    next: ?*Obj,

    pub fn allocate(allocator: *const std.mem.Allocator, obj_type: ObjType, kind: type, vm: *VM) !*kind {
        const x: *kind = try allocator.create(kind);

        const obj_ptr: *Obj = @ptrCast(x);
        obj_ptr.type = obj_type;

        // add to vm objects to track and free later
        obj_ptr.next = vm.objects;
        vm.objects = obj_ptr;

        return @ptrCast(@alignCast(obj_ptr));
    }

    pub fn show(self: *const Obj) !void {
        switch (self.type) {
            .String => {
                const str: *const ObjString = @ptrCast(@alignCast(self));
                try std.io.getStdOut().writer().print("{s}", .{str.slice});
            },
        }
    }

    pub fn destroy(self: *Obj, allocator: *const std.mem.Allocator) void {
        switch (self.type) {
            .String => {
                const str: *ObjString = @ptrCast(@alignCast(self));
                allocator.free(str.slice);
                allocator.destroy(str);
            },
        }
    }

    pub fn as(self: *Obj, target: type) *target {
        return switch (self.type) {
            .String => @ptrCast(@alignCast(self)),
        };
    }

    pub fn show_type(self: *const Obj) []const u8 {
        return switch (self.type) {
            .String => "STRING",
        };
    }
};

pub const ObjType = enum {
    String,

    pub fn get_type(kind: type) ObjType {
        switch (kind) {
            ObjString => .String,
            else => unreachable,
        }
    }
};

pub const ObjString = struct {
    obj: Obj,
    slice: []const u8,

    pub fn alloc(allocator: *const std.mem.Allocator, chars: []const u8, vm: *VM) *ObjString {
        const obj_string = Obj.allocate(allocator, ObjType.String, ObjString, vm) catch unreachable;
        obj_string.slice = chars;

        return obj_string;
    }

    pub fn copy(allocator: *const std.mem.Allocator, chars: []const u8, vm: *VM) *ObjString {
        const heap_chars = allocator.alloc(u8, chars.len) catch unreachable;
        @memcpy(heap_chars, chars.ptr);

        return alloc(allocator, heap_chars, vm);
    }

    pub fn take(allocator: *const std.mem.Allocator, chars: []const u8, vm: *VM) *ObjString {
        return alloc(allocator, chars, vm);
    }
};
