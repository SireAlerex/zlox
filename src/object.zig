const std = @import("std");
const Value = @import("value.zig").Value;
const VM = @import("vm.zig").VM;

pub const Obj = extern struct {
    type: ObjType,
    next: ?*Obj,

    pub fn allocate(allocator: *const std.mem.Allocator, obj_type: ObjType, kind: type, vm: *VM) !*kind {
        const ptr: *kind = try allocator.create(kind);

        const obj_ptr: *Obj = @ptrCast(ptr);
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
                _ = try std.io.getStdOut().write(str.slice());
            },
        }
    }

    pub fn destroy(self: *Obj, allocator: *const std.mem.Allocator) void {
        switch (self.type) {
            .String => {
                const str: *ObjString = @ptrCast(@alignCast(self));
                allocator.free(str.slice());
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

pub const ObjString = extern struct {
    obj: Obj,
    chars: [*]const u8,
    len: usize,
    hash: u32,

    pub fn alloc(allocator: *const std.mem.Allocator, chars: []const u8, hash: u32, vm: *VM) !*ObjString {
        const obj_string = try Obj.allocate(allocator, ObjType.String, ObjString, vm);
        obj_string.chars = chars.ptr;
        obj_string.len = chars.len;
        obj_string.hash = hash;

        _ = vm.strings.insert(allocator, obj_string, Value.nil);

        return obj_string;
    }

    pub fn copy(allocator: *const std.mem.Allocator, chars: []const u8, vm: *VM) !*ObjString {
        const hash = hash_string(chars);
        if (vm.strings.find_string(chars, hash)) |interned| {
            return interned;
        }

        const heap_chars = try allocator.alloc(u8, chars.len);
        @memcpy(heap_chars, chars.ptr);

        return alloc(allocator, heap_chars, hash, vm);
    }

    pub fn take(allocator: *const std.mem.Allocator, chars: []const u8, vm: *VM) !*ObjString {
        const hash = hash_string(chars);
        if (vm.strings.find_string(chars, hash)) |interned| {
            allocator.free(chars);
            return interned;
        }

        return alloc(allocator, chars, hash, vm);
    }

    fn hash_string(chars: []const u8) u32 {
        return std.hash.Fnv1a_32.hash(chars);
    }

    pub fn slice(self: *const ObjString) []const u8 {
        return self.chars[0..self.len];
    }
};
