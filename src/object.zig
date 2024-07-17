const std = @import("std");
const Value = @import("value.zig").Value;
const VM = @import("vm.zig").VM;
const Chunk = @import("chunk.zig").Chunk;

pub const Obj = extern struct {
    type: ObjType,
    next: ?*Obj,

    pub fn allocate(allocator: std.mem.Allocator, obj_type: ObjType, kind: type, vm: *VM) !*kind {
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
                const str = self.as_const(ObjString);
                _ = try std.io.getStdOut().write(str.slice());
            },
            .Function => {
                const func = self.as_const(ObjFunction);
                if (func.name) |name| {
                    _ = try std.io.getStdOut().writer().print("<fn {s}>", .{name.slice()});
                } else {
                    _ = try std.io.getStdOut().write("<script>");
                }
            },
        }
    }

    pub fn destroy(self: *Obj, allocator: *const std.mem.Allocator) void {
        switch (self.type) {
            .String => {
                const str = self.as(ObjString);
                allocator.free(str.slice());
                allocator.destroy(str);
            },
            .Function => {
                const func = self.as(ObjFunction);
                func.chunk.destroy();
                allocator.destroy(func);
            },
        }
    }

    pub fn as(self: *Obj, target: type) *target {
        return switch (self.type) {
            inline else => @ptrCast(@alignCast(self)),
        };
    }

    pub fn as_const(self: *const Obj, target: type) *const target {
        return switch (self.type) {
            inline else => @ptrCast(@alignCast(self)),
        };
    }

    pub fn show_type(self: *const Obj) []const u8 {
        return switch (self.type) {
            .String => "STRING",
            .Function => "FUNCTION",
        };
    }
};

pub const ObjType = enum(u8) {
    String,
    Function,

    pub fn get_type(kind: type) ObjType {
        switch (kind) {
            ObjString => .String,
            ObjFunction => .Function,
            else => unreachable,
        }
    }
};

pub const ObjString = extern struct {
    obj: Obj,
    chars: [*]const u8,
    len: usize,
    hash: u32,

    pub fn alloc(allocator: std.mem.Allocator, chars: []const u8, hash: u32, vm: *VM) !*ObjString {
        const obj_string = try Obj.allocate(allocator, ObjType.String, ObjString, vm);
        obj_string.chars = chars.ptr;
        obj_string.len = chars.len;
        obj_string.hash = hash;

        _ = vm.strings.insert(&allocator, obj_string, Value.nil);

        return obj_string;
    }

    pub fn copy(allocator: std.mem.Allocator, chars: []const u8, vm: *VM) !*ObjString {
        const hash = hash_string(chars);
        if (vm.strings.find_string(chars, hash)) |interned| {
            return interned;
        }

        const heap_chars = try allocator.alloc(u8, chars.len);
        @memcpy(heap_chars, chars.ptr);

        return alloc(allocator, heap_chars, hash, vm);
    }

    pub fn take(allocator: std.mem.Allocator, chars: []const u8, vm: *VM) !*ObjString {
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

pub const ObjFunction = extern struct {
    obj: Obj,
    arity: u8, // TODO: increase size if space available ? (after GC fields on Obj)
    chunk: *Chunk,
    name: ?*ObjString,

    pub fn new(allocator: std.mem.Allocator, vm: *VM) *ObjFunction {
        var function = Obj.allocate(allocator, ObjType.Function, ObjFunction, vm) catch @panic("Out Of Memory");
        function.arity = 0;
        function.name = null;
        function.chunk = Chunk.init(allocator) catch @panic("Out Of Memory");

        return function;
    }
};
