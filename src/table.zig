const std = @import("std");
const ObjString = @import("object.zig").ObjString;
const Value = @import("value.zig").Value;
const VM = @import("vm.zig").VM;

pub const Table = struct {
    count: usize = 0,
    capacity: usize = 0,
    entries: [*]Entry = undefined,

    const TABLE_MAX_LOAD: f32 = 0.75;
    const TABLE_INIT_SIZE: usize = 1;

    pub fn init(self: *Table, allocator: *const std.mem.Allocator) !void {
        self.entries = (try allocator.alloc(Entry, TABLE_INIT_SIZE)).ptr;
        self.capacity = TABLE_INIT_SIZE;

        for (0..self.capacity) |i| {
            self.entries[i].key = null;
            self.entries[i].value = Value.nil;
        }
    }

    pub fn insert(self: *Table, allocator: *const std.mem.Allocator, key: *ObjString, value: Value) bool {
        // growing if count would be above capacity * load factor
        if (self.count + 1 > @as(usize, @intFromFloat(@as(f32, @floatFromInt(self.capacity)) * TABLE_MAX_LOAD))) {
            self.adjust_capacity(allocator, self.capacity * 2);
        }

        var entry = self.find(key);
        const is_new_key = entry.key == null;
        // checking if value is null to verify entry is truly empty and not a tombstone
        if (is_new_key and entry.value == .nil) self.count += 1;

        entry.key = key;
        entry.value = value;
        return is_new_key;
    }

    pub fn get(self: *Table, key: *ObjString) ?Value {
        if (self.count == 0) return null;

        const entry = self.find(key);
        return if (entry.key != null) entry.value else null;
    }

    pub fn delete(self: *Table, key: *ObjString) bool {
        if (self.count == 0) return false;

        // find the entry
        var entry = self.find(key);

        if (entry.key != null) {
            // place the tombstone
            entry.key = null;
            entry.value = Value.new(true);
            return true;
        } else return false;
    }

    fn find(self: *Table, key: *ObjString) *Entry {
        return find_entry(self.entries, self.capacity, key);
    }

    fn adjust_capacity(self: *Table, allocator: *const std.mem.Allocator, capacity: usize) void {
        var entries = allocator.alloc(Entry, capacity) catch unreachable;
        for (0..capacity) |i| {
            entries[i].key = null;
            entries[i].value = Value.nil;
        }

        self.count = 0;
        for (0..self.capacity) |i| {
            const entry = &self.entries[i];

            if (entry.key) |key| {
                var dest = find_entry(entries.ptr, capacity, key);
                dest.key = key;
                dest.value = entry.value;
                self.count += 1;
            } else continue;
        }

        allocator.free(self.entries[0..self.capacity]);

        self.entries = entries.ptr;
        self.capacity = capacity;
    }

    pub fn free(self: *Table, allocator: *const std.mem.Allocator) void {
        allocator.free(self.entries[0..self.capacity]);
    }

    pub fn find_string(self: *Table, chars: []const u8, hash: u32) ?*ObjString {
        if (self.count == 0) return null;

        var index = hash % self.capacity;
        while (true) {
            const entry = &self.entries[index];
            if (entry.key) |key| {
                if (key.len == chars.len and key.hash == hash and std.mem.eql(u8, key.slice(), chars)) return key;
            } else if (entry.value == .nil) return null;

            index = (index + 1) % self.capacity;
        }
    }

    /// Add all entries from parameter table to called table
    pub fn add_all(self: *Table, allocator: *const std.mem.Allocator, from: *Table) void {
        for (0..from.capacity) |i| {
            const entry = from.entries[i];
            if (entry.key) |key| {
                self.insert(allocator, key, entry.value);
            }
        }
    }
};

pub const Entry = struct {
    key: ?*ObjString,
    value: Value,
};

fn find_entry(entries: [*]Entry, capacity: usize, key: *ObjString) *Entry {
    var index = key.hash % capacity;
    var tombstone: ?*Entry = null;

    while (true) {
        const entry = &entries[index];
        if (entry.key == null) {
            if (entry.value == .nil) { // return last tombstone if found, otherwise return this empty entry
                return if (tombstone != null) tombstone.? else entry;
            } else { // set first found tombstone
                if (tombstone == null) tombstone = entry;
            }
        } else if (entry.key.? == key) return entry;

        index = (index + 1) % capacity;
    }
}
