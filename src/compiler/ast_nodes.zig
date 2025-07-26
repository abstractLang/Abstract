pub const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Node = struct {

    data: NodeData,
    beggin: usize,
    end: usize,

    pub fn new(allocator: Allocator, data: NodeData) !*Node {
        const instance = try allocator.create(@This());
        instance.* = .{
            .data = data,
            .beggin = 0,
            .end = 0,
        };
        return instance;
    }
    pub fn deinit(s: *@This(), allocator: Allocator) void {
        s.children.deinit();
        allocator.destroy(s);
    }

};
const NodeList = std.ArrayList(*Node);

pub const NodeData = union {

    attribute: Attribute,
    variable: Variable,
    function: Function,

    value: Value,

};
pub const Attribute = struct {
    identifier: []const u8,
    args: []NodeData,
};
pub const Variable = struct {
    isconst: bool,
    type: ?*Value,
    identifier: *Value,
    value: ?*Value,
};
pub const Function = struct {
    identifier: *Value,
    type: ?*Value,
};

pub const Value = union {
    float_literal: struct {
        value: isize,
        scale: usize,
    },
    identifier: struct {
        value: []const u8,
    },
    call: struct {
        func: *Value,
        args: std.ArrayListUnmanaged(Node)
    },
};

pub const Module = struct {

    root: *Namespace,

};

pub const Namespace = struct {
    name: []const u8,
    path: []const u8,
    scripts: []const []const u8,

    children: std.ArrayList(*Node),
};

