pub const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Node = struct {

    data: NodeData,

    pub fn new(allocator: Allocator, data: NodeData) !*Node {
        const instance = try allocator.create(@This());
        instance.* = .{
            .data = data,
            .children = .init(allocator)
        };
        return instance;
    }
    pub fn deinit(s: *@This(), allocator: Allocator) void {
        s.children.deinit();
        allocator.destroy(s);
    }

};
const NodeList = std.ArrayList(*Node);

const NodeData = union {
    // technically not nodes but who cares
    module: Module,
    namespace: Namespace,


};


pub const Module = struct {

    root: *Namespace,

};

pub const Namespace = struct {
    name: []const u8,
    path: []const u8,
    scripts: []const []const u8,
};
