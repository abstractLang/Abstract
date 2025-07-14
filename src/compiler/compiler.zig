const std = @import("std");
const CompileOptions = @import("CompileOptions.zig");
const fs = std.fs;
const fatal = std.process.fatal;

const nodes = @import("ast_nodes.zig");
const syntaxBuilder = @import("syntaxBuilder.zig");

/// This allocator must be used for all AST node related shit
var node_allocator: std.mem.Allocator = undefined;

pub fn compile_args(allocator: std.mem.Allocator, args: [][:0]u8) void {

    if (args.len < 0) fatal("No arguments provided for the compilation!" ++
        "Try `compilation -help` to learn how to use this command!", .{});

    var options: CompileOptions = .{};

    var i: usize = 0;
    while (i < args.len) : (i += 1) {

        //else
        options.module_directory = args[i];

    }

    compile(allocator, options)
    catch |err| @panic(@errorName(err));

}

pub fn compile(allocator: std.mem.Allocator, options: CompileOptions) !void {
    
    // Check if the options are valid
    if (options.module_directory == null) fatal("No module name provided!", .{});


    // Setting up environment
    var node_alloc_arena = std.heap.ArenaAllocator.init(allocator);
    defer node_alloc_arena.deinit();
    node_allocator = node_alloc_arena.allocator();


    const dir = std.fs.cwd().openDir(options.module_directory.?, .{
        .access_sub_paths = true,
        .iterate = true,
        .no_follow = true
    })
    catch |err| switch (err) {
        std.fs.Dir.OpenError.AccessDenied =>
            fatal("Access to directory '{s}' denyed!", .{options.module_directory.?}),
        
        std.fs.Dir.OpenError.NotDir =>
            fatal("Path do not points to a directory!", .{}),
        
        std.fs.Dir.OpenError.FileNotFound =>
            fatal("Directory '{s}' not found!", .{options.module_directory.?}),
        
        else => undefined
    };


    const namespaces = listNamespacesAndScripts(
        node_allocator,
        dir,
        options.module_directory.?,
    ) catch @panic("Unexpected");
    syntaxBuilder.buildTree(node_allocator, namespaces);

}

fn listNamespacesAndScripts(allocator: std.mem.Allocator, project_dir: std.fs.Dir, project_path: []const u8) ![]nodes.Namespace {

    const Dir = std.fs.Dir;
    const NPList = std.ArrayListUnmanaged(nodes.Namespace);
    const StringList = std.ArrayListUnmanaged([]const u8);
    const StackItem = struct {
        iter: Dir.Iterator,
        dirname_len: usize = 0,
        namespace_len: usize = 0,
        scripts: StringList = .empty
    };

    var npls: NPList = .empty;

    var stack: std.ArrayListUnmanaged(StackItem) = .empty;
    var dirname_buf: std.ArrayListUnmanaged(u8) = .{};
    var namespace_buf: std.ArrayListUnmanaged(u8) = .{};

    stack.append(allocator, .{ .iter = project_dir.iterate() }) catch @panic("OOM");

    while (stack.items.len > 0) {

        var current = &stack.items[stack.items.len - 1];

        if (try current.iter.next()) |entry| {

            if (entry.kind == .directory) {

                const last_idx = stack.items.len;

                const child = try current.iter.dir.openDir(entry.name, .{
                    .access_sub_paths = true,
                    .iterate = true,
                    .no_follow = true
                });

                dirname_buf.ensureUnusedCapacity(allocator, entry.name.len + std.fs.path.sep_str.len) catch @panic("OOM");
                dirname_buf.appendSliceAssumeCapacity(entry.name);
                dirname_buf.appendSliceAssumeCapacity(std.fs.path.sep_str);

                namespace_buf.ensureUnusedCapacity(allocator, 1 + entry.name.len) catch @panic("OOM");
                namespace_buf.appendAssumeCapacity('.');
                namespace_buf.appendSliceAssumeCapacity(entry.name);

                stack.insert(allocator, last_idx, .{
                    .iter = child.iterate(),
                    .dirname_len = dirname_buf.items.len,
                    .namespace_len = namespace_buf.items.len,
                }) catch @panic("OOM");
            }
            else if (entry.kind == .file) {
                const filename = try std.fmt.allocPrint(allocator, "{s}/{s}{s}",
                    .{project_path, dirname_buf.items, entry.name});
                try current.scripts.append(allocator, filename);
            }

        } else {
            var popped = stack.pop().?;
            popped.iter.dir.close();
            popped.scripts.shrinkAndFree(allocator, popped.scripts.items.len);

            try npls.append(allocator, .{
                .name = try allocator.dupe(u8, namespace_buf.items[0 .. popped.namespace_len]),
                .path = try allocator.dupe(u8, dirname_buf.items[0 .. popped.dirname_len]),
                .scripts = popped.scripts.items,
            });

            if (stack.getLastOrNull()) |curr| {
                dirname_buf.shrinkRetainingCapacity(curr.dirname_len);
                namespace_buf.shrinkRetainingCapacity(curr.namespace_len);
            }
        }


    }

    stack.deinit(allocator);
    dirname_buf.deinit(allocator);
    namespace_buf.deinit(allocator);

    return npls.toOwnedSlice(allocator);
}
