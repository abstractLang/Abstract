const std = @import("std");
const builtin = @import("builtin");
const process = std.process;
const fatal = process.fatal;

pub const compiler = @import("compiler/compiler.zig");

var debug_allocator: std.heap.DebugAllocator(.{}) = .init;

pub fn main() !void {

    const gpa, const is_debug = gpa: {

        if (builtin.os.tag == .wasi) break :gpa .{ std.heap.wasm_allocator, false };

        break :gpa switch (builtin.mode) {
            .Debug, .ReleaseSafe => .{ debug_allocator.allocator(), true },
            .ReleaseFast, .ReleaseSmall => .{ std.heap.smp_allocator, false },
        };

    };
    defer if (is_debug) { _ = debug_allocator.deinit(); };

    const args = try process.argsAlloc(gpa);
    defer process.argsFree(gpa, args);

    try analyze_args(gpa, args);

    std.log.info("Process finished!", .{});

}

pub fn analyze_args(allocator: std.mem.Allocator, args: [][:0]u8) !void {

    if (args.len <= 1) {
        std.log.info("{s}", .{ usage });
        fatal("Expected command arguments, found 0!", .{});
    }

    if (std.mem.eql(u8, args[1], "compile")) {
        compiler.compile_args(allocator, args[1..]);
    }

}

const usage = \\Usage (will change):
    \\  compile                            - Compiles a project
;
