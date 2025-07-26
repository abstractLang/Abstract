const std = @import("std");
const root = @import("root");
const fs = std.fs;

const lexer = @import("lexer/lexer.zig");
const nodes = @import("ast_nodes.zig");

const Node = nodes.Node;
const NodeData = nodes.NodeData;
const Token = lexer.Token;


pub fn build_module_tree(allocator: std.mem.Allocator, namespaces: []nodes.Namespace) !void {
    try iterate_scripts(allocator, namespaces);
}

fn iterate_scripts(allocator: std.mem.Allocator, namespaces: []nodes.Namespace) !void {

    // Iterate all files of all namespaces
    for (namespaces) |namespace| {
        for (namespace.scripts) |script_path| {

            const file = fs.cwd().readFileAlloc(allocator, script_path, 1024 * 1024 * 1024) catch |err| b: {
                var res: anyerror![]u8 = err;

                if (err == error.FileTooBig) {
                    std.log.warn("Script file \"{s}\" has more than 1GiB! Big files can result in slow compilation and more memory usage!", .{script_path});
                    res = fs.cwd().readFileAlloc(allocator, script_path, 1024 * 1024 * 1024 * 1024);
                }

                break :b res catch |err2| std.debug.panic("Error while iterating though scripts: {s}!", .{@errorName(err2)});  
            };

            const tokens = lexer.lex_slice(allocator, file) catch @panic("OOM");
            try build_namespace_tree(
                allocator,
                namespace.name,
                namespace.path,
                tokens
            );
        }
    }

}

fn build_namespace_tree(
    allocator: std.mem.Allocator,
    namespace_name: []const u8,
    namespace_path: []const u8,
    tokens: []const Token
) !void {


    var tkn_idx: usize = 0;

    const namespace = try allocator.create(nodes.Namespace);
    errdefer allocator.destroy(namespace);

    namespace.name = namespace_name;
    namespace.path = namespace_path;
    namespace.children = .init(allocator);

    while (tkn_idx < tokens.len) : (tkn_idx += 1) {

        var off: usize = 0;
        const nodeornull = handle_tokens_failable(allocator, &off, tokens[tkn_idx..])
        catch |err| @panic(@errorName(err));
        tkn_idx += off;

        if (nodeornull) |node| try namespace.children.append(node);

    }

    tkn_idx = undefined;

}

fn handle_tokens_failable(allocator: std.mem.Allocator, off: *usize, tokens: []const Token) !?*Node {

    for (tokens, 0..) |i, idx| {
        std.log.info("{: >2} {s: <50} {s}", .{ idx, @tagName(i.kind), i.value orelse "" });
        if (i.kind == .line_feed) { std.log.info("\n", .{}); break; }
    }

    switch (tokens[off.*].kind) {
        
        .line_feed =>  return null,
        
        .at_sign_char => { // Attribute
            
            // @<identifier>
            // @<identifier>(<args...>)

            off.* += 1;
            if (tokens[off.*].kind != .identifier) return error.Attribute_ExpectedIdentifier;
            const identifier = tokens[off.*].value.?;
     
            return try Node.new(allocator, .{ .attribute = .{
                .identifier = identifier,
                .args = undefined,
            }});

        },

        .let_keyword,
        .const_keyword => { // Variable

            // <let/const> <identifier>
            // <let/const> <identifier> = <value>
            // <let/const> <type> <identifier>
            // <let/const> <type> <identifier> = value

            const isconst = tokens[off.*].kind == .const_keyword;
            var type_identifier: ?*nodes.Value = null;
            var name_identifier: *nodes.Value = undefined;
            var value: ?*nodes.Value = null;

            off.* += 1;
            if (tokens[off.*].kind != .identifier) return error.Const_ExpectedIdentifier_1;
            
            off.* += 1;
            if (tokens[off.*].kind != .equals_char) {
                type_identifier = try parse_value(allocator, off, tokens);
            }
            name_identifier = try parse_identifier(allocator, off, tokens);
            
            off.* += 1;
            if (tokens[off.*].kind == .equals_char) {
                off.* += 1;
                value = try parse_value(allocator, off, tokens);
            }

            if (tokens[off.*].kind != .line_feed) return error.General_ExpectedLineFeed;
            return try Node.new(allocator, .{ .variable = .{
                .isconst = isconst,
                .type = type_identifier,
                .identifier = name_identifier,
                .value = value,
            }});
        },

        .func_keyword => { // Function
            
            // func <indentifier>(<args...>) <type> {}
            // func <indentifier>(<args...>) {}

            const identifier = try parse_identifier(allocator, off, tokens);
            var type_val: ?*nodes.Value = undefined;

            type_val = try parse_value(allocator, off, tokens);
            
            return try Node.new(allocator, .{ .function = .{
                .identifier = identifier,
                .type = type_val,
            }});

        },

        else => std.debug.panic("{s} {}", .{ @tagName(tokens[off.*].kind), off.*})   
    }
}

fn parse_identifier(allocator: std.mem.Allocator, off: *usize, tokens: []const Token) !*nodes.Value {
    _ = tokens;

    const v = try allocator.create(nodes.Value);
    v.* = .{ .identifier = .{ .value = "fuck?" } };
    off.* += 1;

    return v;

}
fn parse_value(allocator: std.mem.Allocator, off: *usize, tokens: []const Token) !*nodes.Value {

    switch (tokens[off.*].kind) {
        .floating_number_literal => {

            const val, const scale = parseDecimal(tokens[off.*].value.?) catch |err| return switch (err) {
                error.InvalidFormat => error.Expression_DecimalInvalidFormat,
                error.InvalidCharacter => error.Expression_DecimalInvalidCharacter,
            };
            off.* += 1;
            const v = try allocator.create(nodes.Value);
            v.* = .{ .float_literal = .{
                .value = val,
                .scale = scale,
            } };           

            return v;

        },

        .identifier => return parse_identifier(allocator, off, tokens),

        else => std.debug.panic("{s} {}", .{@tagName( tokens[off.*].kind), off.* })
    }

    return null;
}

fn parseDecimal(str: []const u8) !struct { isize, usize } {
    var value: i64 = 0;
    var scale: u8 = 0;
    var is_negative = false;
    var seen_dot = false;

    var i: usize = 0;
    if (str.len > 0 and str[0] == '-') {
        is_negative = true;
        i += 1;
    }

    while (i < str.len) : (i += 1) {
        const c = str[i];
        if (c == '.') {
            if (seen_dot) return error.InvalidFormat; // mais de um ponto
            seen_dot = true;
        } else if (c >= '0' and c <= '9') {
            value = value * 10 + @as(i64, @intCast(c - '0'));
            if (seen_dot) scale += 1;
        } else {
            return error.InvalidCharacter;
        }
    }

    if (is_negative) value = -value;

    return .{ value, scale };
}
