const std = @import("std");
const root = @import("root");
const fs = std.fs;

const nodes = @import("ast_nodes.zig");


pub fn buildTree(allocator: std.mem.Allocator, namespaces: []nodes.Namespace) void {

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

            const tokens = lexer(allocator, file);
            for (tokens) |i| {
                _ = i;
                std.log.info("a", .{});
            }

        }
    }

}

const Token = union {
    integer_number_literal: []const u8,  // e.g.: 123, 0x10, 0b0110
    floating_number_literal: []const u8, // e.g.: 1.23
    string_literal: []const u8,          // e.g.: "bruh"
    character_literal: []const u8,       // e.g.: 'a', '\r'
    identifier: []const u8,              // e.g.: variable_name

    namespace_keyword: void,             // namespace
    import_keyword: void,                // import
    from_keyword: void,                  // from
    let_keyword: void,                   // let
    const_keyword: void,                 // const
    func_keyword: void,                  // func
    struct_keyword: void,                // struct
    extends_keyword: void,               // extends
    packet_keyword: void,                // packet
    enum_keyword: void,                  // enum

    switch_keyword: void,                // switch
    match_keyword: void,                 // match

    if_keyword: void,                    // if
    elif_keyword: void,                  // elif
    else_keyword: void,                  // else
    while_keyword: void,                 // while
    for_keyword: void,                   // for
    do_keyword: void,                    // do
    in_keyword: void,                    // in
    break_keyword: void,                 // break

    as_keyword: void,                    // as
    return_keyword: void,                // return

    null_keyword: void,                  // null
    true_keyword: void,                  // true
    false_keyword: void,                 // false

    left_parenthesis_char: void,         // (
    right_parenthesis_char: void,        // )

    left_brace_char: void,               // {
    right_brace_char: void,              // }

    left_square_bracket_char: void,      // [
    right_square_bracket_char: void,     // ]

    left_angle_char: void,               // <
    right_angle_char: void,              // >

    escaped_left_brace_char: void,       // \{

    plus_char: void,                     // +
    minus_char: void,                    // -
    star_char: void,                     // *
    slash_char: void,                    // /
    percent_char: void,                  // %
    equals_char: void,                   // =
    ampersand_char: void,                // &
    question_char: void,                 // ?
    bang_char: void,                     // !

    at_sign_char: void,                  // @

    right_arrow_operator: void,          // =>

    equal_operator: void,                // ==
    not_equal_operator: void,            // !=
    less_equal_operator: void,           // <=
    greater_equal_operator: void,        // >=

    and_operator: void,                  // and
    or_operator: void,                   // or

    power_operator: void,                // **

    add_assign_operator: void,           // +=
    sub_assign_operator: void,           // -=
    mul_assign_operator: void,           // *=
    div_assign_operator: void,           // /=
    mod_assign_operator: void,           // %=

    increment_operator: void,            // ++
    decrement_operator: void,            // --

    range_operator: void,                // ..

    single_quote_char: void,             // '
    double_quote_char: void,             // "

    comma_char: void,                    // ,
    dot_char: void,                      // .

    eof: void,                           // \EOF
    line_feed: void,                     // \n
};
fn lexer(allocator: std.mem.Allocator, script: []const u8) ![]const Token {
    var token_list: std.ArrayList(Token) = .init(allocator);

    var c: u8 = undefined;
    var i: usize = undefined;
    while (i < script.len) : ({ i += 1; c = script[i]; }) {



    }

    return token_list.toOwnedSlice(token_list.items.len);
}
