const std = @import("std");

pub const TokenKind = enum {
    undef,

    integer_number_literal,       // e.g.: 123, 0x10, 0b0110
    floating_number_literal,      // e.g.: 1.23
    string_literal,               // e.g.: "bruh"
    character_literal,            // e.g.: 'a', '\r'
    escaped_character_literal,    // e.g.: 'a', '\r'
    identifier,                   // e.g.: variable_name

    namespace_keyword,            // namespace
    import_keyword,               // import
    from_keyword,                 // from
    let_keyword,                  // let
    const_keyword,                // const
    func_keyword,                 // func
    struct_keyword,               // struct
    extends_keyword,              // extends
    packet_keyword,               // packet
    enum_keyword,                 // enum

    switch_keyword,               // switch
    match_keyword,                // match

    if_keyword,                   // if
    elif_keyword,                 // elif
    else_keyword,                 // else
    while_keyword,                // while
    for_keyword,                  // for
    do_keyword,                   // do
    in_keyword,                   // in
    break_keyword,                // break

    as_keyword,                   // as
    return_keyword,               // return

    null_keyword,                 // null
    true_keyword,                 // true
    false_keyword,                // false

    left_parenthesis_char,        // (
    right_parenthesis_char,       // )

    left_brace_char,              // {
    right_brace_char,             // }

    left_square_bracket_char,     // [
    right_square_bracket_char,    // ]

    left_angle_char,              // <
    right_angle_char,             // >

    escaped_left_brace_char,      // \{

    plus_char,                    // +
    minus_char,                   // -
    star_char,                    // *
    slash_char,                   // /
    reverse_slash_char,           // \
    percent_char,                 // %
    equals_char,                  // =
    ampersand_char,               // &
    question_char,                // ?
    bang_char,                    // !

    at_sign_char,                 // @

    right_arrow_operator,         // =>

    equal_operator,               // ==
    not_equal_operator,           // !=
    less_equal_operator,          // <=
    greater_equal_operator,       // >=

    and_operator,                 // and
    or_operator,                  // or

    power_operator,               // **

    add_assign_operator,          // +=
    sub_assign_operator,          // -=
    mul_assign_operator,          // *=
    div_assign_operator,          // /=
    mod_assign_operator,          // %=
    pow_assign_operator,          // **=

    increment_operator,           // ++
    decrement_operator,           // --

    range_operator,               // ..

    single_quote_char,            // '
    double_quote_char,            // "

    comma_char,                   // ,
    dot_char,                     // .

    eof,                          // \EOF
    line_feed,                    // \n
};
pub const Token = struct {
    kind: TokenKind,
    value: ?[]const u8 = null,
};

const dictionary = [_]struct { []const u8, TokenKind } {
    // keywords
    .{"namespace", TokenKind.namespace_keyword },
    .{"import", TokenKind.import_keyword },
    .{"from", TokenKind.from_keyword },

    .{"let", TokenKind.let_keyword },
    .{"const", TokenKind.const_keyword },
    .{"func", TokenKind.func_keyword },
    .{"struct", TokenKind.struct_keyword },
    .{"extends", TokenKind.extends_keyword },
    .{"packet", TokenKind.packet_keyword },
    .{"enum", TokenKind.enum_keyword },

    .{"switch", TokenKind.switch_keyword },
    .{"match", TokenKind.match_keyword },
    .{"if", TokenKind.if_keyword },
    .{"elif", TokenKind.elif_keyword },
    .{"else", TokenKind.else_keyword },

    .{"while", TokenKind.while_keyword },
    .{"for", TokenKind.for_keyword },
    .{"do", TokenKind.do_keyword },
    .{"in", TokenKind.in_keyword },
    .{"break", TokenKind.break_keyword },

    .{"return", TokenKind.return_keyword },

    .{"as", TokenKind.as_keyword },

    // values
    .{"null", TokenKind.null_keyword },
    .{"true", TokenKind.true_keyword },
    .{"false", TokenKind.false_keyword },

    // operators
    .{"and", TokenKind.and_operator },
    .{"or", TokenKind.or_operator },
};
const line_juctions = .{
    .only_left = .{
        TokenKind.right_brace_char,
        TokenKind.right_parenthesis_char,
    },
    .only_right = .{
        TokenKind.left_brace_char,
        TokenKind.left_square_bracket_char,
        TokenKind.escaped_left_brace_char,
        TokenKind.comma_char,
    },
    .both = .{
        TokenKind.dot_char,
        TokenKind.range_operator,

        TokenKind.plus_char, TokenKind.minus_char, TokenKind.power_operator,
        TokenKind.star_char, TokenKind.percent_char, TokenKind.equals_char,

        TokenKind.right_arrow_operator, TokenKind.less_equal_operator,
        TokenKind.equal_operator, TokenKind.not_equal_operator,
        TokenKind.left_angle_char,  TokenKind.right_angle_char,
        TokenKind.less_equal_operator, TokenKind.greater_equal_operator,

        TokenKind.add_assign_operator, TokenKind.mul_assign_operator,
        TokenKind.sub_assign_operator, TokenKind.div_assign_operator,
        TokenKind.mod_assign_operator, TokenKind.pow_assign_operator,
    },
};
const lang_symbols: []const u8  = "=+-*/!@$%|:;.?<>";
const lang_numerics: []const u8 = "0123456789abcdef";

pub fn lex_slice(allocator: std.mem.Allocator, source: []const u8) ![]const Token {
    var token_list: std.ArrayList(Token) = .init(allocator);
    defer token_list.deinit();

    var i: usize = 0;
    while (i < source.len) : (i += 1) {

        const c = source[i];
        const c2 = if (i+1 < source.len) source[i+1] else 0;


        // Check skipable
        if (c == ' ' or c == '\t') { continue; }

        // Check if it's a multiline character
        if (is_language_symbol(c) and is_language_symbol(c2)) {

            const cc: []const u8 = &.{ c, c2 };

            const tkind =
                 if (std.mem.eql(u8, cc, "=>")) TokenKind.right_arrow_operator
            else if (std.mem.eql(u8, cc, "==")) TokenKind.equal_operator
            else if (std.mem.eql(u8, cc, "!=")) TokenKind.not_equal_operator
            else if (std.mem.eql(u8, cc, "<=")) TokenKind.less_equal_operator
            else if (std.mem.eql(u8, cc, ">=")) TokenKind.greater_equal_operator
            else if (std.mem.eql(u8, cc, "**")) TokenKind.power_operator
            else if (std.mem.eql(u8, cc, "+=")) TokenKind.add_assign_operator
            else if (std.mem.eql(u8, cc, "-=")) TokenKind.sub_assign_operator
            else if (std.mem.eql(u8, cc, "*=")) TokenKind.mul_assign_operator
            else if (std.mem.eql(u8, cc, "/=")) TokenKind.div_assign_operator
            else if (std.mem.eql(u8, cc, "%=")) TokenKind.mod_assign_operator
            else TokenKind.undef;

            if (tkind != TokenKind.undef) {
                try token_list.append(.{ .kind = tkind });
                i += 1;
                continue;
            }

        }

        // Check if line feed
        if (c == '\n' or c == '\r') {
            try token_list.append(.{ .kind = .line_feed  });
            if (c == '\r' and c2 == '\n') i+=1;
            continue;
        }

        // Check if single character
        var match: bool = true;
        switch (c) {
            '(' => try token_list.append(.{ .kind = .left_parenthesis_char }),
            ')' => try token_list.append(.{ .kind = .right_parenthesis_char }),
            '{' => try token_list.append(.{ .kind = .left_brace_char }),
            '}' => try token_list.append(.{ .kind = .right_brace_char }),
            '[' => try token_list.append(.{ .kind = .left_square_bracket_char }),
            ']' => try token_list.append(.{ .kind = .right_square_bracket_char }),
            
            '<' => try token_list.append(.{ .kind = .left_angle_char }),
            '>' => try token_list.append(.{ .kind = .right_angle_char }),
            
            '+' => try token_list.append(.{ .kind = .plus_char }),
            '-' => try token_list.append(.{ .kind = .minus_char }),
            '*' => try token_list.append(.{ .kind = .star_char }),
            '/' => try token_list.append(.{ .kind = .slash_char }),
            '%' => try token_list.append(.{ .kind = .percent_char }),
            '=' => try token_list.append(.{ .kind = .equals_char }),

            '&' => try token_list.append(.{ .kind = .ampersand_char }),
            '?' => try token_list.append(.{ .kind = .question_char }),
            '!' => try token_list.append(.{ .kind = .bang_char }),
            '@' => try token_list.append(.{ .kind = .at_sign_char }),

            ',' => try token_list.append(.{ .kind = .comma_char }),
            '.' => try token_list.append(.{ .kind = .dot_char }),

            else => match = false
        }
        if (match) continue;

        // Build number
        if (std.ascii.isDigit(c)) {

            var j: usize = i;
            var base: usize = 10;
            var is_floating: bool = false;
            if (c == 0) {
                j += 1;
                switch (c2) {
                    'x' => base = 16,
                    'b' => base = 2,
                    'o' => base = 8,
                    else => j -= 1
                }
            }

            while (j+1 < source.len) : (j+=1) {
                const c3 = source[j+1];
                if (c3 == '.') is_floating = true;
                if (c3 != '_' and c3 != '.' and !is_digit(c3, base)) break;
            }

            const v = try allocator.dupe(u8, source[i..j+1]);
            errdefer allocator.free(v);

            try token_list.append(.{ 
                .kind = if (is_floating) .floating_number_literal else .integer_number_literal,
                .value = v,
            });

            i = j;
            continue;

        }

        // Check word
        if (is_valid_identifier_starter(c)) {

            var j = i;
            while (j+1 < source.len) : (j += 1) {
                if (!is_valid_identifier(source[j+1])) break;
            }

            const nameslice = try allocator.dupe(u8, source[i..j+1]);
            errdefer allocator.free(nameslice);

            const is_in_dic, const kind = b: {
                for (dictionary) |k| {
                    const str = k.@"0";
                    const tkn = k.@"1";

                    if (std.mem.eql(u8, str, nameslice)) break :b .{ true, tkn };
                }
                break :b .{ false, undefined };
            };

            if (is_in_dic) {
                try token_list.append(.{ .kind = kind });
            }
            else {
                try token_list.append(.{
                    .kind = .identifier,
                    .value = nameslice
                });
            }

            i = j;
            continue;

        }

        // Check string
        // TODO implement escaped characters and interpolation
        if (c == '"') {
            try token_list.append(.{ .kind = .double_quote_char });

            var j: usize = i;
            while (j+1 > source.len) : (j += 1) {
                if (source[j+1] == '"') {
                    try token_list.append(.{
                        .kind = .string_literal,
                        .value = source[i..j]
                    });
                    try token_list.append(.{ .kind = .double_quote_char });
                    break;
                }
            }

            i = j;
        }
        if (c == '\'') {

        }

        // Ignore comments
        if (c == '#') {
            if (source.len > i + 3 and std.mem.eql(u8, source[i..i+3], "###")) {
                i += 3;
                while (source.len > i+3 and !std.mem.eql(u8, source[i..i+3], "###")) : (i += 1) {}
                i += 3;
            } else {
                i += 1;
                while (source.len > i+1 and source[i] != '\n') : (i += 1) {}
            }
        }

    }

    return token_list.toOwnedSlice();
}
pub fn reduce_line_feeds(allocator: std.mem.Allocator, tokens: []const Token) ![]const Token {

    var tkn_list = std.ArrayList(Token).fromOwnedSlice(allocator, @constCast(tokens));

    for (tkn_list.items, 0..) |t, i| {
        if (t.kind == .line_feed) {
            while (tkn_list.items.len > i+1 and tkn_list.items[i+1].kind == .line_feed) {
                _ = tkn_list.orderedRemove(i+1);
            }
        }
    }

    return tkn_list.toOwnedSlice();

}
pub fn free_tokens(allocator: std.mem.Allocator, tokens: []const Token) void {
    for (tokens) |i| if (i.value) |v| allocator.free(v);
    allocator.free(tokens);
}

inline fn is_digit(char: u8, base: usize) bool {
    return std.mem.containsAtLeastScalar(u8, lang_numerics[0..base], 1, std.ascii.toLower(char));
}
inline fn is_language_symbol(char: u8) bool {
    return std.mem.containsAtLeastScalar(u8, lang_symbols, 1, char);
}
inline fn is_valid_identifier_starter(char: u8) bool {
    return char == '_' or std.ascii.isAlphabetic(char);
}
inline fn is_valid_identifier(char: u8) bool {
    return char == '_' or std.ascii.isAlphanumeric(char);
}

const expect = std.testing.expect;
const test_allocator = std.testing.allocator;
test "lexer general" {

    var arena = std.heap.ArenaAllocator.init(test_allocator);
    const allocator = arena.allocator();

    const sample = 
    \\@public const f64 PI = 3.1415
    \\@public const f32 E = 2.71828
    \\
    \\# This shouold not be tokenized!
    \\
    \\@public func sin(type T, T rad) T {
    \\      call("Hello, World!")
    \\}
    \\
    ;

    const tkns = try reduce_line_feeds(allocator, try lex_slice(allocator, sample));

    try expect(tkns[0].kind == .at_sign_char);
    try expect(tkns[1].kind == .identifier and std.mem.eql(u8, tkns[1].value.?, "public"));
    try expect(tkns[2].kind == .const_keyword);
    try expect(tkns[3].kind == .identifier and std.mem.eql(u8, tkns[3].value.?, "f64"));
    try expect(tkns[4].kind == .identifier and std.mem.eql(u8, tkns[4].value.?, "PI"));
    try expect(tkns[5].kind == .equals_char);
    try expect(tkns[6].kind == .floating_number_literal and std.mem.eql(u8, tkns[6].value.?, "3.1415"));
    try expect(tkns[7].kind == .line_feed);

    try expect(tkns[8].kind == .at_sign_char);
    try expect(tkns[9].kind == .identifier and std.mem.eql(u8, tkns[9].value.?, "public"));
    try expect(tkns[10].kind == .const_keyword);
    try expect(tkns[11].kind == .identifier and std.mem.eql(u8, tkns[11].value.?, "f32"));
    try expect(tkns[12].kind == .identifier and std.mem.eql(u8, tkns[12].value.?, "E"));
    try expect(tkns[13].kind == .equals_char);
    try expect(tkns[14].kind == .floating_number_literal and std.mem.eql(u8, tkns[14].value.?, "2.71828"));
    try expect(tkns[15].kind == .line_feed);

    try expect(tkns[16].kind == .at_sign_char);
    try expect(tkns[17].kind == .identifier and std.mem.eql(u8, tkns[17].value.?, "public"));
    try expect(tkns[18].kind == .func_keyword);
    try expect(tkns[19].kind == .identifier and std.mem.eql(u8, tkns[19].value.?, "sin"));
    try expect(tkns[20].kind == .left_parenthesis_char);
    try expect(tkns[21].kind == .identifier and std.mem.eql(u8, tkns[21].value.?, "type"));
    try expect(tkns[22].kind == .identifier and std.mem.eql(u8, tkns[22].value.?, "T"));
    try expect(tkns[23].kind == .comma_char);
    try expect(tkns[24].kind == .identifier and std.mem.eql(u8, tkns[24].value.?, "T"));
    try expect(tkns[25].kind == .identifier and std.mem.eql(u8, tkns[25].value.?, "rad"));
    try expect(tkns[26].kind == .right_parenthesis_char);
    try expect(tkns[27].kind == .identifier and std.mem.eql(u8, tkns[27].value.?, "T"));

    try expect(tkns[28].kind == .left_brace_char);
    try expect(tkns[29].kind == .line_feed);

    try expect(tkns[30].kind == .identifier and std.mem.eql(u8, tkns[30].value.?, "call"));
    try expect(tkns[31].kind == .left_parenthesis_char);
    try expect(tkns[32].kind == .double_quote_char);
    try expect(tkns[33].kind == .string_literal and std.mem.eql(u8, tkns[33].value.?, "Hello, World!"));
    try expect(tkns[34].kind == .double_quote_char);
    try expect(tkns[35].kind == .line_feed);
    try expect(tkns[36].kind == .right_parenthesis_char);

    try expect(tkns[37].kind == .right_brace_char);

    arena.deinit();

}
