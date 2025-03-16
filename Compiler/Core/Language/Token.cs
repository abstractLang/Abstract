using static Abstract.Core.Language.Token;

namespace Abstract.Core.Language;

/// <summary>
/// A token sliced from the source code.
/// </summary>
public class Token(SourceRange trivia, SourceRange value, Kind kind)
{
    /// <summary>
    /// Pseudo-tokens that prefix the token and do not
    /// represent important information.
    /// e.g.: spaces, tabs or comments.
    /// </summary>
    public readonly SourceRange trivia = trivia;
    /// <summary>
    /// The actual token value.
    /// </summary>
    public readonly SourceRange value = value;

    public Token before = null!;
    public Token after = null!;

    /// <summary>
    /// The token kind.
    /// </summary>
    public readonly Kind kind = kind;

    public override string ToString() => $"{trivia}{value}";


    /// <summary>
    /// The list of general token kinds.
    /// </summary>
    public enum Kind : byte
    {
        undefined = 0,
        statement_end, // line feed or semicolon

        keyword_from,
        keyword_import,

        keyword_let,
        keyword_const,
        keyword_func,
        keyword_struct,
        keyword_enum,

        keyword_conditional_if,
        keyword_conditional_elif,
        keyword_conditional_else,

        keyword_control_while,
        keyword_control_for,
        keyword_control_do,
        keyword_control_break,

        keyword_operator_and,
        keyword_operator_or,

        keyword_cast_as,

        keyword_value_true,
        keyword_value_false,
        keyword_value_null,

        keyword_new,
        keyword_destroy,

        char_dot,
        char_comma,
        char_bang,
        char_interrogation,
        char_at,
        char_anpersant,

        char_cross,
        char_dash,
        char_slash,
        char_star,
        char_percent,
        char_equals,

        opetator_equality,

        char_open_parenthesis,   char_close_parenthesis,
        char_open_curlyBracket,  char_close_curlyBracket,
        char_open_squareBracket, char_close_squareBracket,


        identifier,

        literal_string,
        literal_char,
        literal_integer,
        literal_float,

    }
}
