using Abstract.Parser.Core.Sources;

namespace Abstract.Parser.Core.Language;

public struct Token
{
    public string value;
    public TokenType type;

    public int start;
    public int end;

    public Script scriptRef;

    public override readonly string ToString() => $"{value} ({type});";
    public readonly string ValueString()
        => type switch
        {
            TokenType.LineFeedChar => "\n",
            TokenType.EOFChar => "[\\EOF]",
            _ => value,
        };
}
public enum TokenType : byte
{
    Undefined,              // undefined token (default value)

    IntegerNumberLiteral,
    FloatingNumberLiteral,
    StringLiteral,
    Identifier,

    NamespaceKeyword,       // namespace
    ImportKeyword,          // import
    TypeKeyword,
    LetKeyword,             // let
    ConstKeyword,           // const
    FuncKeyword,            // func
    StructKeyword,          // struct
    ExtendsKeyword,         // extends

    IfKeyword,              // if
    ElifKeyword,            // elif
    ElseKeyword,            // else
    WhileKeyword,           // while
    ForKeyword,             // for
    DoKeyword,              // do
    InKeyword,              // in
    BreakKeyword,           // break

    AsKeyword,              // as

    ReturnKeyword,          // return

    NullKeyword,            // null
    TrueKeyword,            // true
    FalseKeyword,           // false

    LeftPerenthesisChar,    // (
    RightParenthesisChar,   // )

    LeftBracketChar,        // {
    RightBracketChar,       // }

    LeftSquareBracketChar,  // [
    RightSquareBracketChar, // ]

    LeftAngleChar,          // <
    RightAngleChar,         // >

    CrossChar,              // +
    MinusChar,              // -
    StarChar,               // *
    SlashChar,              // /
    PercentChar,            // %
    EqualsChar,             // =
    AmpersandChar,          // &

    AtSiginChar,            // @

    RightArrowOperator,     // =>

    EqualOperator,          // ==
    UnEqualOperator,        // !=
    LessEqualsOperator,     // <=
    GreatEqualsOperator,    // >=

    PowerOperator,          // **

    AddAssigin,             // +=
    SubAssigin,             // -=
    MulAssigin,             // *=
    DivAssigin,             // /=
    RestAssigin,            // %=

    IncrementOperator,      // ++
    DecrementOperator,      // --

    RangeOperator,          // ..

    CommaChar,              // ,
    DotChar,                // .

    EOFChar,                // \EOF
    LineFeedChar,           // \n
}