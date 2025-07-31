namespace Abstract.Compiler.Parser.Core;

public enum TokenKind : byte
{
    Undefined,              // undefined token (default value)

    IntegerNumberLiteral,
    FloatingNumberLiteral,
    StringLiteral,
    CharacterLiteral,
    Identifier,

    NamespaceKeyword,       // namespace
    ImportKeyword,          // import
    FromKeyword,            // from
    TypeKeyword,
    LetKeyword,             // let
    ConstKeyword,           // const
    FuncKeyword,            // func
    StructKeyword,          // struct
    ExtendsKeyword,         // extends
    PacketKeyword,          // packet
    EnumKeyword,            // enum

    SwitchKeyword,          // switch
    MatchKeyword,           // match

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

    EscapedLeftBracketChar, // \{

    CrossChar,              // +
    MinusChar,              // -
    StarChar,               // *
    SlashChar,              // /
    PercentChar,            // %
    EqualsChar,             // =
    AmpersandChar,          // &
    QuestionChar,           // ?
    BangChar,               // !

    AtSiginChar,            // @

    RightArrowOperator,     // =>

    EqualOperator,          // ==
    UnEqualOperator,        // !=
    LessEqualsOperator,     // <=
    GreatEqualsOperator,    // >=

    AndOperator,            // and
    OrOperator,             // or

    PowerOperator,          // **

    AddAssign,              // +=
    SubAssign,              // -=
    MulAssign,              // *=
    DivAssign,              // /=
    RestAssign,             // %=

    IncrementOperator,      // ++
    DecrementOperator,      // --

    RangeOperator,          // ..

    SingleQuotes,           // '
    DoubleQuotes,           // "

    CommaChar,              // ,
    DotChar,                // .
    SpaceChar,              // \s

    EofChar,                // \EOF
    LineFeedChar,           // \n
}