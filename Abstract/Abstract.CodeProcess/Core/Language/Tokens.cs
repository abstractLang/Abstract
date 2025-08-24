namespace Abstract.CodeProcess.Core.Language;

public struct Token
{
    public string value;
    public TokenType type;

    public uint start;
    public uint end;

    public readonly (uint start, uint end) Range => (start, end);
    public readonly uint RangeLength => end - start;

    public override readonly string ToString() => $"{value} ({type})";
    public readonly string ValueString()
        => type switch
        {
            TokenType.LineFeedChar => "\n",
            TokenType.EofChar => "[\\EOF]",
            _ => value,
        };
}
public enum TokenType : byte
{
    Undefined,              // undefined token (default value)

    IntegerNumberLiteral,
    FloatingNumberLiteral,
    StringLiteral,
    CharacterLiteral,
    Identifier,
    
    FromKeyword,            // from
    ImportKeyword,          // import
    TypeKeyword,
    LetKeyword,             // let
    ConstKeyword,           // const
    FuncKeyword,            // func
    StructKeyword,          // struct
    ExtendsKeyword,         // extends
    PacketKeyword,          // packet
    TypedefKeyword,         // typedef

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

    AddAssigin,             // +=
    SubAssigin,             // -=
    MulAssigin,             // *=
    DivAssigin,             // /=
    RestAssigin,            // %=

    IncrementOperator,      // ++
    DecrementOperator,      // --

    RangeOperator,          // ..

    SingleQuotes,           // '
    DoubleQuotes,           // "

    CommaChar,              // ,
    DotChar,                // .

    EofChar,                // \EOF
    LineFeedChar,           // \n
    EspaceChar,             //  
}
