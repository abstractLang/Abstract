namespace Abstract.Core.Language;

public enum NodeKind : byte
{
    Undefined = 0,
    Token,
    Root,
    Identifier,
    Type,

    FromImport,
    FunctionDeclaration,
    Attribute,
    Variable,

    Scope,
    FunctionCall,

    ArgumentsList,
    ParametersList,
    TypedIdentifier,

    BinaryExpression,
    UnaryExpression,

    StringLiteral,
    IntegerLiteral,
}
