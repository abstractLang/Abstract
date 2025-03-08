namespace Abstract.Core.Language;

public enum NodeKind : byte
{
    Undefined = 0,
    Token,
    Root,
    Identifier,

    FromImport,
    FunctionDeclaration,

    Scope,
    FunctionCall,

    ArgumentsList,
    ParametersList,
    TypedIdentifier,

    BinaryExpression,
    UnaryExpression,

    StringLiteral,
}
