namespace Abstract.Core.Language;

public enum NodeKind : byte
{
    Undefined = 0,
    Token,
    Root,
    Identifier,
    Type,

    // control shit
    FromImport,
    FunctionDeclaration,
    StructureDeclaration,
    EnumDeclaration,
    Attribute,
    Variable,

    // scopes
    Scope,
    ImplicitScope,

    // expressions
    FunctionCall,
    ConstructorCall,
    DestructorCall,

    ArgumentsList,
    ParametersList,
    TypedIdentifier,

    BinaryExpression,
    UnaryExpression,

    StringLiteral,
    IntegerLiteral,
}
