namespace Abstract.Parser.Core.Language.AbstractSyntaxTree;

/// <summary>
/// A referenceable data slot.
/// This, specifically, should be included only abouve controll nodes
/// (Structs, Functions, Namespaces)
/// </summary>
/// <param name="constant"> If the data slot is constant (true) or variable (false) </param>
public class TopLevelVariableDeclarationNode(bool constant) : StatementNode, IReferenceable
{
    public new IReferenceable? Parent { get => base.Parent as IReferenceable; set => base.Parent = (SyntaxNode)value!; }

    public readonly bool constant = constant;
    public MasterSymbol Symbol { get; set; } = null!;
    public TypeNode ReturnType { get; set; } = null!;

    public ExpressionNode? Value { get; set; } = null;

    public ReferenceSymbol GetGlobalSymbol() => new((string[])[.. Parent?.GetGlobalSymbol().Tokens ?? [], .. Symbol.Tokens], Symbol);
    public override string ToString() => (!constant ? "let " : "const ") + $"{ReturnType} {Symbol}"
        + (Value != null ? $" = {Value}" : "");
}
