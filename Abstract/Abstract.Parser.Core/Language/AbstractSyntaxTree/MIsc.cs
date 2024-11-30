namespace Abstract.Parser.Core.Language.AbstractSyntaxTree;

public class ParameterCollectionNode : ControlScopeNode<ParameterNode>
{
    public override string ToString() => $"({string.Join(", ", children)})";
}
public class ArgumentCollectionNode : ControlScopeNode<ExpressionNode>
{
    public override string ToString() => $"({string.Join(", ", children)})";
}

public class ParameterNode : SyntaxNode, IReferenceable
{
    public new IReferenceable? Parent { get; set; }
    public MasterSymbol Symbol { get; set; } = null!;
    public TypeNode ReturnType { get; set; } = null!;

    public ReferenceSymbol GetGlobalSymbol() => new((string[])[.. Parent?.GetGlobalSymbol().Tokens ?? [], .. Symbol.Tokens], Symbol);

    public override string ToString() => $"{ReturnType} {Symbol}";
}
