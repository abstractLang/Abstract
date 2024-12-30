namespace Abstract.Parser.Core.Language.SyntaxNodes.Expression;

public class IdentifierCollectionNode(bool incomplete = false) : ExpressionNode
{
    public readonly bool incomplete = incomplete;

    public override string ToString()
        => (incomplete ? "." : "") + $"{string.Join('.', _children)}";
}
