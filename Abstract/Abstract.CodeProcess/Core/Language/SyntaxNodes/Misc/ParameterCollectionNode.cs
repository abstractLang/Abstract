using Abstract.Parser.Core.Language.SyntaxNodes.Base;
using Abstract.Parser.Core.Language.SyntaxNodes.Expression;

namespace Abstract.Parser.Core.Language.SyntaxNodes.Misc;

public class ParameterCollectionNode : SyntaxNode
{
    public TypedIdentifierNode[] Items => [.. _children[1..^1].Select(e => (TypedIdentifierNode)e)];
    public override string ToString() => $"({string.Join(", ", _children[1..^1])})";
}
