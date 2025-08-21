using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression;

namespace Abstract.CodeProcess.Core.Language.SyntaxNodes.Misc;

public class ParameterCollectionNode : SyntaxNode
{
    public TypedIdentifierNode[] Items => [.. _children[1..^1].Select(e => (TypedIdentifierNode)e)];
    public override string ToString() => $"({string.Join(", ", _children[1..^1])})";
}
