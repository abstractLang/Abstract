using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression;

namespace Abstract.CodeProcess.Core.Language.SyntaxNodes.Misc;

public class ArgumentCollectionNode : SyntaxNode
{
    public ExpressionNode[] Arguments => [..  _children[1..^1].Select(e => (ExpressionNode)e)];

    public override string ToString() => _children.Count > 2
        ? base.ToString()
        : $"({string.Join(", ", _children[1..^1])})";
}
