using Abstract.Parser.Core.Language.SyntaxNodes.Base;
using Abstract.Parser.Core.Language.SyntaxNodes.Expression;

namespace Abstract.Parser.Core.Language.SyntaxNodes.Misc;

public class ArgumentCollectionNode : SyntaxNode
{
    public ExpressionNode[] Arguments => [..  _children[1..^1].Select(e => (ExpressionNode)e)];

    public override string ToString() => $"({string.Join(", ", _children[1..^1])})";
}
