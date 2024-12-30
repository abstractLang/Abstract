using Abstract.Parser.Core.Language.SyntaxNodes.Value;

namespace Abstract.Parser.Core.Language.SyntaxNodes.Expression;

public class UnaryExpressionNode : ExpressionNode
{
    public override string ToString() => $"{_children[0]}{_children[1]}";
    public string Operator => ((TokenNode)_children[0]).Value;
    public ExpressionNode Expression => (ExpressionNode)_children[1];
}
