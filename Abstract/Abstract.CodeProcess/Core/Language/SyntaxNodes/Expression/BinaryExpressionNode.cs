using Abstract.CodeProcess.Core.Language.SyntaxNodes.Value;

namespace Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression;

public class BinaryExpressionNode : ExpressionNode
{
    public ExpressionNode Left => (ExpressionNode)_children[0];
    public string Operator => ((TokenNode)_children[1]).Value;
    public ExpressionNode Right => (ExpressionNode)_children[2];
}
