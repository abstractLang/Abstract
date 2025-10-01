namespace Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression;

public class TypeCastNode : ExpressionNode
{
    public ExpressionNode Value => (ExpressionNode)_children[0];
    public ExpressionNode TargetType => (ExpressionNode)_children[2];
}