using Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Expresions;

public class IRUnaryExp(UnaryExpressionNode origin,
IRUnaryExp.UnaryPrefix pre, IRExpression value) : IRExpression(origin)
{
    public UnaryPrefix Prefix = pre;
    public IRExpression Value = value;
    
    public enum UnaryPrefix
    {
        Plus,
        Minus,
        Not,
    }
}