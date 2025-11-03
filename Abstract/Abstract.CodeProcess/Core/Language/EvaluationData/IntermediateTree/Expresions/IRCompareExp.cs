using System.Text;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Expresions;

public class IRCompareExp(
    BinaryExpressionNode origin,
    IRCompareExp.Operators ope,
    IRExpression left,
    IRExpression right) : IRExpression(origin, new BooleanTypeReference())
{

    public Operators Operator { get; set; } = ope;
    public IRExpression Left { get; set; } = left;
    public IRExpression Right { get; set; } = right;
    
    public override string ToString()
    {
        var sb = new StringBuilder();

        sb.AppendLine($"({Operator.ToString().ToLower()}");
        sb.AppendLine(Left.ToString().TabAll());
        sb.Append(Right.ToString().TabAll());
        sb.Append(')');
        
        return sb.ToString();
    }
    
    public enum Operators
    {
        GreaterThan,
        LessThan,
        GreaterThanOrEqual,
        LessThanOrEqual,
    }
}
