using System.Text;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Statements;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Expresions;

public class IRAssign(
    AssignmentExpressionNode origin, 
    IRExpression targ,
    IRExpression val
    ) : IRExpression(origin, targ.Type)
{
    public IRExpression Target { get; set; } = targ;
    public IRExpression Value { get; set; } = val;
    
    public override string ToString()
    {
        var sb = new StringBuilder();

        sb.AppendLine($"(Assign");
        sb.AppendLine(Target.ToString().TabAll());
        sb.Append(Value.ToString().TabAll());
        sb.Append(')');
        
        return sb.ToString();
    }
}
