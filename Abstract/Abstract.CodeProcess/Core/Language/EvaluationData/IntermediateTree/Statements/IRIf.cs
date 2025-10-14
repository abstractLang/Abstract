using System.Text;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Expresions;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Statements;

public class IRIf(SyntaxNode origin, IRExpression exp, IRBlock then) : IRStatement(origin), IIfElse
{
    public IRExpression Condition = exp;
    public IRBlock Then = then;
    public IIfElse? Else = null;

    public override string ToString()
    {
        var sb = new StringBuilder();

        sb.Append("(if ");
        sb.AppendLine(Condition.ToString());
        sb.Append(Then.ToString().TabAll());

        if (Else != null)
        {
            sb.AppendLine("(else");
            sb.Append(Else.ToString().TabAll());
            sb.Append(')');
        }

        sb.Append(')');
            
        return sb.ToString();
    }
}
