using System.Text;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Expresions;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Statements;

public class IRWhile(SyntaxNode origin, IRExpression c, IRBlock? s, IRBlock p) : IRStatement(origin)
{
    public IRExpression Condition = c;
    public IRBlock? Step = s;
    public IRBlock Process = p;

    public override string ToString()
    {
        var sb = new StringBuilder();
        
        sb.AppendLine($"(while {Condition})");
        if (Step != null)
            sb.Append($"(step\t{Step.ToString().TabAll()})");
        sb.Append($"{Process.ToString().TabAll()})");
        
        return sb.ToString();
    }
}
