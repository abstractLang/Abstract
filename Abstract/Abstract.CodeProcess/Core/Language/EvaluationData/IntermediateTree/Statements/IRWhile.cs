using System.Text;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Expresions;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Statements;

public class IRWhile(SyntaxNode origin, IRBlock?  d, IRExpression? c, IRBlock? s, IRBlock p) : IRStatement(origin)
{
    public IRBlock? Define = d;
    public IRExpression Condition = c;
    public IRBlock? Step = s;
    public IRBlock Process = p;

    public override string ToString()
    {
        var sb = new StringBuilder();
        
        sb.AppendLine($"(while {Condition.ToString().TabAll()[1..]})");
        
        if (Define != null) sb.Append($"(def\n{Define.ToString().TabAll()})");
        if (Step != null) sb.Append($"(step\n{Step.ToString().TabAll()})");
        
        sb.Append($"{Process.ToString().TabAll()})");
        
        return sb.ToString();
    }
}
