using System.Text;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Expresions;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Statements;

public class IRReturn(SyntaxNode origin, IRExpression? val) : IRStatement(origin)
{
    public IRExpression? Value { get; set; } = val;
    
    public override string ToString()
    {
        if (Value == null) return "(ret)";
        var sb = new StringBuilder();

        sb.AppendLine($"(ret");
        sb.AppendLine(Value.ToString().TabAll());
        sb.Append(')');
        
        return sb.ToString();
    }
}
