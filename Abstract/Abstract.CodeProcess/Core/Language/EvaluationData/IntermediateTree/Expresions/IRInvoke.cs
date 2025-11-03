using System.Text;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Values;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.FunctionReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Expresions;

public class IRInvoke(
    FunctionCallExpressionNode origin,
    IRExpression target,
    IRExpression[] args) : IRExpression(origin, ((FunctionTypeReference?)target.Type)?.Returns ?? null!)
{

    public IRExpression Target { get; set; } = target;
    public IRExpression[] Arguments { get; set; } = args;
    
    public override string ToString()
    {
        var sb = new StringBuilder();

        sb.AppendLine("(invoke");
        sb.AppendLine(Target.ToString().TabAll());
        foreach (var arg in Arguments)
        {
            sb.AppendLine(arg.ToString().TabAll());
        }
        sb.Length -= Environment.NewLine.Length;
        sb.Append(')');
        
        return sb.ToString();
    }
}
