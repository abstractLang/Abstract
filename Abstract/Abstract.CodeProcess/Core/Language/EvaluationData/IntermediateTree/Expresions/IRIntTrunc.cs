using System.Text;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin.Integer;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Expresions;

public class IRIntTrunc(SyntaxNode origin, IRExpression val, IntegerTypeReference totype) : IRExpression(origin, totype)
{
    public IRExpression Value = val;

    public override string ToString()
    {
        var sb = new StringBuilder();

        sb.AppendLine($"({Type}.trunc");
        sb.Append(Value.ToString().TabAll());
        sb.Append(')');
        
        return sb.ToString();
    }
}
