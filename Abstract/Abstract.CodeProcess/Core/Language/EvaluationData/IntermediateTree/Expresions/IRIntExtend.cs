using System.Text;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin.Integer;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Expresions;

public class IRIntExtend(SyntaxNode origin, IRExpression val, IntegerTypeReference totype) : IRExpression(origin)
{
    public IRExpression Value = val;
    public readonly IntegerTypeReference toType = totype;

    public override string ToString()
    {
        var sb = new StringBuilder();

        sb.AppendLine($"({totype}.extend");
        sb.Append(Value.ToString().TabAll());
        sb.Append(')');
        
        return sb.ToString();
    }
}
