using System.Text;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Expresions;

public class IRSignCast(SyntaxNode origin, IRExpression val, bool signed) : IRExpression(origin)
{
    public IRExpression Value = val;
    public readonly bool Signed = signed;

    public override string ToString()
    {
        var sb = new StringBuilder();

        sb.AppendLine(Signed ? "(tosigned" : "(tounsigned");
        sb.Append(Value.ToString().TabAll());
        sb.Append(')');
        
        return sb.ToString();
    }
}