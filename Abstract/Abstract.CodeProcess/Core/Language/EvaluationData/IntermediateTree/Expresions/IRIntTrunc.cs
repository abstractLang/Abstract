using System.Text;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Expresions;

public class IRIntTrunc(SyntaxNode origin, IRExpression val, int size) : IRExpression(origin)
{
    public IRExpression Value = val;
    public readonly int Size = size;

    public override string ToString()
    {
        var sb = new StringBuilder();

        sb.AppendLine($"(trunc.to.{Size}");
        sb.Append(Value.ToString().TabAll());
        sb.Append(')');
        
        return sb.ToString();
    }
}
