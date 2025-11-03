using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Expresions;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Values;

public class IRAccess(SyntaxNode origin, IRExpression a, IRExpression b) : IRReference(origin, b.Type)
{
    public IRExpression A = a;
    public IRExpression B = b;

    public override string ToString() => $"{A}->{B}";
}
