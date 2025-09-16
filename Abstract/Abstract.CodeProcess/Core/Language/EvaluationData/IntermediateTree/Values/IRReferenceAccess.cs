using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Expresions;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Values;

public class IRReferenceAccess(SyntaxNode origin, LanguageReference a, IRExpression b) : IRReference(origin)
{
    public readonly LanguageReference A = a;
    public readonly IRExpression B = b;

    public override string ToString() => $"{A}->{B}";
}
