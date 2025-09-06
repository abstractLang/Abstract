using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.FunctionReferences;

public class UnsolvedFunctionReference(SyntaxNode node): FunctionReference
{
    public readonly SyntaxNode Expression = node;

    public override string ToString() => $"UFun({Expression})";
}
