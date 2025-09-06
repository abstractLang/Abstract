using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences;

public class UnsolvedTypeReference(SyntaxNode node) : TypeReference
{
    public readonly SyntaxNode syntaxNode = node;

    public override string ToString() => $"UType<{syntaxNode}>";
}
