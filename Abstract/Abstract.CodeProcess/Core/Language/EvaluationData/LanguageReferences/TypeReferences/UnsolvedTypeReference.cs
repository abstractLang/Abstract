using Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences;

public class UnsolvedTypeReference(ExpressionNode node) : TypeReference
{
    public readonly ExpressionNode syntaxNode = node;

    public override string ToString() => $"UType<{syntaxNode}>";
}
