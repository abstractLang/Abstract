using Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.AttributeReferences;

public abstract class AttributeReference(AttributeNode node) : LanguageReference
{
    public readonly AttributeNode syntaxNode = node;

}
