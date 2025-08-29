using Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.AttributeReferences;

public class BuiltInAttributeReference(AttributeNode node, BuiltinAttributes bia) : AttributeReference(node)
{
    public readonly BuiltinAttributes Attribute = bia;
}
