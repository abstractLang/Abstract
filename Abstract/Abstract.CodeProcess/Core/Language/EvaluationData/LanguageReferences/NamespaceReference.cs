using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences;

public class NamespaceReference(NamespaceObject nmsp) : LanguageReference
{
    public readonly NamespaceObject NamespaceObject = nmsp;
}