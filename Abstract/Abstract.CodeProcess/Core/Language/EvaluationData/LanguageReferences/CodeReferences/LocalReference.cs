using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects.CodeObjects;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.CodeReferences;

public class LocalReference(LocalVariableObject local) : LanguageReference
{
    public readonly LocalVariableObject Local = local;

    public override string ToString() => $"local.{Local.index:D2}";
}
