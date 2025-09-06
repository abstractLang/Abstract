using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects.CodeObjects;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.CodeReferences;

public class ParameterReference(ParameterObject param) : LanguageReference
{
    public readonly ParameterObject Parameter = param;
    
    public override string ToString() => $"arg.{Parameter.index:D2}";
}
