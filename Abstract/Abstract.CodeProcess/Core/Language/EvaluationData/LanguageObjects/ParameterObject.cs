using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;

public class ParameterObject(TypeReference type, string name) : LangObject(null!)
{
    public readonly string Name = name;
    public TypeReference Type { get; set; } = type;

    public override string ToString() => $"Parameter '{Name}': {Type}";
}
