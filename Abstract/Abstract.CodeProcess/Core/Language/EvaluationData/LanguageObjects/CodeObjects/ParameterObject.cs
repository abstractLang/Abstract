using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects.CodeObjects;

public class ParameterObject(TypeReference type, string name) : LangObject(null!, null!)
{
    public readonly string Name = name;
    public TypeReference Type { get; set; } = type;

    public int index = 0;
    
    public override string ToString() => $"$({index:D2}) Parameter '{Name}': {Type?.ToString() ?? "<!nil>"}";
}
