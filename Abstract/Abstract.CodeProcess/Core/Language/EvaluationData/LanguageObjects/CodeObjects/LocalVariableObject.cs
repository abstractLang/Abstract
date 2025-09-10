using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects.CodeObjects;

public class LocalVariableObject(TypeReference typeref, string name)//: LangObject(null!)
{
    public TypeReference Type { get; set; } = typeref;
    public readonly string Name = name;

    public int index = 0;

    public override string ToString() => $"$({index:D2}) Local '{Name}': {Type?.ToString() ?? "<!nil>"}";
}
