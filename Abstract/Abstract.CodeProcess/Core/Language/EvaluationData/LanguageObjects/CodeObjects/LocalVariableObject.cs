using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects.CodeObjects;

public class LocalVariableObject(string name)//: LangObject(null!)
{
    public TypeReference Type { get; set; }
    public readonly string Name = name;

    public override string ToString() => $"Local '{Name}': {Type}";
}