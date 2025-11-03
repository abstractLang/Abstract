namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin;

public class NullableTypeReference(TypeReference internaltype) : BuiltInTypeReference
{
    public TypeReference InternalType { get; set; } = internaltype;
    
    public override string ToString() => $"?{InternalType}";
}