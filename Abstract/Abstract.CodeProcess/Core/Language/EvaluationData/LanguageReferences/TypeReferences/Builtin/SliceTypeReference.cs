namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin;

public class SliceTypeReference(TypeReference internaltype) : TypeReference
{
    public TypeReference InternalType { get; set; } = internaltype;
    
    public override string ToString() => $"[]{InternalType}";
}
