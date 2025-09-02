namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences;

public class IntegerTypeReference(bool signess, byte size) : BuiltInTypeReference
{
    public readonly bool Signed = signess;
    public readonly byte BitSize = size;

    public override string ToString() => (Signed ? 's' : 'u') + $"{BitSize}";
}