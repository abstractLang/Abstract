namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin;

public class IntegerTypeReference : BuiltInTypeReference
{
    public readonly bool Signed;
    public readonly bool PtrSized;
    public readonly byte BitSize = 0;

    public IntegerTypeReference(bool signed, byte size)
    {
        Signed = signed;
        PtrSized = false;
        BitSize = size;
    }
    public IntegerTypeReference(bool signed)
    {
        Signed = signed;
        PtrSized = true;
    }

    public override string ToString() => (Signed ? 'i' : 'u') + (PtrSized ? "ptr" : $"{BitSize}");
}
