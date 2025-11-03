namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.CodeReferences;

public class MetaReference(MetaReference.MetaReferenceType t): LanguageReference
{
    public readonly MetaReferenceType Type = t;
    
    public enum MetaReferenceType
    {
        Name,
        
        ByteSize,
        BitSize,
        
        Alignment,
    }
}
