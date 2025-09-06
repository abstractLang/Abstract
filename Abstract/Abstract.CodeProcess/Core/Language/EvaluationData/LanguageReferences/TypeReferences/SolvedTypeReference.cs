using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences;

public class SolvedStructTypeReference(StructObject struc) : TypeReference
{
    public readonly StructObject Struct = struc;

    public override string ToString() => $"Type<{Struct.Global}>";
}