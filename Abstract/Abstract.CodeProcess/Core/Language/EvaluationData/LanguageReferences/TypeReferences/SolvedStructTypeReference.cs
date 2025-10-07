using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences;

public class SolvedStructTypeReference(StructObject struc) : TypeReference
{
    public readonly StructObject Struct = struc;

    public override string ToString() => $"Struct<{string.Join('.', Struct.Global)}>";

    public int CalculateSuitability(SolvedStructTypeReference to)
    {
        if (Struct == to.Struct) return 3;
        // TODO check casting possibility
        return 0;
    }
}
