using Abstract.Parser.Core.ProgMembers;

namespace Abstract.Parser.Core.ProgData.DataReference;

public class TypeDataRef(TypeReference type, Structure typeStructRef) : DataRef
{
    public readonly TypeReference type = type;

    private readonly Structure typeStructureReference = typeStructRef;
    public override TypeReference refferToType => new SolvedTypeReference(typeStructureReference);

    public override string ToString() => $"t({type})";
}
