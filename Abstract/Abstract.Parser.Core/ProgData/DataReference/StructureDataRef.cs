using Abstract.Parser.Core.ProgMembers;

namespace Abstract.Parser.Core.ProgData.DataReference;

public class StructureDataRef(Structure struc, Structure typeStructRef) : DataRef
{
    public readonly Structure structure = struc;

    private readonly Structure typeStructureReference = typeStructRef;
    public override TypeReference refferToType => new SolvedTypeReference(typeStructureReference);

    public override string ToString() => $"s({structure})";
}
