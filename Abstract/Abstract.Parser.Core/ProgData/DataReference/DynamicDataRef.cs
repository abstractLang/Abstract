using Abstract.Parser.Core.ProgMembers;

namespace Abstract.Parser.Core.ProgData.DataReference;

public class DynamicDataRef(TypeReference type) : DataRef
{

    private TypeReference type = type;

    public override TypeReference refferToType => type;

    public override string ToString() => $"d({type})";
}
