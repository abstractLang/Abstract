using Abstract.Parser.Core.ProgMembers;

namespace Abstract.Parser.Core.ProgData.DataReference;

public class FunctionGroupRef(FunctionGroup group) : DataRef
{

    public readonly FunctionGroup group = group;

    public override TypeReference refferToType => null!;

    public override string ToString() => $"f({group})";
}
