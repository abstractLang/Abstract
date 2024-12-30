using Abstract.Parser.Core.ProgMembers;

namespace Abstract.Parser.Core.ProgData.DataReference;

public class LocalDataRef(ExecutableCodeBlock block, MemberIdentifier name) : DataRef
{

    private ExecutableCodeBlock block = block;
    private MemberIdentifier name = name;

    public override TypeReference refferToType => block.GetLocalReference(name).type;

    public override string ToString() {
        var (type, isconst) = block.GetLocalReference(name);
        return $"l(" + (isconst ? "const" : "let" ) + $" {name} -> {type})";
    }
}
