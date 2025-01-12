using Abstract.Parser.Core.ProgMembers;

namespace Abstract.Parser.Core.ProgData.DataReference;

public class LocalDataRef(ExecutableCodeBlock block, MemberIdentifier name) : DataRef
{

    private ExecutableCodeBlock block = block;
    private MemberIdentifier name = name;

    public override TypeReference refferToType => block.GetLocalReference(name).type;
    public int GetIndex => block.GetLocalReference(name).idx;

    public override string ToString() {
        var (idx, type, isconst) = block.GetLocalReference(name);
        return $"l{idx}(" + (isconst ? "const" : "let" ) + $" {name} -> {type})";
    }
}
