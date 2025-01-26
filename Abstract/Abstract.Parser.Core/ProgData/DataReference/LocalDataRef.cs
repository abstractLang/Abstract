using Abstract.Parser.Core.ProgMembers;

namespace Abstract.Parser.Core.ProgData.DataReference;

public class LocalDataRef(ExecutableContext ctx, MemberIdentifier name) : DataRef
{

    private ExecutableContext context = ctx;
    private MemberIdentifier name = name;

    public override TypeReference refferToType => context.GetLocalReference(name).type;
    public int GetIndex => context.GetLocalReference(name).idx;

    public override string ToString() {
        var (idx, type, isconst) = context.GetLocalReference(name);
        return $"l{idx}(" + (isconst ? "const" : "let" ) + $" {name} -> {type})";
    }
}
