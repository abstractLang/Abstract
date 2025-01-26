using System.Numerics;
using Abstract.Parser.Core.ProgMembers;

namespace Abstract.Parser.Core.ProgData.DataReference.ComptimeConstants;

public class StringConstRef(ExecutableContext ctx, int index) : ConstantRef<string>(ctx, index)
{
    public override string ToString() => $"const(\"{GetData()}\")";
    public override TypeReference refferToType
    => new SolvedTypeReference((Structure)ctx.TryGetMember("Std.Compilation.Types.ComptimeString")!);
}
public class IntegerConstRef(ExecutableContext ctx, int index) : ConstantRef<BigInteger>(ctx, index)
{
    public override string ToString() => $"const({GetData()}i)";
    public override TypeReference refferToType
    => new SolvedTypeReference((Structure)ctx.TryGetMember("Std.Compilation.Types.ComptimeInteger")!);
}
public class FloatConstRef(ExecutableContext ctx, int index) : ConstantRef<double>(ctx, index)
{
    public override string ToString() => $"const({GetData():0.0################})";
    public override TypeReference refferToType
    => new SolvedTypeReference((Structure)ctx.TryGetMember("Std.Compilation.Types.ComptimeFloating")!);
}
public class BooleanConstRef(ExecutableContext ctx, int index) : ConstantRef<bool>(ctx, index)
{
    public override string ToString() => $"const({GetData()})";
    public override TypeReference refferToType
    => new SolvedTypeReference((Structure)ctx.TryGetMember("Std.Compilation.Types.ComptimeBoolean")!);
}
