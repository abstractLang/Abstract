using System.Numerics;
using Abstract.Parser.Core.ProgMembers;

namespace Abstract.Parser.Core.ProgData.DataReference.ComptimeConstants;

public class StringConstRef(ExecutableCodeBlock block, int index) : ConstantRef<string>(block, index)
{
    public override string ToString() => $"const(\"{GetData()}\")";
    public override TypeReference refferToType
    => new SolvedTypeReference((Structure)block.TryGetMember("Std.Compilation.Types.ComptimeString")!);
}
public class IntegerConstRef(ExecutableCodeBlock block, int index) : ConstantRef<BigInteger>(block, index)
{
    public override string ToString() => $"const({GetData()}i)";
    public override TypeReference refferToType
    => new SolvedTypeReference((Structure)block.TryGetMember("Std.Compilation.Types.ComptimeInteger")!);
}
public class FloatConstRef(ExecutableCodeBlock block, int index) : ConstantRef<double>(block, index)
{
    public override string ToString() => $"const({GetData():0.0################})";
    public override TypeReference refferToType
    => new SolvedTypeReference((Structure)block.TryGetMember("Std.Compilation.Types.ComptimeFloating")!);
}
public class BooleanConstRef(ExecutableCodeBlock block, int index) : ConstantRef<bool>(block, index)
{
    public override string ToString() => $"const({GetData()})";
    public override TypeReference refferToType
    => new SolvedTypeReference((Structure)block.TryGetMember("Std.Compilation.Types.ComptimeBoolean")!);
}
