using System.Numerics;

namespace Abstract.Parser.Core.ProgData.DataReference.ComptimeConstants;

public class StringConstRef(ExecutableCodeBlock block, int index) : ConstantRef<string>(block, index)
{
    public override string ToString() => $"const(\"{GetData()}\")";
}
public class IntegerConstRef(ExecutableCodeBlock block, int index) : ConstantRef<BigInteger>(block, index)
{
    public override string ToString() => $"const({GetData()}i)";
}
public class FloatConstRef(ExecutableCodeBlock block, int index) : ConstantRef<double>(block, index)
{
    public override string ToString() => $"const({GetData():0.0################})";
}
public class BooleanConstRef(ExecutableCodeBlock block, int index) : ConstantRef<bool>(block, index)
{
    public override string ToString() => $"const({GetData()})";
}
