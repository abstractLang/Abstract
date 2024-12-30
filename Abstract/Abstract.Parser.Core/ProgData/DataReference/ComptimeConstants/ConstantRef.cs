namespace Abstract.Parser.Core.ProgData.DataReference.ComptimeConstants;

public class ConstantRef<T>(ExecutableCodeBlock block, int index) : DataRef
{
    protected ExecutableCodeBlock block = block;
    protected int index = index;

    public T GetData() => (T)block.GetConstantReference(index);

    public override TypeReference refferToType => throw new NotImplementedException();
}
