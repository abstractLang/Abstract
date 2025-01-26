namespace Abstract.Parser.Core.ProgData.DataReference.ComptimeConstants;

public abstract class ConstantRef<T>(ExecutableContext ctx, int index) : DataRef
{
    protected ExecutableContext ctx = ctx;
    protected int index = index;

    public T GetData() => (T)ctx.GetConstantReference(index);
}
