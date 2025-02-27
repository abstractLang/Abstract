namespace Abstract.Core.Src;

public abstract class Source
{

    public abstract ReadOnlyMemory<char> Read();
    public override string ToString() => $"undefined source";
}
