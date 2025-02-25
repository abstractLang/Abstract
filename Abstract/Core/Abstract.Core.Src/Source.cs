namespace Abstract.Core.Src;

public abstract class Source
{

    public abstract Memory<char> Read();
    public override string ToString() => $"undefined source";
}
