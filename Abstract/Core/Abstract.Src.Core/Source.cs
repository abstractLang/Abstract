namespace Abstract.Src.Core;

public abstract class Source
{

    public abstract Memory<char> Read();
    public override string ToString() => $"undefined source";
}
