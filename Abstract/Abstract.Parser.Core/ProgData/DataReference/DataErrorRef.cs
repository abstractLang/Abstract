namespace Abstract.Parser.Core.ProgData.DataReference;

public class DataErrorRef() : DataRef
{
    public override TypeReference refferToType => null!;

    public override string ToString() => $"!cmperr!";
}
