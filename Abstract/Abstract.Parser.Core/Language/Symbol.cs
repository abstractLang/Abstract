namespace Abstract.Parser.Core.Language;

public class Symbol
{
    public string[] Name { get; set; }
    public bool IsCompound => Name.Length > 1;


    public override string ToString() => string.Join('.', Name);
}
