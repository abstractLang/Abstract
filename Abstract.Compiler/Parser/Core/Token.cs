namespace Abstract.Compiler.Parser.Core;

public struct Token
{
    public string value;
    public TokenKind type;

    public uint start;
    public uint end;

    public bool spaceBefore;
    public bool spaceAfter;

    public readonly (uint start, uint end) Range => (start, end);
    public readonly uint RangeLength => end - start;

    public readonly override string ToString() => $"{value} ({type})";
    public readonly string ValueString() => type switch
        {
            TokenKind.LineFeedChar => "\n",
            TokenKind.EofChar => "[\\EOF]",
            _ => value,
        };
}
