using Abstract.Parser.Core.Language.AbstractSyntaxTree;

namespace Abstract.Parser.Core.Language;

public interface ISymbol
{
    public string ToString(string? format = null);
    /* FORMAT:
    *  g - global
    *  l | default - local
    */
}

public struct TempSymbol (IEnumerable<string> tokens) : ISymbol
{
    public readonly string[] tokens = [.. tokens];

    public bool IsCompound => tokens.Length > 0;

    public override string ToString() => ToString("l");
    public readonly string ToString(string? format = null) => string.Join('.', tokens);
}

public struct ReferenceSymbol(IEnumerable<string> tokens, MasterSymbol pointsTo) : ISymbol
{
    public readonly string[] tokens = [.. tokens];
    public readonly MasterSymbol pointingTo = pointsTo;

    public bool IsCompound => tokens.Length > 0;

    public override string ToString() => ToString("l");
    public string ToString(string? format = null) => format switch {
        "g" => pointingTo.ToString("g"),
        _ => string.Join('.', tokens)
    };
}

public class MasterSymbol(IEnumerable<string> tokens, IReferenceable pointsTo) : ISymbol
{
    public readonly string[] tokens = [.. tokens];
    public readonly IReferenceable pointsTo = pointsTo;

    public bool IsCompound => tokens.Length > 0;

    public override string ToString() => ToString("l");
    public string ToString(string? format = null) => string.Join('.', tokens);
}
