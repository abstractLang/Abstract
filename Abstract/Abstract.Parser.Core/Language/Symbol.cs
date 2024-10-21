using Abstract.Parser.Core.Language.AbstractSyntaxTree;
using System.Diagnostics.CodeAnalysis;

namespace Abstract.Parser.Core.Language;

public interface ISymbol
{
    public string[] Tokens { get; }

    public bool Equals(object? obj);
    public string ToString();
    /* FORMAT:
    *  g - global
    *  l | default - local
    */
}

public struct TempSymbol (IEnumerable<string> tokens) : ISymbol
{
    private readonly string[] _tokens = [.. tokens];
    public readonly string[] Tokens => [.. _tokens];

    public bool IsCompound => _tokens.Length > 0;


    public static bool operator ==(TempSymbol a, ISymbol b) => a.Equals(b);
    public static bool operator !=(TempSymbol a, ISymbol b) => !a.Equals(b);
    public override bool Equals([NotNullWhen(true)] object? obj)
    {
        if (obj is ISymbol @symbol) return Enumerable.SequenceEqual(_tokens, symbol.Tokens);
        else return false;
    }
    public override int GetHashCode() => base.GetHashCode();


    public override string ToString() => ToString("l", null);

    public string ToString(string? format = null) => ToString(format, null);
    public string ToString(string? format, IFormatProvider? formatProvider) => string.Join('.', tokens);
}

public struct ReferenceSymbol : ISymbol
{
    private readonly string[] _tokens;
    public readonly string[] Tokens => [.. _tokens];
    public readonly MasterSymbol pointingTo;

    public bool IsCompound => _tokens.Length > 0;

    public ReferenceSymbol(IEnumerable<string> tokens, MasterSymbol pointsTo)
    {
        _tokens = [.. tokens];
        pointingTo = pointsTo;
    }
    public ReferenceSymbol(ISymbol from, MasterSymbol pointsTo)
    {
        _tokens = from.Tokens;
        pointingTo = pointsTo;
    }

    public static bool operator ==(ReferenceSymbol a, ISymbol b) => a.Equals(b);
    public static bool operator !=(ReferenceSymbol a, ISymbol b) => !a.Equals(b);
    public override bool Equals([NotNullWhen(true)] object? obj)
    {
        if (obj is ISymbol @symbol) return Enumerable.SequenceEqual(_tokens, symbol.Tokens);
        else return false;
    }
    public override int GetHashCode() => base.GetHashCode();

    public override string ToString() => pointingTo?.ToString() ?? string.Join('.', _tokens);

}

public class MasterSymbol(IEnumerable<string> tokens, IReferenceable pointsTo) : ISymbol
{
    private readonly string[] _tokens = [.. tokens];
    public string[] Tokens => [.. _tokens];
    public readonly IReferenceable pointsTo = pointsTo;

    public bool IsCompound => _tokens.Length > 0;


    public static bool operator ==(MasterSymbol a, ISymbol b) => a.Equals(b);
    public static bool operator !=(MasterSymbol a, ISymbol b) => !a.Equals(b);
    public override bool Equals([NotNullWhen(true)] object? obj)
    {
        if (obj is ISymbol @symbol) return Enumerable.SequenceEqual(_tokens, symbol.Tokens);
        else return false;
    }
    public override int GetHashCode() => base.GetHashCode();


    public override string ToString() => string.Join('.', tokens);
}
