namespace Abstract.CodeProcess.Core;

public class IdentifierComparer : IEqualityComparer<string[]>
{
    public static bool IsEquals(string[]? x, string[]? y)
    {
        if (x is null || y is null) return false;
        return x.SequenceEqual(y);
    }
    public bool Equals(string[]? x, string[]? y) => IsEquals(x, y);


    public int GetHashCode(string[] key)
    {
        var val = string.Join('.', key);
        return string.GetHashCode(val, StringComparison.Ordinal);
    }
}
