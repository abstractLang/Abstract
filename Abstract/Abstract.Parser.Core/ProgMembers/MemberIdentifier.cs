using System.Diagnostics.CodeAnalysis;

namespace Abstract.Parser.Core.ProgMembers;

public readonly struct MemberIdentifier
{

    public MemberIdentifier(IEnumerable<string> tokens) => this.tokens = [.. tokens];
    public MemberIdentifier(params string[] tokens) => this.tokens = tokens;
    public MemberIdentifier(string identifier) => tokens = identifier.Split('.');

    public readonly string[] tokens;
    public readonly string Name => string.Join(".", tokens);
    public override readonly string ToString() => Name;

    public static MemberIdentifier operator +(MemberIdentifier a, MemberIdentifier b) => new([.. a.tokens, ..b.tokens]);

    public static bool operator ==(MemberIdentifier a, MemberIdentifier b) => a.Equals(b);
    public static bool operator !=(MemberIdentifier a, MemberIdentifier b) => !a.Equals(b);

    public override bool Equals([NotNullWhen(true)] object? obj)
    {
        if (obj is MemberIdentifier @mi)
            return mi.Name == Name;
        return base.Equals(obj);
    }
    public override int GetHashCode() => HashCode.Combine(Name);

}
