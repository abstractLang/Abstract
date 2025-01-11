using System.Diagnostics.CodeAnalysis;
using System.Text;

namespace Abstract.Binutils.Abs.Bytecode;

public readonly struct InstructionInfo(byte opcode, Base b, Types t, Condition c, bool u, DataSize s, string[] a)
{
    public readonly byte opcode = opcode;
    public readonly Base b = b;
    public readonly Types t = t;
    public readonly Condition c = c;
    public readonly bool u = u;
    public readonly DataSize s = s;

    public readonly string[] args = a;

    public override string ToString()
    {
        StringBuilder sb = new();

        sb.Append(b.ToString());
        if (t != 0) sb.Append($" {t.ToString().ToLower()}");
        if (c != 0) sb.Append($".{c}");
        if (u) sb.Append($".u");

        if (s != DataSize.Unespecified) sb.Append($".{s switch {
            DataSize.Byte => 'b',
            DataSize.Short => 's',
            DataSize.Int => 'i',
            DataSize.Long => 'l',
            _ => 'b'
        }}");

        return sb.ToString();
    }

    public string ToFancyString()
    {
        StringBuilder sb = new();

        sb.Append(b.ToString());

        if (c != 0) sb.Append($".{c}");

        if (t != 0) {
            sb.Append(new string(' ', Math.Max(0, 10 - sb.Length)));
            sb.Append($"{t.ToString().ToLower()}");
        }

        if (u) sb.Append($".u");

        if (args != null && args.Length > 0) 
        {
            sb.Append(new string(' ', Math.Max(0, 20 - sb.Length)));
            sb.Append(string.Join(' ', args.Select(e => $"{{{e}}}")));
        }


        if (s != DataSize.Unespecified) sb.Append($".{s switch {
            DataSize.Byte => 'b',
            DataSize.Short => 's',
            DataSize.Int => 'i',
            DataSize.Long => 'l',
            _ => 'b'
        }}");

        return sb.ToString();
    }

    public static bool operator ==(InstructionInfo a, InstructionInfo b) => a.Equals(b);
    public static bool operator !=(InstructionInfo a, InstructionInfo b) => !a.Equals(b);

    public override bool Equals([NotNullWhen(true)] object? obj)
        => obj != null && obj.GetHashCode() == this.GetHashCode();
    public override int GetHashCode()
        => HashCode.Combine(ToString());
}
