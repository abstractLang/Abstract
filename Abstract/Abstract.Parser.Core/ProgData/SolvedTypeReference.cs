using Abstract.Parser.Core.ProgMembers;

namespace Abstract.Parser.Core.ProgData;

public class SolvedTypeReference : TypeReference
{

    public SolvedTypeReference(Structure struc)
    {
        structure = struc;
        isGeneric = false;
        arguments = null!;
    }
    public SolvedTypeReference(Structure struc, params IGenericExpression[] args)
    {
        structure = struc;
        isGeneric = true;
        arguments = args;
    }

    public readonly Structure structure;

    public readonly bool isGeneric;
    public readonly IGenericExpression[] arguments;

    public override string ToString() => ToString("n");
    public override string ToString(string format) => (format != "n" ? $"(Solved) " : "")
    + $"{structure}"
    + (isGeneric ? $"({string.Join(", ", arguments.Select(e => e?.ToString() ?? "nil"))})" : "");
}
