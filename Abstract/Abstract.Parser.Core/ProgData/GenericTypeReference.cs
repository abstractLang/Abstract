using Abstract.Parser.Core.ProgMembers;

namespace Abstract.Parser.Core.ProgData;

public class GenericTypeReference(ProgramMember from, int idx) : TypeReference
{
    public readonly ProgramMember from = from;
    public readonly int parameterIndex = idx;


    public override string ToString() => $"(Generic) param. {parameterIndex} of {from.GlobalReference}";
}
