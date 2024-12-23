namespace Abstract.Parser.Core.ProgMembers;

public class AttributeReference(Function func, MemberIdentifier identifier)
{
    public readonly MemberIdentifier identifier = identifier;
    public readonly Function function = func;

    public override string ToString() => $"{identifier}";
}
