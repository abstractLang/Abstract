namespace Abstract.Parser.Core.ProgMembers;

public class Enumerator(ProgramMember? parent, MemberIdentifier identifier)
: ProgramMember(parent, identifier)
{
    public override string ToString() => $"{GlobalReference}";
}
