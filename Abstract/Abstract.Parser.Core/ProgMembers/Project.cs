namespace Abstract.Parser.Core.ProgMembers;

public class Project(ProgramRoot program, MemberIdentifier projectName)
: ProgramMember(program, projectName)
{
    public override Project? ParentProject => this;
    public override MemberIdentifier GlobalReference => identifier;

    public override string ToString() => $"Project {identifier}";
}
