namespace Abstract.Parser.Core.ProgMembers;

public class Project(ProgramRoot program, MemberIdentifier projectName)
: ProgramMember(program, projectName)
{
    public override Project? ParentProject => this;
    public override MemberIdentifier GlobalReference => identifier;
    private List<ProgramMember> _dependences = [];
    public ProgramMember[] Dependences => [.. _dependences];

    public void TestUseOfReference(ProgramMember reference)
    {
        var refProject = reference.ParentProject;
        if (refProject != this && !_dependences.Contains(reference))
            _dependences.Add(reference);
    }

    public override string ToString() => $"Project {identifier}";
}
