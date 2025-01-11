namespace Abstract.Parser.Core.ProgMembers;

public class ProgramRoot(MemberIdentifier programName) : ProgramMember(null, programName)
{
    public override ProgramRoot? ParentProgram => this;
    public override Project? ParentProject => null!;

    #region Reference tables
    private Dictionary<MemberIdentifier, ProgramMember> _globalReferences = [];
    public Dictionary<MemberIdentifier, Project> projects = [];
    public Dictionary<MemberIdentifier, Namespace> namespaces = [];
    public Dictionary<MemberIdentifier, FunctionGroup> functions = [];
    public Dictionary<MemberIdentifier, Structure> types = [];
    public Dictionary<MemberIdentifier, Field> fields = [];
    public Dictionary<MemberIdentifier, Enumerator> enums = [];
    public Dictionary<MemberIdentifier, List<AttributeReference>> attributes = [];
    #endregion

    public void AppendProject(Project proj)
    {
        projects.Add(proj.identifier, proj);
    }

    public void AppendGlobals(MemberIdentifier alias, ProgramMember member)
    {
        _globalReferences.Add(alias, member);
    }
    public ProgramMember? TryGetGlobal(MemberIdentifier name)
    {
        if (_globalReferences.TryGetValue(name, out var res)) return res;
        return null;
    }

    public override ProgramMember? SearchForChild(MemberIdentifier name)
    {
        if (projects.TryGetValue(name.Step.step, out var _v) && name.Step.next.HasValue)
        {
            ProgramMember currNode = _v;
            (var step, var next) = name.Step.next.Value.Step;

            do {
                currNode = currNode.SearchForChild(step)!;

                if (next.HasValue) (step, next) = next.Value.Step;
                else break;
            }
            while (currNode != null);
            
            return currNode;
        }
        return null!;
    }

    public override string ToString() => $"{identifier}";
}
