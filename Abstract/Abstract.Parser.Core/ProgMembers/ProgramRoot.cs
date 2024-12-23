namespace Abstract.Parser.Core.ProgMembers;

public class ProgramRoot(MemberIdentifier programName) : ProgramMember(null, programName)
{

    protected Dictionary<MemberIdentifier, ProgramMember> _globalReferences = [];

    public void AppendGlobals(MemberIdentifier alias, ProgramMember member)
    {
        _globalReferences.Add(alias, member);
    }

    public ProgramMember? TryGetGlobal(MemberIdentifier name)
    {
        if (_globalReferences.TryGetValue(name, out var res)) return res;
        return null;
    }
}
