namespace Abstract.Parser.Core.ProgMembers;

public abstract class ProgramMember(ProgramMember? parent, MemberIdentifier identifier)
{

    public readonly ProgramMember? parent = parent;
    public readonly MemberIdentifier identifier = identifier;


    public MemberIdentifier GlobalReference
        => parent != null ? parent.GlobalReference + identifier : identifier;

}
