using Abstract.Parser.Core.ProgData;

namespace Abstract.Parser.Core.ProgMembers;

public class Field(ProgramMember? parent, MemberIdentifier identifier)
: ProgramMember(parent, identifier)
{

    public TypeReference dataType = null!;
    public override string ToString() => $"{GlobalReference}";

}
