namespace Abstract.Parser.Core.ProgMembers;

public class Property(ProgramMember? parent, MemberIdentifier identifier)
: Field(parent, identifier)
{
    
    public Function getter = null!;
    public Function setter = null!;

    public override string ToString() => $"{GlobalReference} {GetAccessAsString()}";
    public string GetAccessAsString() => "{"
    + (getter != null ? $"get => {getter}" : "")
    + (getter != null && setter != null ? "; " : "")
    + (setter != null ? $"set => {setter}" : "")
    + '}';

}
