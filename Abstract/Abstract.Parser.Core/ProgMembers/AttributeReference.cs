using Abstract.Parser.Core.ProgData.FunctionExecution;

namespace Abstract.Parser.Core.ProgMembers;

public class AttributeReference(ICallable func, MemberIdentifier identifier)
{
    public readonly MemberIdentifier identifier = identifier;
    public readonly ICallable callable = func;

    public override string ToString() => $"{identifier}";
}
