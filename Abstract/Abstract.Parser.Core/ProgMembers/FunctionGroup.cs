using System.Collections;

namespace Abstract.Parser.Core.ProgMembers;

public class FunctionGroup(ProgramMember? parent, MemberIdentifier identifier)
: ProgramMember(parent, identifier), IEnumerable
{
    protected List<Function> _overloads = [];
    public bool IsSingle => _overloads.Count == 1;

    public Function[] Overloads => [.. _overloads];

    public void AddOverload(Function overload)
    {
        _overloads.Add(overload);
    }

    public override string ToString() => IsSingle
        ? $"{_overloads[0]}"
        : $"{identifier} ({_overloads.Count})";

    IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
    public IEnumerator<Function> GetEnumerator() => _overloads.GetEnumerator();
}
