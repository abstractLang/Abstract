using Abstract.Parser.Core.Language.SyntaxNodes.Control;
using Abstract.Parser.Core.ProgData;

namespace Abstract.Parser.Core.ProgMembers;

public class Structure(StructureDeclarationNode node, ProgramMember? parent, MemberIdentifier identifier)
: ProgramMember(parent, identifier)
{
    public readonly StructureDeclarationNode structureNode = node;

    public bool IsGeneric => _generics.Count > 0;
    protected Dictionary<MemberIdentifier, TypeReference> _generics = [];

    protected Dictionary<string, FunctionGroup> _overloadedOperators = [];
    protected Dictionary<Structure, Function> _implicitConversions = [];

    public string[] AvaliableOperators => [.. _overloadedOperators.Keys];

    public Structure extends = null!;

    public int bitsize = 0;
    public int aligin = -1; // -1 means not specified

    public void AppendGeneric(MemberIdentifier identifier, int idx)
    {
        _generics.Add(identifier, new GenericTypeReference(this, idx));
    }
    public void AppendOperator(string op, Function overload)
    {
        if (!_overloadedOperators.TryGetValue(op, out var _val))
            _overloadedOperators.Add(op, new(this, op));
        
        _overloadedOperators[op].AddOverload(overload);
    }

    public void AppendImplicitConvert(Structure to, Function how)
    {
        _implicitConversions.Add(to, how);
    }

    public override TypeReference? SearchForGeneric(MemberIdentifier identifier)
    {
        if (_generics.TryGetValue(identifier, out var r)) return r;
        return base.SearchForGeneric(identifier);
    }
    public FunctionGroup? SearchForOperators(string op)
    {
        if (_overloadedOperators.TryGetValue(op, out var r)) return r;
        return null;
    }
    

    public bool CanBeDirectlyImplicitlyConvertedTo(Structure target)
    => _implicitConversions.ContainsKey(target);
    public Function? TryGetImplicitConvertTo(Structure target)
    {
        if (_implicitConversions.TryGetValue(target, out var r)) return r;
        return null!;
    }

    public override string ToString() => GlobalReference;
}
