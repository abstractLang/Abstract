using Abstract.Parser.Core.Language.SyntaxNodes.Control;
using Abstract.Parser.Core.ProgData;

namespace Abstract.Parser.Core.ProgMembers;

public class Function(
    ProgramMember? parent,
    MemberIdentifier identifier,
    FunctionDeclarationNode node
) : ProgramMember(parent, identifier)
{
    public readonly FunctionDeclarationNode functionNode = node;
    public BlockNode FunctionBodyNode => functionNode.Body!;

    public (TypeReference type, MemberIdentifier name)[] baseParameters = null!;
    public TypeReference baseReturnType = null!;

    /*
        A function can be generic in 2 cases:
            - if there is any type as it parameter;
            - if there is any 'anytype' parameter;
    */
    public bool IsGeneric => _generics.Count > 0;
    protected Dictionary<MemberIdentifier, TypeReference> _generics = [];

    public List<(TypeReference[] key, (ExecutableContext ctx, TypeReference returnType) value)> implementations = [];

    public void AppendGeneric(MemberIdentifier identifier, int idx)
    {
        _generics.Add(identifier, new GenericTypeReference(this, idx));
    }
    
    public void AppendImplementation(TypeReference[] argumentTypes, ExecutableContext context, TypeReference returns)
    {
        implementations.Add((argumentTypes, (context, returns)));
    }
    public (ExecutableContext ctx, TypeReference returnType) GetImplementation(TypeReference[] types)
    {
        return implementations.Find(e => Enumerable.SequenceEqual(e.key, types)).value;
    }
    public bool HasImplementation(TypeReference[] types)
    {
        return implementations.Any((e => Enumerable.SequenceEqual(e.key, types)));
    }
    public bool HasAnyImplementation() => implementations.Count > 0;

    public override ProgramMember? SearchForChild(MemberIdentifier identifier)
    {
        /*
        * Functions does not owns nothing
        * other than some generic type
        * references, so we throw the search
        * call for the parent.  
        */
        return parent!.SearchForChild(identifier);
    }
    public override TypeReference? SearchForGeneric(MemberIdentifier identifier)
    {
        if (_generics.TryGetValue(identifier, out var r)) return r;
        return base.SearchForGeneric(identifier);
    }

    public override string ToString() => (baseParameters != null && baseReturnType != null)
        ? $"{GlobalReference}({string.Join(", ", baseParameters.Select(e => e.type) ?? [])}) -> {baseReturnType}"
        : $"{GlobalReference}({functionNode.ParameterCollection}) -> {functionNode.ReturnType}";

}
