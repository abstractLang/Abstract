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

    public TypeReference returnType = null!;
    public (TypeReference type, MemberIdentifier name)[] parameters = null!;

    /*
        A function can be generic in 2 cases:
            - if there is any type as it parameter;
            - if there is any 'anytype' parameter;
    */
    public bool IsGeneric => _generics.Count > 0 || parameters.Any(e
        => (e.type as SolvedTypeReference)?
        .structure.GlobalReference == "Std.Types.AnyType");
    protected Dictionary<MemberIdentifier, TypeReference> _generics = [];

    public void AppendGeneric(MemberIdentifier identifier, int idx)
    {
        _generics.Add(identifier, new GenericTypeReference(this, idx));
    }
    
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

    public override string ToString() => (parameters != null && returnType != null)
        ? $"{GlobalReference}({string.Join(", ", parameters.Select(e => e.type) ?? [])}) -> {returnType}"
        : $"{GlobalReference}({functionNode.ParameterCollection}) -> {functionNode.ReturnType}";

}
