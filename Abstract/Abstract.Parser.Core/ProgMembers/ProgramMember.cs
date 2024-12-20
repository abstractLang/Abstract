namespace Abstract.Parser.Core.ProgMembers;

public abstract class ProgramMember(ProgramMember? parent, MemberIdentifier identifier)
{

    public readonly ProgramMember? parent = parent;
    public readonly MemberIdentifier identifier = identifier;

    protected List<MemberAttribute> _attributes = [];
    protected List<Namespace> _childrenNamespaces = [];
    protected List<Function> _childrenFunctions = [];
    protected List<Structure> _childrenTypes = [];
    protected List<Field> _childrenFields = [];
    protected List<Enumerator> _childrenEnums = [];
    // functions are not included here because of overloading :p \/
    protected Dictionary<MemberIdentifier, ProgramMember> _allChildren = [];

    public MemberAttribute[] Attributes => [.. _attributes];
    public Namespace[] ChildrenNamespaces => [.. _childrenNamespaces];
    public Function[] ChildrenFunctions => [.. _childrenFunctions];
    public Structure[] ChildrenTypes => [.. _childrenTypes];
    public Field[] ChildrenFields => [.. _childrenFields];
    public Enumerator[] ChildrenEnums => [.. _childrenEnums];

    public MemberIdentifier GlobalReference
        => parent != null ? parent.GlobalReference + identifier : identifier;

    #region tree
    public void AppendChild(ProgramMember child)
    {
        if (child is Namespace @a) _childrenNamespaces.Add(a);
        else if (child is Function @b) _childrenFunctions.Add(b);
        else if (child is Structure @c) _childrenTypes.Add(c);
        else if (child is Field @f) _childrenFields.Add(f);
        else if (child is Enumerator @e) _childrenEnums.Add(e);

        if (child is not Function)
            _allChildren.Add(child.identifier, child);
    }
    public virtual ProgramMember? SearchForChild(MemberIdentifier identifier)
    {
        // Search on this scope
        if (_allChildren.TryGetValue(identifier, out var res)) return res;

        // Search on prents and namespace
        if (this is not Namespace) return parent?.SearchForChild(identifier);
        
        return null;
    }
    #endregion

    #region attributes
    public void AppendAttribute(params MemberAttribute[] attrib)
        => _attributes.AddRange(attrib);
    public bool HasAttribute(MemberIdentifier name)
        => _attributes.Any(e => e.Name == name);
    public bool TryGetAttribute(MemberIdentifier name, out MemberAttribute attrib)
    {
        attrib = _attributes.Find(e => e.Name == name)!;
        return attrib != null;
    }
    public MemberAttribute GetAttribute(MemberIdentifier name)
        => _attributes.Find(e => e.Name == name)!;
    #endregion
}
