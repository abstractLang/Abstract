using Abstract.Parser.Core.ProgData;

namespace Abstract.Parser.Core.ProgMembers;

public abstract class ProgramMember(ProgramMember? parent, MemberIdentifier identifier)
{

    public virtual ProgramRoot? ParentProgram => parent?.ParentProgram;
    public virtual Project? ParentProject => parent?.ParentProject;

    public readonly ProgramMember? parent = parent;
    public readonly MemberIdentifier identifier = identifier;

    protected List<MemberAttribute> _attributes = [];
    protected Dictionary<MemberIdentifier, Namespace> _childrenNamespaces = [];
    protected Dictionary<MemberIdentifier, FunctionGroup> _childrenFunctions = [];
    protected Dictionary<MemberIdentifier, Structure> _childrenTypes = [];
    protected Dictionary<MemberIdentifier, Field> _childrenFields = [];
    protected Dictionary<MemberIdentifier, Enumerator> _childrenEnums = [];
    protected Dictionary<MemberIdentifier, ProgramMember> _allChildren = [];

    public MemberAttribute[] Attributes => [.. _attributes];
    public Namespace[] ChildrenNamespaces => [.. _childrenNamespaces.Values];
    public FunctionGroup[] ChildrenFunctions => [.. _childrenFunctions.Values];
    public Structure[] ChildrenTypes => [.. _childrenTypes.Values];
    public Field[] ChildrenFields => [.. _childrenFields.Values];
    public Enumerator[] ChildrenEnums => [.. _childrenEnums.Values];

    public virtual MemberIdentifier GlobalReference
        => parent != null ? parent.GlobalReference + identifier : identifier;

    #region tree
    public void AppendChild(ProgramMember child)
    {
        if (child is Namespace @a) _childrenNamespaces.Add(a.identifier, a);
        else if (child is Structure @b) _childrenTypes.Add(b.identifier, b);
        else if (child is Field @c) _childrenFields.Add(c.identifier, c);
        else if (child is Enumerator @d) _childrenEnums.Add(d.identifier, d);

        else if (child is Function @e)
        {
            FunctionGroup group;
            if (!_childrenFunctions.TryGetValue(e.identifier, out group!))
            {
                group = new(this, e.identifier);
                _allChildren.Add(e.identifier, group);
            }
            group.AddOverload(e);

            return;
        }
        else if (child is FunctionGroup @f) _childrenFunctions.Add(f.identifier, f);

        _allChildren.Add(child.identifier, child);
    }
    public virtual ProgramMember? SearchForChild(MemberIdentifier identifier)
    {
        // Search on this scope
        if (_allChildren.TryGetValue(identifier, out var res)) return res;

        // Search on parents and namespace
        if (this is not Namespace) return parent?.SearchForChild(identifier);
        
        return null;
    }
    public virtual TypeReference? SearchForGeneric(MemberIdentifier identifier)
    {
        return parent?.SearchForGeneric(identifier);
    }
    #endregion

    #region attributes
    public void AppendAttribute(params MemberAttribute[] attrib)
    {
        foreach (var i in attrib) _attributes.Add(i);
    }
    public bool HasAttribute(string name) => _attributes.Any((e) => e.Name == name);
    public bool TryGetAttribute(string name, out MemberAttribute attrib)
    {
        attrib = _attributes.FirstOrDefault((e) => e.Name == name)!;
        return attrib != null;
    }
    public MemberAttribute GetAttribute(string name) => _attributes.First((e) => e.Name == name);
    public MemberAttribute[] GetAttributesList() => [.. _attributes];
    #endregion
}
