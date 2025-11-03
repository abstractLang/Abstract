using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.AttributeReferences;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;

public abstract class LangObject(string[] global, string name)
{
    private readonly List<AttributeReference> _attributes = [];
    private LangObject _parent = null!;
    private readonly List<LangObject> _children = [];
 
    public readonly string[] Global = global;
    public readonly string Name = name;
    public LangObject Parent { get =>_parent; set => _parent = value; }
    public virtual NamespaceObject Namespace => _parent.Namespace;
    public ImportObject? Imports = null;
    
    public LangObject[] Children => [.. _children];
    public AttributeReference[] Attributes => [.. _attributes];
    
    public void AppendAttributes(params AttributeReference[] attrs) => _attributes.AddRange(attrs);

    public void AppendChild(LangObject child)
    {
        _children.Add(child);
        child._parent = this;
    }

    public abstract override string ToString();
}

