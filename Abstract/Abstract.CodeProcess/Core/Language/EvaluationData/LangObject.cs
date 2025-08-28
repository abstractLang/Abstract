using Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;

namespace Abstract.CodeProcess.Core.Language.EvaluationData;

public abstract class LangObject(string[] global)
{
    private readonly List<AttributeNode> _attributes = [];
    private LangObject _parent = null!;
    private readonly List<LangObject> _children = [];
 
    public readonly string[] Global = global;
    public LangObject Parent => _parent;
    public LangObject[] Children => [.. _children];
    public AttributeNode[] Attributes => [.. _attributes];
    
    public void AppendAttributes(params AttributeNode[] attrs) => _attributes.AddRange(attrs);

    public void AppendChild(LangObject child)
    {
        _children.Add(child);
        child._parent = this;
    }
}
