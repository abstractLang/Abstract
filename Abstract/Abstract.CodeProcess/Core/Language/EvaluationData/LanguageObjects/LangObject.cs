using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.AttributeReferences;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;

namespace Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;

public abstract class LangObject(string[] global)
{
    private readonly List<AttributeReference> _attributes = [];
    private LangObject _parent = null!;
    private readonly List<LangObject> _children = [];
 
    public readonly string[] Global = global;
    public LangObject Parent => _parent;
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

