using Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;

namespace Abstract.CodeProcess.Core.Language;

public class Module(string name)
{
    public readonly string name = name;
    
    private List<NamespaceNode> _namespaces = [];
    public NamespaceNode[] Namespaces => [.. _namespaces];

    public NamespaceNode AddNamespace(string name)
    {
        var node = new NamespaceNode(name);
        _namespaces.Add(node);
        return node;
    }
    
}