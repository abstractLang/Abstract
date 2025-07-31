namespace Abstract.Compiler.Parser.Core.Nodes.Control;

public class NamespaceNode(string name)
{

    public string Name { get; set; } = name;

    public string[] FullName => Parent?.FullName == null ? [Name] : [..Parent!.FullName, Name];

    private readonly List<NamespaceNode> _children = [];

    public NamespaceNode? Parent { get; private set; } = null;

    public NamespaceNode[] Children => [.. _children];

    
    public void AddChild(NamespaceNode child)
    {
        _children.Add(child);
        child.Parent = this;
    }

}
