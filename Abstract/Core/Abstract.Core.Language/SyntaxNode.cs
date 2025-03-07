using System;

namespace Abstract.Core.Language;

public class SyntaxNode(NodeKind kind) : ISyntaxNode
{
    private ISyntaxNode _parent = null!;
    public ISyntaxNode Parent { get => _parent; set => _parent = value; }

    private readonly NodeKind _kind = kind;
    public NodeKind Kind => _kind;

    private List<ISyntaxNode> _children = [];
    public ISyntaxNode[] Children => [.. _children];

    public void AppendChild(ISyntaxNode node)
    {
        if (node is TokenNode @tknnode) tknnode.Parent = this;
        else if (node is SyntaxNode @stxnode) stxnode.Parent = this;
    
        _children.Add(node);
    }
    public void AppendChildren(IEnumerable<ISyntaxNode> nodes)
    {
        foreach (var node in nodes)
        {
            if (node is TokenNode @tknnode) tknnode.Parent = this;
            else if (node is SyntaxNode @stxnode) stxnode.Parent = this;
        
            _children.Add(node);
        }
    }

    public override string ToString() => string.Join("", _children);
}
