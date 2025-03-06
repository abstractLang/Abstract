using System;

namespace Abstract.Core.Language;

public interface ISyntaxNode
{
    public ISyntaxNode Parent { get; protected set; }
    public NodeKind Kind { get; }
    public string ToString();
}
