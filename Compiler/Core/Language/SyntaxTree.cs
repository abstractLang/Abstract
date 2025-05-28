using Abstract.Core.Src;

namespace Abstract.Core.Language;

public sealed class SyntaxTree(Script src)
{

    public readonly Script src = src;
    public readonly SyntaxNode root = new(NodeKind.Root);

}
