using Abstract.Parser.Core.Language.SyntaxNodes.Base;

namespace Abstract.Parser.Core.Language.SyntaxNodes.Misc;

public class ParameterCollectionNode : SyntaxNode
{
    public override string ToString() => $"({string.Join(", ", _children[1..^1])})";
}
