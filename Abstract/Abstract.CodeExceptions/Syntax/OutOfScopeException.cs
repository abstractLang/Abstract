using Abstract.Build.Core.Sources;
using Abstract.Build.Exceptions;
using Abstract.Parser.Core.Language.SyntaxNodes.Base;

namespace Abstract.Parser.Core.Exceptions.Syntax;

public class OutOfScopeException(Script s, SyntaxNode node) : SyntaxException(s, node.Range)
{

    private SyntaxNode _outNode = node;

    public override string Message
        => $"\"{_outNode.ToString().Split("\n", StringSplitOptions.RemoveEmptyEntries)[0]}\" ({_outNode.GetType().Name})"
        + " is not allowed in this scope";

}
