using Abstract.Build.Core.Sources;
using Abstract.Parser.Core.Language.AbstractSyntaxTree;

namespace Abstract.Parser.Core.Exeptions.Evaluation;

public class InvalidScoppingException : EvaluationException
{

    public InvalidScoppingException(SyntaxNode node) : base(node)
    {
        _node = node;
    }
    public InvalidScoppingException(SyntaxNode node, Script s) : base(node, s)
    {
        _node = node;
    }

    private readonly SyntaxNode _node;
    public override string Message => $"Syntax element of type {_node.GetType().Name} is not allowed on this scope!";

}
