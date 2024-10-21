using Abstract.Parser.Core.Language.AbstractSyntaxTree;

namespace Abstract.Parser.Core.Exeptions.Evaluation;

public class InvalidScoppingException (SyntaxNode node) : EvaluationException(node)
{

    private readonly SyntaxNode _node = node;
    public override string Message => $"Invalid scope here!!! ({_node.GetType().Name})";

}
