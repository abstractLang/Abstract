using Abstract.Build.Core.Exceptions;
using Abstract.Build.Core.Sources;
using Abstract.Parser.Core.Language.AbstractSyntaxTree;

namespace Abstract.Parser.Core.Exeptions.Evaluation;

public class InvalidScoppingException (SyntaxNode node) : CompilerException($"Invalid scope here!!! ({node.GetType().Name})")
{

    private readonly SyntaxNode _node = node;

}
