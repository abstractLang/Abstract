using Abstract.Build.Core.Exceptions;
using Abstract.Parser.Core.Language.AbstractSyntaxTree;

namespace Abstract.Parser.Core.Exeptions.Evaluation;

public abstract class EvaluationException(SyntaxNode node) : CompilerException(node.SourceScript, node.Range, "")
{
}
