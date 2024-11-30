using Abstract.Build.Core.Exceptions;
using Abstract.Build.Core.Sources;
using Abstract.Parser.Core.Language.AbstractSyntaxTree;

namespace Abstract.Parser.Core.Exeptions.Evaluation;

public abstract class EvaluationException: CompilerException
{

    public EvaluationException(SyntaxNode node) : base(node.SourceScript, node.Range, "") {}
    public EvaluationException(SyntaxNode node, Script script) : base(script, node.Range, "") {}

}
