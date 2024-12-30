using Abstract.Build.Core.Sources;
using Abstract.Build.Exceptions;
using Abstract.Parser.Core.Language.SyntaxNodes.Statement;

namespace Abstract.Parser.Core.Exceptions.Evaluation;

public class NoOverloadForTypes(
    FunctionCallExpressionNode call,
    string types
)
: SyntaxException(
    call.GetSourceScript(),
    call.Range,
    $"the target {call.FunctionReference} in \"{call}\" do not expect the type sequence ({types})"
)
{
}
