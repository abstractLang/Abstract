using Abstract.Build.Core.Sources;
using Abstract.Build.Exceptions;
using Abstract.Parser.Core.Language.SyntaxNodes.Expression;
using Abstract.Parser.Core.Language.SyntaxNodes.Statement;

namespace Abstract.Parser.Core.Exceptions.Evaluation;

public class ReferenceNotCallableException(FunctionCallExpressionNode call)
: SyntaxException(
    call.GetSourceScript(),
    call.Range,
    $"\"{call}\" is not callable as {call.FunctionReference} is not a function nor a generic type"
)
{
}
