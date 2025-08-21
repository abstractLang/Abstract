using Abstract.Build.Core.Sources;
using Abstract.Build.Exceptions;
using Abstract.Parser.Core.Language.SyntaxNodes.Expression;
using Abstract.Parser.Core.Language.SyntaxNodes.Statement;

namespace Abstract.Parser.Core.Exceptions.Evaluation;

public class ReferenceNotFound(IdentifierCollectionNode reference)
: SyntaxException(
    reference.GetSourceScript(),
    reference.Range,
    $"reference \"{reference}\" not found in this scope"
)
{
}
