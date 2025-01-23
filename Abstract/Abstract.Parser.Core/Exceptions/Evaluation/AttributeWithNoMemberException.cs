using Abstract.Build.Exceptions;
using Abstract.Parser.Core.Language.SyntaxNodes.Control;

namespace Abstract.Parser.Core.Exceptions.Evaluation;

public class AttributeWithNoMemberException(AttributeNode attribute)
: SyntaxException(
    attribute.GetSourceScript(),
    attribute.Range,
    $"Attribute {attribute} is being associated with no program member"
)
{
}
