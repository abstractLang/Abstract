using Abstract.Parser.Core.Language.SyntaxNodes.Base;
using Abstract.Parser.Core.ProgData.DataReference;

namespace Abstract.Parser.Core.Language.SyntaxNodes.Expression;

public abstract class ExpressionNode : InstructionalNode
{
    public DataRef DataReference { get; set; } = null!;
}
