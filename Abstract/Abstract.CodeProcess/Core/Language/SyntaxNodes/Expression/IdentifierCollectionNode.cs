using Abstract.CodeProcess.Core.Language.SyntaxNodes.Value;

namespace Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression;

public class IdentifierCollectionNode(bool incomplete = false) : ExpressionNode
{
    public readonly bool incomplete = incomplete;
    public IdentifierNode[] Nodes => Children.OfType<IdentifierNode>().ToArray(); 
    public string[] Values => Nodes.Select(n => n.Value).ToArray();

    public override string ReadableValue => (incomplete ? "." : "")
                                            + $"{string.Join('.', _children.Select(e => e.ReadableValue))}";
}
