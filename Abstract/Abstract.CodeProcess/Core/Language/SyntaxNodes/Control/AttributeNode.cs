namespace Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;

public class AttributeNode : ControlNode
{
    public override string ToString() => $"@{string.Join("", (object[])Children[1..])}";
}
