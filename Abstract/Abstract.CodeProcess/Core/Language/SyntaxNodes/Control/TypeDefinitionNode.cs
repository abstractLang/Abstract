using System.Text;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Value;

namespace Abstract.CodeProcess.Core.Language.SyntaxNodes.Control;

public class TypeDefinitionNode : ControlNode
{
    public IdentifierNode Identifier => (IdentifierNode)_children[1];
    public BlockNode Body => (BlockNode)_children[2];

    public override string ToString()
    {
        var sb = new StringBuilder();

        sb.Append($"typedef {Identifier} ");
        if (!Body.Content.Any())
        {
            sb.Append("{}");
            return sb.ToString();
        }
        sb.AppendLine("{");

        var blockList = Body.Content.ToArray();
        for (var i = 0; i < blockList.Length; i++)
        {
            var b = blockList[i];
            
            if (b is TypeDefinitionNode || i == blockList.Length - 1)
                sb.AppendLine($"\t{b}");
            else
                sb.AppendLine($"\t{b},");
        }
        
        sb.Append('}');

        return sb.ToString();
    }
}
