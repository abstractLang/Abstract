using System.Text;

namespace Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;

public class BlockNode : SyntaxNode
{
    public IEnumerable<SyntaxNode> Content => _children.Count > 2
        ? _children[1..^1]
        : [];
    
    public string ToString(string f)
    {
        var sb = new StringBuilder();

        sb.AppendLine(_children[0].ToString());

        if (_children.Count > 2)
        {
            foreach (var j in _children[1..^1]
                .Select(i => i.ToString().Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries))
                .SelectMany(lines => lines)) sb.AppendLine($"\t{j}");
        }

        sb.AppendLine(_children[^1].ToString());

        return sb.ToString();
    }
}
