using System.Text;
using Abstract.Parser.Core.Language.SyntaxNodes.Base;
using Abstract.Parser.Core.ProgData;

namespace Abstract.Parser.Core.Language.SyntaxNodes.Control;

public class BlockNode : SyntaxNode
{
    public ExecutableCodeBlock EvaluatedData {get; set; } = null!;

    public IEnumerable<SyntaxNode> Content => _children.Count > 2 ? _children[1..^1] : [];
    
    public override string ToString()
    {
        var sb = new StringBuilder();

        sb.AppendLine(_children[0].ToString());

        if (_children.Count > 2)
        {
            foreach (var i in _children[1..^1])
            {
                var lines = i.ToString().Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries);
                foreach (var j in lines) sb.AppendLine($"\t{j}");
            }
        }

        sb.AppendLine(_children[^1].ToString());

        return sb.ToString();
    }
    public override string ToFancyString()
    {
        var sb = new StringBuilder();

        sb.AppendLine(_children[0].ToString());

        if (_children.Count > 2)
        {
            foreach (var i in _children[1..^1])
            {
                var lines = i.ToFancyString().Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries);
                foreach (var j in lines) sb.AppendLine($"\t{j}");
            }
        }

        sb.AppendLine(_children[^1].ToString());

        return sb.ToString();
    }
}
