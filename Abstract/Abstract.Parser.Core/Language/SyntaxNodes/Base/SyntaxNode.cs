using System.Text;
using Abstract.Build.Core.Sources;

namespace Abstract.Parser.Core.Language.SyntaxNodes.Base;

public abstract class SyntaxNode
{
    protected SyntaxNode _parent = null!;
    protected List<SyntaxNode> _children = [];
    public virtual (uint start, uint end) Range => (_children[0].Range.start, _children[^1].Range.end);

    public SyntaxNode[] Children => [.. _children];
    public void AppendChild(SyntaxNode node)
    {
        if (node != null) {
            node._parent = this;
            _children.Add(node);
        }
    }

    public virtual Script GetSourceScript() => _parent.GetSourceScript();

    public override string ToString() => $"{string.Join(" ", _children)}";
    public virtual string ToFancyString() => $"{string.Join(" ", _children.Select(e => e.ToFancyString()))}";
    
    public virtual string ToTree()
    {
        var buf = new StringBuilder();

        buf.AppendLine($"{GetType().Name}");
        for (int i = 0; i < _children.Count; i++)
        {
            buf.Append("  " + (i < _children.Count - 1 ? "|- " : "'- "));
            var lines = _children[i].ToTree().Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries);
            buf.AppendLine(lines[0]);
            foreach (var l in lines[1..])
                buf.AppendLine((i < _children.Count - 1 ? $"  |  " : $"     ") + l);
        }

        return buf.ToString();
    }
}
