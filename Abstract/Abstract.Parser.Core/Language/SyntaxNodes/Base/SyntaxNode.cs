using System.Text;
using Abstract.Build.Core.Sources;

namespace Abstract.Parser.Core.Language.SyntaxNodes.Base;

public abstract class SyntaxNode
{
    protected SyntaxNode _parent = null!;
    public virtual (uint start, uint end) Range => (_children[0].Range.start, _children[^1].Range.end);

    #region Tree related
    protected List<SyntaxNode> _children = [];
    public SyntaxNode[] Children => [.. _children];
    
    public void AppendChild(SyntaxNode node, int idx = -1)
    {
        if (node != null) {
            node._parent = this;
            if (idx == -1)
                _children.Add(node);
            else
                _children.Insert(idx, node);
        }
    }
    public int GetChildIndex(SyntaxNode child)
    {
        return _children.IndexOf(child);
    }
    public void RemoveChild(SyntaxNode child)
    {
        _children.Remove(child);
    }
    public void RemoveChild(int childIndex)
    {
        _children.RemoveAt(childIndex);
    }
    public void ReplaceChild(SyntaxNode target, SyntaxNode replacement)
    {
        int idx = GetChildIndex(target);
        RemoveChild(idx);
        AppendChild(replacement, idx);
    }
    #endregion

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
