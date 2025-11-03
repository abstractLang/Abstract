using System.Text;

namespace Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;

public abstract class SyntaxNode : IFormattable
{
    protected SyntaxNode _parent = null!;

    public SyntaxNode Parent => _parent;
    public (uint line_start, uint line_end, uint start, uint end)? OverrideRange { get; set; } = null;
    public string? OverrideToString { get; set; } = null;

    public virtual (uint line_start, uint line_end, uint start, uint end) Range => OverrideRange.HasValue
        ? OverrideRange!.Value
        : (_children[0].Range.line_start, _children[^1].Range.line_end, _children[0].Range.start, _children[^1].Range.end);
    

    #region Tree related
    protected List<SyntaxNode> _children = [];
    public SyntaxNode[] Children => [.. _children];
    
    public void AppendChild(SyntaxNode node, int idx = -1)
    {
        if (node == null) return;
        
        node._parent = this;
        if (idx == -1)
            _children.Add(node);
        else
            _children.Insert(idx, node);
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

    
    public string ToString(string? f, IFormatProvider? _) => ToString(f);
    public string ToString(string? f)
    {
        return f == "pos"
            ? $"\'{ToString()}\' ({Range.line_start + 1}:{Range.start + 1})"
            : ToString();
    }

    public override string ToString()=> string.IsNullOrEmpty(OverrideToString)
        ? $"{string.Join(" ", _children)}"
        : OverrideToString;
    
    
    public virtual string ToTree()
    {
        var buf = new StringBuilder();

        buf.AppendLine($"{GetType().Name}");
        for (var i = 0; i < _children.Count; i++)
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
