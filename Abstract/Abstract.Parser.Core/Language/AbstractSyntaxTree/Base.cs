using Abstract.Build.Core.Sources;
using System.Collections;
using System.Text;

namespace Abstract.Parser.Core.Language.AbstractSyntaxTree;

/// <summary>
/// Represents a referenceable node
/// </summary>
public interface IReferenceable
{
    public IReferenceable? Parent { get; set; }
    public MasterSymbol Symbol { get; set; }
    public TypeNode ReturnType { get; set; }

    public ReferenceSymbol GetGlobalSymbol();
}

/// <summary>
/// The base for all syntax nodes
/// </summary>
public abstract class SyntaxNode
{
    public (uint start, uint end) Range = (0, 0);

    public virtual Script SourceScript => Parent?.SourceScript ?? null!;
    public SyntaxNode? Parent { get; set; }
    public List<AttributeNode> Attributes { get; set; } = [];

    public bool HasAttribute(string name) => Attributes.Any(e => e.Symbol.ToString() == name);
    public AttributeNode GetAttribute(string name) => Attributes.First(e => e.Symbol.ToString() == name);
    public bool TryGetAttribute(string name, out AttributeNode attribute)
    {
        attribute = Attributes.FirstOrDefault(e => e.Symbol.ToString() == name)!;
        return attribute != null;
    }

    public string AttributesToString() => string.Join(" ", Attributes) + '\n';
    public override string ToString() => $"{GetType().Name}SyntaxNode";
}

/// <summary>
/// The base for all syntax nodes that makes some kind of control
/// or scope
/// </summary>
public abstract class ControlNode : SyntaxNode
{

}
/// <summary>
/// The base for all syntax nodes that controlls scopes an iterable childrens
/// </summary>
/// <typeparam name="T"> The type of node that the scope accepts </typeparam>
public abstract class ControlScopeNode<T> : ControlNode, IEnumerable<T> where T : SyntaxNode
{
    protected readonly List<T> children = [];
    public T[] Children => [.. children];

    public void AppendChild(T node)
    {
        children.Add(node);
        node.Parent = this;
    }
    public void AppendChildren(params T[] nodes)
    {
        foreach (var i in nodes)
        {
            children.Add(i);
            i.Parent = this;
        }
    }

    public void RemoveChild(T node) => children.Remove(node);

    public IEnumerator<T> GetEnumerator() => children.GetEnumerator();
    IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();

    public override string ToString()
    {
        if (children.Count == 0) return "{}";

        StringBuilder str = new();

        str.AppendLine("{");

        foreach (var i in children)
        {
            var l = (i.AttributesToString() + i.ToString())
                .Replace("\r\n", "\n")
                .Split('\n', StringSplitOptions.RemoveEmptyEntries);

            foreach (var j in l) str.AppendLine($"\t{j}");
        }

        str.AppendLine("}");

        return str.ToString();
    }
}

/// <summary>
/// The base for all instructional and executeable syntax nodes
/// </summary>
public abstract class InstructionalNode : SyntaxNode
{

}
/// <summary>
/// The base for all language statements
/// </summary>
public abstract class StatementNode : InstructionalNode
{

}

/// <summary>
/// The base for all expressions.
/// Expresions usually return values
/// </summary>
public abstract class ExpressionNode : StatementNode
{
    public ISymbol ExpressionType { get; set; } = null!;
}

/// <summary>
/// The base for all values.
/// Values aways returns something.
/// values are:
///  - function calls;
///  - fields reference;
///  - numeric or textual literals.
/// </summary>
public abstract class ValueExpressionNode : ExpressionNode
{

}
/// <summary>
/// The base for all literal collection values (arrays and shit like that)
/// </summary>
/// <typeparam name="T"> The kind of expressionNodes that can be used </typeparam>
[Obsolete("i'm planning to change this shit, too much limitated")]
public abstract class ValueCollectionNode<T> : ValueExpressionNode, IEnumerable<T> where T : ExpressionNode
{
    protected readonly List<T> children = [];
    public T[] Children => [.. children];

    public void AppendChild(T node)
    {
        children.Add(node);
        node.Parent = this;
    }
    public void AppendChildren(params T[] nodes)
    {
        foreach (var i in nodes)
        {
            children.Add(i);
            i.Parent = this;
        }
    }

    public IEnumerator<T> GetEnumerator() => children.GetEnumerator();
    IEnumerator IEnumerable.GetEnumerator() => GetEnumerator();
}

/// <summary>
/// The base for all type references and modifiers
/// </summary>
public abstract class TypeNode : SyntaxNode { }