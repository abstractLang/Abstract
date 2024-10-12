using Abstract.Parser.Core.Sources;
using System.Collections;
using System.Numerics;
using System.Text;

namespace Abstract.Parser.Core.Language.AbstractSyntaxTree;

public abstract class SyntaxNode
{
    public Script SourceScript { get; set; } = null!;
    public SyntaxNode? Parent { get; set; }

    public override string ToString() => $"{GetType().Name}SyntaxNode";
}
public abstract class ScopeNode<T> : SyntaxNode, IEnumerable<T> where T : SyntaxNode
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

    public override string ToString()
    {
        StringBuilder str = new();

        str.AppendLine("{");

        foreach (var i in children)
        {
            var l = i.ToString().Split('\n');
            foreach (var j in l) str.AppendLine($"\t{l}");
        }

        str.AppendLine("}");

        return str.ToString();
    }
}
public interface IReferenceable
{
    public Symbol Symbol { get; set; }
    public TypeNode ReturnType { get; set; }
}

// Scope
public class ScriptNode : ScopeNode<SyntaxNode>
{
    public string ScriptName => SourceScript.Name;

    public override string ToString() => $"{SourceScript.GetType().Name} \"{ScriptName}\" {base.ToString()}";

}
public class NamespaceNode : ScopeNode<SyntaxNode>, IReferenceable
{
    public Symbol Symbol { get; set; } = null!;
    public TypeNode ReturnType { get => throw new NotImplementedException(); set => throw new NotImplementedException(); }

    public override string ToString() => $"namespace {Symbol} {base.ToString()}";
}
public class FunctionNode : ScopeNode<SyntaxNode>, IReferenceable
{
    public Symbol Symbol { get; set; } = null!;
    public TypeNode ReturnType { get => throw new NotImplementedException(); set => throw new NotImplementedException(); }

    public override string ToString() => $"func {Symbol} {base.ToString()}";
}
public class StructureNode : ScopeNode<SyntaxNode>
{
    public Symbol Symbol { get; set; } = null!;

    public override string ToString() => $"struct {Symbol} {base.ToString()}";
}

public class AttributeNode : SyntaxNode
{
    public Symbol Symbol { get; set; } = null!;

    public override string ToString() => $"@{Symbol}()";
}

// Some collections
public class ParameterCollectionNode : ScopeNode<SyntaxNode>
{
    public override string ToString() => $"({string.Join(", ", children)})";
}
public class ArgumentCollectionNode : ScopeNode<ExpressionNode>
{
    public override string ToString() => $"({string.Join(", ", children)})";
}

public class ParameterNode : SyntaxNode, IReferenceable
{

    public Symbol Symbol { get; set; } = null!;
    public TypeNode ReturnType { get => throw new NotImplementedException(); set => throw new NotImplementedException(); }
}

public class ExpressionNode : SyntaxNode
{

}

// Type nodes
public abstract class TypeNode : SyntaxNode { }
public class PrimitiveTypeNode : TypeNode { }
public class ReferenceTypeNode : TypeNode { }
public class GenericTypeNode : TypeNode { }

// Collection values
public class CollectionNode : ExpressionNode
{

}

// Literal values
public class IdentifierLiteralNode : ExpressionNode
{
    public Symbol symbol = null!;
    public override string ToString() => $"{symbol}";
}
public class NumericLiteralNode : ExpressionNode
{
    public BigInteger value;

    public override string ToString() => $"{value}";
}
public class FloatingLiteralNode : ExpressionNode
{
    public double value;

    public override string ToString() => $"{value:f}";
}
