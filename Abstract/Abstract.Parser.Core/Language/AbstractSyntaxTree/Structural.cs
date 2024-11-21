using Abstract.Build.Core.Sources;
using System.Text;

namespace Abstract.Parser.Core.Language.AbstractSyntaxTree;

/// <summary>
/// Represents a instance of a compilated program
/// </summary>
/// <param name="programName"> The name of the progrma being compilated </param>
public class ProgramRoot(string programName) : ControlScopeNode<ControlNode>, IReferenceable
{
    public MasterSymbol Symbol { get; set; } = new([programName], null!);
    public TypeNode ReturnType { get => null!; set { } }
    public new IReferenceable? Parent { get => null!; set { } }

    public readonly Guid ProgramId = Guid.NewGuid();

    public ReferenceSymbol GetGlobalSymbol() => new(Symbol.Tokens, Symbol);
    public override string ToString()
    {
        var str = new StringBuilder();

        str.AppendLine($"Program: {Symbol}");
        str.AppendLine($"GUID: {ProgramId}");

        str.Append(base.ToString());

        return str.ToString();
    }
}
/// <summary>
/// Represents a instance of a script included on the compilation
/// </summary>
/// <param name="src"> The script reference </param>
public class ScriptRoot(Script src) : ControlScopeNode<ControlNode>, IReferenceable
{
    private Script _sourceScript = src;

    public override Script SourceScript => _sourceScript;
    public string ScriptName => _sourceScript.Name;

    public new IReferenceable? Parent { get => base.Parent as IReferenceable; set => base.Parent = (SyntaxNode)value!; }
    public MasterSymbol Symbol { get => Parent?.Symbol!; set { } }
    public TypeNode ReturnType { get => Parent!.ReturnType; set => Parent!.ReturnType = value; }

    public override string ToString() => $"{SourceScript.GetType().Name} \"{ScriptName}\" {base.ToString()}";

    ReferenceSymbol IReferenceable.GetGlobalSymbol() => Parent!.GetGlobalSymbol();
}
/// <summary>
/// Represents a level of scoped itens, with a identification name
/// </summary>
public class NamespaceNode : ControlScopeNode<ControlNode>, IReferenceable
{
    public new IReferenceable? Parent { get => base.Parent as IReferenceable; set => base.Parent = (SyntaxNode)value!; }
    public MasterSymbol Symbol { get; set; } = null!;
    public TypeNode ReturnType { get => throw new NotImplementedException(); set => throw new NotImplementedException(); }


    public ReferenceSymbol GetGlobalSymbol() => new((string[])[.. Parent?.GetGlobalSymbol().Tokens ?? [], .. Symbol.Tokens], Symbol);
    public override string ToString() => $"namespace {Symbol} {base.ToString()}";
}
/// <summary>
/// Represents the declaration and implementation of a function or subroutine
/// </summary>
public class FunctionNode : ControlScopeNode<InstructionalNode>, IReferenceable
{
    public new IReferenceable? Parent { get => base.Parent as IReferenceable; set => base.Parent = (SyntaxNode)value!; }
    public MasterSymbol Symbol { get; set; } = null!;
    public ParameterCollectionNode Parameters { get; set; } = null!;
    public TypeNode ReturnType { get; set; } = null!;


    public ReferenceSymbol GetGlobalSymbol() => new((string[])[.. Parent?.GetGlobalSymbol().Tokens ?? [], .. Symbol.Tokens], Symbol);
    public override string ToString() => $"func {ReturnType} {Symbol} {Parameters} {base.ToString()}";
}
/// <summary>
/// Represents the declaration and implementation of a data structure
/// </summary>
public class StructureNode : ControlScopeNode<ControlNode>, IReferenceable
{
    public new IReferenceable? Parent { get => base.Parent as IReferenceable; set => base.Parent = (SyntaxNode)value!; }
    public MasterSymbol Symbol { get; set; } = null!;
    public ParameterCollectionNode? GenericParameters { get; set; } = null;
    public TypeNode ReturnType { get; set; } = null!;
    public TypeNode? ExtendsType { get; set; } = null;

    public ReferenceSymbol GetGlobalSymbol() => new((string[])[.. Parent?.GetGlobalSymbol().Tokens ?? [], .. Symbol.Tokens], Symbol);
    public override string ToString() => $"struct {Symbol} "
        + (ExtendsType != null ? $"extends {ExtendsType}" : "") + $" {base.ToString()}";
}

/// <summary>
/// Represents a attribute given to a namespace, structure or function
/// </summary>
public class AttributeNode : SyntaxNode
{
    public ISymbol Symbol { get; set; } = null!;
    public ArgumentCollectionNode Arguments { get; set; } = null!;

    public override string ToString() => $"@{Symbol}{Arguments}";
}

/// <summary>
/// Represents a collection of sequencial instructions
/// </summary>
public class CodeBlockNode : InstructionalNode
{
}
