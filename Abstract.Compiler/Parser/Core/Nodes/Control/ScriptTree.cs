using Abstract.Compiler.Build.Core.Resources;

namespace Abstract.Compiler.Parser.Core.Nodes.Control;

public class ScriptTree(Script src) : SyntaxNode
{
    public readonly Script source = src;
}
