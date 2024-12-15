using Abstract.Build.Core.Sources;

namespace Abstract.Parser.Core.Language.SyntaxNodes.Control;

public class ScriptReferenceNode(Script src) : ControlNode
{
    private readonly Script script = src;
    public override Script GetSourceScript() => script;
}
