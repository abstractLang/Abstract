using Abstract.Build.Core.Sources;
using Abstract.Build;

namespace Abstract.Parser.Core.Language.SyntaxNodes.Control;

public class ProgramNode : ControlNode
{
    public string? outDirectory = null;
    public string? debugOutDirectory = null;

    public override Script GetSourceScript() => null!;
}
