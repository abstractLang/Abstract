namespace Abstract.Parser.Core.Language.SyntaxNodes.Control;

public class ProjectNode(string projName) : ControlNode
{
    public readonly string projectName = projName;

    public override string ToString() => $"Project {projectName}";
}
