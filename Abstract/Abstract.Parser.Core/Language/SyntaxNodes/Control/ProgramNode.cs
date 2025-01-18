using Abstract.Build.Core.Sources;
using Abstract.Build;
using System.Text;

namespace Abstract.Parser.Core.Language.SyntaxNodes.Control;

public class ProgramNode : ControlNode
{
    public string? outDirectory = null;
    public string? debugOutDirectory = null;

    public override Script GetSourceScript() => null!;

    public override string ToFancyString()
    {
        StringBuilder str = new();

        str.AppendLine($"PROGRAM NODE");
        str.AppendLine($"out directory: {outDirectory}");
        str.AppendLine();

        foreach (var i in Children)
        {
            var proj = (i as ProjectNode)!;

            str.AppendLine($"PROJECT {proj.projectName} {{");

            try
            {

                var lines = proj.ToFancyString().Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries);
                foreach (var line in lines) str.AppendLine($"\t{line}");

            }
            catch (Exception ex) { str.AppendLine(ex.ToString()); }

            str.AppendLine("}");
        }

        return str.ToString();
    }
}
