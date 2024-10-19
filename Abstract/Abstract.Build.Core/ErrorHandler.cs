using Abstract.Build.Core.Exceptions;
using Abstract.Build.Core.Sources;
using System.Text;

namespace Abstract.Build;

public class ErrorHandler
{

    private List<BuildException> _generalErrors = [];
    private Dictionary<Script, List<BuildException>> _scriptErrors = [];
    public int ErrorCount { get; private set; } = 0;

    public void RegisterError(Script script, BuildException ex)
    {
        if (script != null)
        {
            if (_scriptErrors.TryGetValue(script, out List<BuildException>? value)) value.Add(ex);
            else _scriptErrors.Add(script, [ex]);
        }
        else _generalErrors.Add(ex);

        ErrorCount++;
    }


    public void Dump()
    {
        var s = new StringBuilder();

        s.AppendLine($"(/) {ErrorCount} errors:");

        foreach (var e in _generalErrors)
            s.AppendLine(e.Message);

        foreach (var e in _scriptErrors)
        {
            s.AppendLine($"\"{e.Key.Directory}\" ({e.Value.Count} errors):");
            foreach (var i in e.Value)
            {
                if (i is CompilerException @comp)
                {
                    var pos = GetLineAndCollumn(e.Key, comp.Range.start);
                    s.AppendLine($"\t - {i.Message} ({pos.line}:{pos.collumn});");
                }
                else
                    s.AppendLine($"\t - {i.Message};");
            }
        }

        Console.WriteLine(s.ToString());
    }

    private (long line, long collumn) GetLineAndCollumn(Script src, long length)
    {
        long line = 0;
        long collumn = 0;

        var srcContent = src.GetContent();
        for (var i = 0; i < length; i++)
        {
            if (srcContent[i] == '\n')
            {
                line++;
                collumn = 0;
            }
            else collumn++;
        }

        return (++line, ++collumn);
    }
}

