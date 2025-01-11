using System.Collections.Immutable;
using System.Text;

namespace Abstract.Build;

public class BuildOptions
{

    private string? _target = null;
    public string? Target => _target;

    public string ProgramName = null!;

    private bool _isElfExecutable = false;
    public bool IsElfExecutable => _isElfExecutable;

    private readonly Dictionary<string, List<string>> _inputFilePaths = [];
    public Dictionary<string, List<string>> InputedFilesPath=> _inputFilePaths.ToDictionary();


    private string? _outputDirectory = null;
    private string? _outputFileName = null;

    public string OutputDirectory => _outputDirectory ?? String.Empty;
    public string OutputFileName => _outputFileName ?? String.Empty;

    public void AddInputFile(string project, string path)
    {
        string rooted = Path.GetFullPath(path);
        if (!_inputFilePaths.ContainsKey(project)) _inputFilePaths.Add(project, []);
        _inputFilePaths[project].Add(rooted);
    }

    public void SetOutput(string dir, string name)
    {
        string rooted = Path.GetFullPath(dir);
        _outputDirectory = rooted;
        _outputFileName = name;
    }

    public void SetTarget(string target) => _target = target;

    public void SetElfAsExecuteable() => _isElfExecutable = true;
    public void SetElfAsNotExecuteable() => _isElfExecutable = false;

    public override string ToString()
    {
        var writer = new StringBuilder();

        writer.AppendLine("### BuildOptions: ###");

        writer.AppendLine($"\nCompilation Target: {_target}");

        writer.AppendLine("\nOutput File:");
        writer.AppendLine($"\t\"{Path.Join(_outputDirectory, _outputFileName)}\"");

        writer.AppendLine($"\nInput Files ({_inputFilePaths.Count}):");
        foreach (var file in _inputFilePaths) writer.AppendLine($"\t- \"{file}\";");

        return writer.ToString();
    }

}
