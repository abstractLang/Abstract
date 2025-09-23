using System.Diagnostics;
using System.Runtime.InteropServices;
using System.Text;

namespace Abstract.Cli.Build;

public class BuildOptions(string projectName)
{

    public readonly string ProjectName = projectName;
    
    public string DirectoryQueryRegex = "^[a-zA-Z_][a-zA-Z0-9_]*$";
    public string ScriptQueryRegex = "^[A-Za-z_][A-Za-z0-9_]*(?:\\.(?:a|abs))?$";
    
    // Debug options
    public bool Verbose = false;
    public bool DebugDumpParsedTrees = false;
    public bool DebugDumpAnalyzerIr = false;
    public bool DebugDumpCompressedModules = false;
    
    private readonly Dictionary<string, string> _modules = [];
    public (string name, string path)[] Modules => _modules.Select(x => (x.Key, x.Value)).ToArray();


    public void AppendModule(string name, string path)
    {
        var post = path.Replace("res://", Path.GetDirectoryName(Environment.ProcessPath) + '/');
        var rooted = Path.GetFullPath(post).TrimEnd(Path.DirectorySeparatorChar);
        
        if (_modules.ContainsKey(name)) throw new Exception($"module '{name}' already exists");
        if (_modules.ContainsValue(rooted)) throw new Exception($"path '{path}' already included");
        if (!Directory.Exists(rooted)) throw new Exception($"path '{rooted}' is not a valid directory");
        
        _modules.Add(name, rooted);
    }


    public override string ToString()
    {
        var sb = new StringBuilder();

        sb.AppendLine("{");
        sb.AppendLine($"[Project]={ProjectName}");
        sb.AppendLine($"[Verbose]={Verbose}");
        
        sb.AppendLine("[Modules]={");
        foreach (var i in _modules)
        {
            sb.AppendLine($"  {i.Key} - {i.Value}");
        }
        sb.AppendLine("  }");
        
        sb.AppendLine($"[QueryRegex.Directory]='{DirectoryQueryRegex}'");
        sb.AppendLine($"[QueryRegex.Scripts]='{ScriptQueryRegex}'");
        
        sb.AppendLine("}");
        
        return sb.ToString();
    }
}
