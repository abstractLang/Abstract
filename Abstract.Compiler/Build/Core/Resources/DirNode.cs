namespace Abstract.Compiler.Build.Core.Resources;

public class DirNode(string path)
{

    public readonly string dirPath = path;
    public readonly DirectoryInfo dirInfo = new DirectoryInfo(path);
    public string DirName => dirInfo.Name;

    private readonly List<DirNode> _subNodes = [];
    private readonly List<Script> _scripts = [];
    
    public DirNode[] SubNodes => _subNodes.ToArray();
    public Script[] Scripts => _scripts.ToArray();
    
    public override string ToString() => $"{DirName}/";

    public void AppendSubdirectory(DirNode child) => _subNodes.Add(child);
    public void AppendScript(Script child) => _scripts.Add(child);
}
