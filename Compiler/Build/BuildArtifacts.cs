using Abstract.Build.BuildSteps;
using Abstract.Core.Src;
using Abstract.Parsing;
using static Abstract.Core.Builder.BuildNamespace;

namespace Abstract.Build.BuildArtifacts;

public class Executable : IExecutable
{

    private BuildContext _ctx;
    private string _name;
    private string _rootDir;
    private BuildExecutableStep _step;
    
    public Step Step => _step;

    public Executable(BuildContext ctx, string name, string rootDirectory)
    {
        _ctx = ctx;
        _name = name;
        _rootDir = rootDirectory;
        _step = new(ctx, this);
    }
}