using Abstract.Build.BuildArtifacts;
using Abstract.Build.BuildSteps;

namespace Abstract.Build;

public class BuildContext
{

    public string cacheDir = null!;

    private Step _defaultInstallStep;
    public Step DefaultInstallStep { get => _defaultInstallStep; set => _defaultInstallStep = value; }

    public BuildContext()
    {
        _defaultInstallStep = new StepNode(this, "Build");
    }

    public Executable CreateExecutable(string name, string rootDirectory) => new(this, name, rootDirectory);
    public InstallArtifactStep AddInstallArtifact(Executable artifact) => new(this, artifact);
    public StepNode AddStepNode(string name) => new(this, name);
}

