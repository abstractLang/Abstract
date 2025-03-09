using static Abstract.Core.Builder.BuildNamespace;

namespace Abstract.Build;

public partial class Builder
{

    // TODO build system is complex,
    // remember to document it better

    // No return wrapper
    public static void Build()
        => Environment.Exit(__build__());

    public static ConsoleWrapper console = null!;

    private static int __build__()
    {
        var ctx = new BuilderContext();
        console = new ConsoleWrapper(ctx.GetInstallStep().Progress);

        console.Reset();
        console.Start();

        DefaultBuildScript(ctx);

        ctx.GetInstallStep().Run();

        console.Stop();
        return 0;
    }

    private static void DefaultBuildScript(BuilderContext b)
    {
        var exe = b.CreateExecutable(name: "my-program",
            rootDirectory: "test-code/"
        );
        var install = b.AddInstallArtifact(exe);

        b.GetInstallStep().DependsOn(install);
    }

}
