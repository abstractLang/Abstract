using static Abstract.Build.Builder;

namespace Abstract.Build;

public partial class Builder
{

    // TODO build system is complex,
    // remember to document it better

    // No return wrapper
    public static void Build() => Environment.Exit(__build__());

    public static ConsoleWrapper console = null!;

    private static int __build__()
    {
        var ctx = new BuildContext();
        console = new ConsoleWrapper(ctx.DefaultInstallStep.Progress);

        console.Reset();
        console.Start();

        var cacheDir = Path.Combine(Directory.GetCurrentDirectory(), ".abs-cache");
        var cacheDebugDir = Path.Combine(cacheDir, "debug");
        var cacheModulesDir = Path.Combine(cacheDir, "modules");
        var cacheDependencesDir = Path.Combine(cacheDir, "dependences");

        // Check if the cache directory is already created and create it if not
        if (!Directory.Exists(cacheDir)) Directory.CreateDirectory(cacheDir);
        if (!Directory.Exists(cacheDebugDir)) Directory.CreateDirectory(cacheDebugDir);
        if (!Directory.Exists(cacheModulesDir)) Directory.CreateDirectory(cacheModulesDir);
        if (!Directory.Exists(cacheDependencesDir)) Directory.CreateDirectory(cacheDependencesDir);

        ctx.cacheDir = cacheDir;

        DefaultBuildScript(ctx);
        ctx.DefaultInstallStep.Run();

        console.Stop();
        return 0;
    }

    private static void DefaultBuildScript(BuildContext b)
    {
        var exe = b.CreateExecutable(
            name: "my-program",
            rootDirectory: "test-code/"
        );
        var install = b.AddInstallArtifact(exe);

        b.DefaultInstallStep = b.AddStepNode("Build");
        b.DefaultInstallStep.DependsOn(install);
    }

}
