﻿using static Abstract.Core.Builder.BuildNamespace;

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
        var progress = new Progress("Building project");
        console = new ConsoleWrapper(progress);
        console.Reset();
        console.Start();

        var ctx = new BuilderContext();
        DefaultBuildScript(ctx);

        Queue<Step> stepQueue = new(ctx.GetInstallStep().Dependences);
        while(stepQueue.Count > 0)
        {
            var step = stepQueue.Dequeue();
            progress.Append(step.Progress);
            step.Run();
        }
        progress.Done();

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
