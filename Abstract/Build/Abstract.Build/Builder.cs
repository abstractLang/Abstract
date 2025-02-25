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
        var progress = new Progress("Building project");
        console = new ConsoleWrapper(progress);
        console.Reset();
        console.Start();

        var ctx = new BuilderContext();
        DefaultBuildScript(ctx);

        Queue<Step> waitingSteps = [];
        Queue<Step> next = [];
        Step current = ctx.GetInstallStep();

        while (waitingSteps.Count > 0)
        {
            if (current.HaveDependences)
            {
                waitingSteps.Enqueue(current);
                foreach (var i in current.Dependences)
                    next.Enqueue(i);
            }
        }

        console.Stop();
        return 0;
    }

    private static void DefaultBuildScript(BuilderContext b)
    {
        var exe = CreateExecutable(
            name: "test-prg",
            rootDirectory: "test-code/"
        );
        
        b.GetInstallStep().DependsOn(exe.Step);

    }

}
