using Abstract.Core.Src;
using Abstract.Parsing;
using static Abstract.Core.Builder.BuildNamespace;
using System;
using Abstract.Build;
using Abstract.Build.BuildArtifacts;
using System.Text.RegularExpressions;

namespace Abstract.Build.BuildSteps;

public abstract class Step(BuildContext ctx, string name) : IStep
{
    public readonly BuildContext ctx = ctx;

    private readonly Progress _progress = new(name);
    public Progress Progress => _progress;
    public string Name => _progress.Name;

    private readonly List<Step> _dependences = [];
    public Step[] Dependences => [.. _dependences];
    public bool HasDependences => _dependences.Count > 0 && !_dependences.Any(e => !e._progress.IsComplete());
    private bool _complete = false;
    public bool Complete => _complete;

    public void DependsOn(Step step) => _dependences.Add(step);

    public void Run()
    {
        // Solve dependences
        var depQueue = new Queue<Step>(_dependences);
        while (depQueue.Count > 0)
        {
            var s = depQueue.Dequeue();
            if (s.Complete) continue;
            _progress.Append(s._progress);
            s.Run();
        }
        // Run itself
        Make();

        Progress.Done();
    }
    protected abstract void Make();
}

public class StepNode(BuildContext ctx, string name) : Step(ctx, name)
{
    protected override void Make()
    {

    }
}

public partial class BuildExecutableStep(BuildContext ctx, Executable exe) : Step(ctx, "Building executable")
{
    private Executable _exeRef = exe;
    protected override void Make()
    {
        Builder.console.WriteLog("Scanning project...");
        List<Script> scripts = [];
        int ignored = 0;

        var root = Path.GetFullPath("./");

        Queue<string> dirs = new([root]);
        while (dirs.Count > 0)
        {
            var curdir = dirs.Dequeue();
            foreach (var i in Directory.GetFiles(curdir))
            {
                if (Path.GetExtension(i) == ".a") scripts.Add(new Script(root, i));
                else ignored++;
            }
            foreach (var i in Directory.GetDirectories(curdir))
            {
                var a = new DirectoryInfo(i).Name;
                if (IdentifierPattern().IsMatch(a)) dirs.Enqueue(i);
            }
        }

        Builder.console.WriteSuccess($"{scripts.Count} scripts found; {ignored} files ignored");
        Builder.console.WriteLog("Parsing files...");

        // scripts modification time should be compared with
        // a cached table. Only modified scripts should be
        // parsed and analyzed

        Progress[] ready = new Progress[scripts.Count];
        for (var i = 0; i < scripts.Count; i++)
        {
            ready[i] = Progress.Branch($"Parsing {scripts[i]}");
            var p = ready[i]; var s = scripts[i];
            Task.Run(() => ParseScriptAsync(p, s));
        }

        // Hold on until all individual scripts are processed
        while (ready.Any(e => !e.IsSelfDone())) ;

    }

    private void ParseScriptAsync(Progress progress, Script s)
    {
        // Every file should be it own tree.
        // Each tree will be semantically analyzed
        // and output a little piece of the program
        // that can be reused inf future builds
        // (incremental compilation)

        var scriptHash = SimpleHash(s.FilePath);

        var tokens = Lexer.LexText(ctx, scriptHash, s.Read(), true);
        var tree = Parser.BuildTree(ctx, scriptHash, tokens);
        Analyzer.ShallowAnalyze(ctx, scriptHash, tree);

        progress.Done();
    }

    [GeneratedRegex("^[a-zA-Z_][a-zA-Z_]*$")] private static partial Regex IdentifierPattern();
    static ulong SimpleHash(string input)
    {
        ulong hash = 14695981039346656037;
        foreach (char c in input)
        {
            hash ^= (byte)c;
            hash *= 1099511628211;
        }
        return hash;
    }
}

public class InstallArtifactStep : Step
{
    private Executable _artifact;

    public InstallArtifactStep(BuildContext ctx, Executable artifact) : base(ctx, "Installing Artifact")
    {
        _artifact = artifact;
        DependsOn(artifact.Step);
    }

    protected override void Make()
    {

    }
}
