using System.Diagnostics;
using Abstract.Build.Core.Exceptions;
using Abstract.Build.Core.Sources;
using Abstract.CodeProcess;
using Abstract.CompileTarget.Core;
using Abstract.Parser;
using Abstract.Parser.Core.Language.SyntaxNodes.Control;

namespace Abstract.Build;

public static class Builder
{

    public static Dictionary<string, CompileTargetExtension> _compileTargets = [];


    public static void Execute(BuildOptions buildOps) => Environment.Exit(__Execute(buildOps));

    private static int __Execute(BuildOptions buildOps)
    {
        Stopwatch entireBuild = new();
        Stopwatch singleStep = new();

        Console.WriteLine("Initializing build proccess...");
        entireBuild.Start();
        singleStep.Start();

        var errorHandler = new ErrorHandler();

        int err = ValidadeBuildOptions(buildOps, errorHandler);
        if (err != 0) return err;

        // Creating the out folder...
        if (!Directory.Exists(buildOps.OutputDirectory))
            Directory.CreateDirectory(buildOps.OutputDirectory);

        // Creating the debug out folder...
        if (buildOps.DebugOutDirectory != null && !Directory.Exists(buildOps.DebugOutDirectory))
            Directory.CreateDirectory(buildOps.DebugOutDirectory);
        
        Dictionary<string, List<Script>> projects = [];

        // generating script objects...
        foreach (var i in buildOps.InputedFilesPath)
        {
            projects.Add(i.Key, []);
            foreach (var j in i.Value)
                projects[i.Key].Add(new UserScript(j));
        }
        // appending dependences
        // scripts.Add(new BuildinLibrary("Std"));

        // TODO optimize memory on this pipeline

        ProgramNode program = new()
        {
            outDirectory = buildOps.OutputDirectory,
            debugOutDirectory = buildOps.DebugOutDirectory
        };

        var lexer = new Lexer();
        var parser = new SyntaxTreeBuilder(errorHandler);

        foreach (var project in projects)
        {
            var projectNode = new ProjectNode(project.Key);
            foreach (var script in project.Value)
            {
                var tokens = lexer.Parse(script);
                projectNode.AppendChild(parser.ParseScriptTokens(script, tokens));
            }
            program.AppendChild(projectNode);
        }

        Console.WriteLine($"Parsing Finished. ({singleStep.Elapsed})");
        Console.WriteLine("Starting evaluation proccess...");
        singleStep.Restart();

        var eval = new Evaluator(errorHandler);
        var progroot = eval.EvaluateProgram(program);

        Console.WriteLine($"Evaluation finished. ({singleStep.Elapsed})");

        // DEBUG
        if (buildOps.DebugOutDirectory != null) File.WriteAllText($"{buildOps.DebugOutDirectory}/tree.txt", program.ToTree());
        if (buildOps.DebugOutDirectory != null) File.WriteAllText($"{buildOps.DebugOutDirectory}/program.txt", program.ToFancyString());

        if (errorHandler.ErrorCount == 0)
        {

            Console.WriteLine("Starting compression proccess...");
            singleStep.Restart();

            var compressor = new Compressor(errorHandler);
            var elfprograms = compressor.DoCompression(progroot);

            if (buildOps.DebugOutDirectory != null)
            {
                foreach (var i in elfprograms)
                    File.WriteAllText($"{buildOps.DebugOutDirectory}/{i.Name}.txt", i.ToString());
            }

            Console.WriteLine($"Compression finished. ({singleStep.Elapsed})");

            Console.WriteLine($"Starting compilation process... (target: {buildOps.Target})");
            singleStep.Restart();

            var target = _compileTargets[buildOps.Target!];
            var compiler = target.CompilerInstance;

            // Skipping std for now as it implementation
            // is target-dependant and the elf is just a
            // interface.

            var path = buildOps.OutputDirectory;
            var elfs = elfprograms.Where(e => e.Name != "Std").ToArray();

            compiler.Compile(path, elfs);

            compiler.Dispose();

            Console.WriteLine($"Compilation finished. ({singleStep.Elapsed})");

            Console.WriteLine($"Build finished! ({entireBuild.Elapsed})");
        }
        else
        {
            Console.WriteLine($"Build failed! ({entireBuild.Elapsed})");
            errorHandler.Dump();
        }

        return errorHandler.ErrorCount;
    }

    private static int ValidadeBuildOptions(BuildOptions buildOps, ErrorHandler err)
    {
        foreach (var (project, scripts) in buildOps.InputedFilesPath)
        {
            foreach (var j in scripts)
            {
                try { if (!File.Exists(j)) throw new ScriptNotFoundException(j); }
                catch (ScriptNotFoundException ex) { err.RegisterError(null!, ex); }
            }
        }

        if (err.ErrorCount > 0)
        {
            err.Dump();
            return 1;
        }
        else return 0;
    }

}
