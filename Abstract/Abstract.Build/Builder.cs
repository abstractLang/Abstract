using System.Diagnostics;
using Abstract.Build.Core.Exceptions;
using Abstract.Build.Core.Sources;
using Abstract.CodeProcess;
using Abstract.Parser;
using Abstract.Parser.Core.Language.SyntaxNodes.Control;

namespace Abstract.Build;

public static class Builder
{

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

        ProgramNode program = new();
        program.outDirectory = buildOps.OutputDirectory;

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
        singleStep.Restart();

        Console.WriteLine("Starting compression proccess...");
        singleStep.Restart();

        var compressor = new Compressor(errorHandler);
        compressor.DoCompression(progroot, buildOps.OutputDirectory);

        Console.WriteLine($"Compression finished. ({singleStep.Elapsed})");

        Console.WriteLine($"Build finished! ({entireBuild.Elapsed})");

        File.WriteAllText($"{buildOps.OutputDirectory}/tree.txt", program.ToTree());
        File.WriteAllText($"{buildOps.OutputDirectory}/program.txt", program.ToFancyString());

        if (errorHandler.ErrorCount > 0) errorHandler.Dump();

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
