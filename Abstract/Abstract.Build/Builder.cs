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
        Console.WriteLine("Starting build process...");

        var errorHandler = new ErrorHandler();

        int err = ValidadeBuildOptions(buildOps, errorHandler);
        if (err != 0) return err;

        // Creating the out folder...
        if (!Directory.Exists(buildOps.OutputDirectory))
            Directory.CreateDirectory(buildOps.OutputDirectory);

        
        List<Script> scripts = [];

        // generating script objects...
        foreach (var i in buildOps.InputedFilesPath) scripts.Add(new UserScript(i));
        // appending dependences
        // scripts.Add(new BuildinLibrary("Std"));

        // TODO optimize memory on this pipeline

        ProgramNode program = new();
        program.outDirectory = buildOps.OutputDirectory;

        var lexer = new Lexer();
        var parser = new SyntaxTreeBuilder(errorHandler);

        foreach (var script in scripts)
        {
            var tokens = lexer.Parse(script);
            program.AppendChild(parser.ParseScriptTokens(script, tokens));
        }

        var eval = new Evaluator(errorHandler);
        eval.EvaluateProgram(program);

        File.WriteAllText($"{buildOps.OutputDirectory}/tree.txt", program.ToTree());
        File.WriteAllText($"{buildOps.OutputDirectory}/program.txt", program.ToString());

        if (errorHandler.ErrorCount > 0) errorHandler.Dump();

        return errorHandler.ErrorCount;
    }

    private static int ValidadeBuildOptions(BuildOptions buildOps, ErrorHandler err)
    {
        foreach (var i in buildOps.InputedFilesPath)
        {
            try { if (!File.Exists(i)) throw new ScriptNotFoundException(i); }
            catch (ScriptNotFoundException ex) { err.RegisterError(null!, ex); }
        }

        if (err.ErrorCount > 0)
        {
            err.Dump();
            return 1;
        }
        else return 0;
    }

}
