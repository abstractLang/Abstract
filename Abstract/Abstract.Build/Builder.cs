using Abstract.Build.Core.Exceptions;
using Abstract.Build.Core.Sources;
using Abstract.CodeProcess;
using Abstract.Parser;
using Abstract.Parser.Core.Language.AbstractSyntaxTree;

namespace Abstract.Build;

public static class Builder
{

    public static void Execute(BuildOptions buildOps) => Environment.Exit(__Execute(buildOps));

    private static int __Execute(BuildOptions buildOps)
    {
        var errorHandler = new ErrorHandler();

        ValidadeBuildOptions(buildOps, errorHandler);

        // Creating the out folder...
        if (!Directory.Exists(buildOps.OutputDirectory))
            Directory.CreateDirectory(buildOps.OutputDirectory);

        
        List<Script> scripts = [];

        // generating script objects...
        foreach (var i in buildOps.InputedFilesPath) scripts.Add(new UserScript(i));
        // appending dependences
        // scripts.Add(new BuildinLibrary("Std"));

        // TODO optimize memory on this pipeline

        List<ScriptRoot> scriptsABS = [];

        var lexer = new Lexer();
        var parser = new SyntaxTreeBuilder(errorHandler);

        foreach (var script in scripts)
        {
            var tokens = lexer.Parse(script);

            var abs = parser.Parse(script, tokens);
            scriptsABS.Add(abs);
        }

        lexer = null;
        parser = null;

        var evaluator = new Evaluator(errorHandler);
        var program = evaluator.Evaluate(buildOps.ProgramName, [.. scriptsABS]);

        var filePath = Path.Join(buildOps.OutputDirectory, "program.txt");
        File.WriteAllText(filePath, string.Join('\n', program.ToString()));

        if (errorHandler.ErrorCount > 0) errorHandler.Dump();

        return errorHandler.ErrorCount;
    }

    private static int ValidadeBuildOptions(BuildOptions buildOps, ErrorHandler err)
    {
        List<string> errors = [];

        foreach (var i in buildOps.InputedFilesPath)
        {
            try { if (!File.Exists(i)) throw new ScriptNotFoundException(i); }
            catch (ScriptNotFoundException ex) { err.RegisterError(null!, ex); }
        }

        if (errors.Count > 0)
        {
            Console.WriteLine($"/!\\ {errors.Count} errors:");
            foreach (var i in errors) Console.WriteLine($"\t{i}");
            return 1;
        }
        else return 0;
    }

}
