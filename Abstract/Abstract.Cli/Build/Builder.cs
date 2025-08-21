using System.Diagnostics;
using System.Text.RegularExpressions;
using Abstract.CodeProcess;
using Abstract.CodeProcess.Core;
using Module = Abstract.CodeProcess.Core.Language.Module;

namespace Abstract.Cli.Build;

public static class Builder
{

    private static RegexOptions _regexOptions = RegexOptions.Singleline;
    private static TimeSpan _regexTimeout = TimeSpan.FromMilliseconds(250);
    
    public static void Execute(BuildOptions options)
    {

        var verbose = options.Verbose;
        
        Stopwatch stopwatch = new();
        var time = stopwatch.Elapsed;

        var err = new ErrorHandler();
        var lexer = new Lexer();
        var parser = new Parser(err);
        
        if (verbose) Console.WriteLine("Starting build...");
        stopwatch.Start();
        time = stopwatch.Elapsed;

        List<Module> modules = [];
        foreach (var mod in options.Modules)
        {
            var module = new Module(mod.name);
            modules.Add(module);

            if (verbose) Console.WriteLine($"Processing module '{module.name}'...");
            if (verbose) Console.Write("Searching for files... ");
            time = stopwatch.Elapsed;
            
            var files = SearchSourceFiles(
                mod.path,
                options.DirectoryQueryRegex,
                options.ScriptQueryRegex);

            time = stopwatch.Elapsed - time;
            if (verbose) Console.WriteLine($"Done ({time})");
            
            if (verbose) Console.WriteLine($"Processing {files.Length} files ...");
            time = stopwatch.Elapsed;
            foreach (var i in files)
            {
                var fileContent = File.ReadAllText(i);
                var lex = lexer.Lex(fileContent);
                var tree = parser.Parse(lex);
                
                Console.WriteLine("#-------------------------#");
                Console.WriteLine(tree);
            }

            time = stopwatch.Elapsed - time;
            if (verbose) Console.WriteLine($"Done ({time})");
        }
        
        time = stopwatch.Elapsed;
        stopwatch.Stop();
        if (verbose) Console.WriteLine($"Build Finished ({time})");

    }

    private static string[] SearchSourceFiles(
        string moduleDirectory,
        string directorySearchPattern,
        string fileSearchPattern
    )
    {
        List<string> scripts = [];

        Queue<string> queue = new();
        queue.Enqueue(moduleDirectory);
        
        while (queue.Count > 0)
        {
            var parent = queue.Dequeue();
            
            var directories = Directory.GetDirectories(parent);
            foreach (var d in directories)
            {
                if (Regex.IsMatch(d, directorySearchPattern, _regexOptions, _regexTimeout))
                    queue.Enqueue(d);
            }
                
            var files = Directory.GetFiles(parent);
            scripts.AddRange(from f in files
                where Regex.IsMatch(f, fileSearchPattern, _regexOptions, _regexTimeout)
                select f);
        }
        
        return [.. scripts];
    }
    
}