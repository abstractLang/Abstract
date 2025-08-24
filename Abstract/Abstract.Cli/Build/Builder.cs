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
        // The best is make sure that all the build cache directories
        // are in the right place
        SetupBuildCache();
        
        var verbose = options.Verbose;
        
        var err = new ErrorHandler();
        var lexer = new Lexer();
        var parser = new Parser(err);
        var analizer = new Analizer(err);
        
        if (verbose) Console.WriteLine("Starting build...");
        var completeBuild = Stopwatch.StartNew();

        var parsingModules = Stopwatch.StartNew();
        
        List<Module> modules = [];
        foreach (var mod in options.Modules)
        {
            var module = new Module(mod.name);
            modules.Add(module);
            var mod_path = mod.path;

            if (verbose) Console.WriteLine($"# Processing module '{module.name}':");
            if (verbose) Console.Write("\tSearching for files... ");
            var singleModule = Stopwatch.StartNew();
            
            var nodes = SearchSourceFiles(
                mod_path,
                options.DirectoryQueryRegex,
                options.ScriptQueryRegex);
            
            if (verbose) Console.WriteLine($"Done ({singleModule.Elapsed})");
            if (verbose) Console.Write($"\tProcessing {nodes.Length} namespaces... ");
            singleModule.Restart();
            
            foreach (var (dir, scripts) in nodes)
            {
                var namespaceName = dir[mod_path.Length..]
                    .Trim(Path.DirectorySeparatorChar)
                    .Replace(Path.DirectorySeparatorChar, '.');

                var namespaceNode = module.AddNamespace(namespaceName);
                
                foreach (var i in scripts)
                {
                    var fileContent = File.ReadAllText(i);
                    var lex = lexer.Lex(fileContent);
                    var tree = parser.Parse(lex);

                    namespaceNode.AddTree(tree);
                }
            }
            
            if (verbose) Console.WriteLine($"Done ({singleModule.Elapsed})");
        }
        
        parsingModules.Stop();
        if (verbose) Console.WriteLine($"Modules parsed ({parsingModules.Elapsed})");
        
        analizer.Analize([.. modules]);
        
        completeBuild.Stop();
        if (verbose) Console.WriteLine($"Build Finished ({completeBuild.Elapsed})");

    }

    private static void SetupBuildCache()
    {
        string[] directories = [
            ".abs-cache",
            ".abs-cache/debug",
            ".abs-cache/modules",
        ];

        foreach (var i in directories)
            if (!Directory.Exists(i)) Directory.CreateDirectory(i);
    }
    
    private static (string, string[])[] SearchSourceFiles(
        string moduleDirectory,
        string directorySearchPattern,
        string fileSearchPattern
    )
    {
        List<(string, string[])> scripts = [];

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
            var match = from f in files
                where Regex.IsMatch(f, fileSearchPattern, _regexOptions, _regexTimeout)
                select f;
            
            scripts.AddRange((parent, files));
        }
        
        return [.. scripts];
    }
    
}