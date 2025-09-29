using System.Diagnostics;
using System.Text.RegularExpressions;
using Abstract.CodeProcess;
using Abstract.CodeProcess.Core;
using Abstract.Module.Core.Configuration;
using Abstract.Realizer;
using LangModule = Abstract.CodeProcess.Core.Language.Module;

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

        ModuleLanguageTargetConfiguration target;
        {
            ModuleLanguageTargetConfiguration? found = null;
            foreach (var m in Program.modules)
            {
                found = m.Config.Targets
                    .OfType<ModuleLanguageTargetConfiguration?>()
                    .FirstOrDefault(e => e.Value.TargetIdentifier == options.TargetQuery);
                if (found.HasValue) break;
            }
            if (!found.HasValue) throw new Exception($"Target '{options.TargetQuery}' not found");
            target = found.Value;
        }


        var err = new ErrorHandler();
        
        var lexer = new Lexer();
        var parser = new Parser(err);
        var analizer = new Analizer(err);
        var compressor = new Compressor();
        
        
        if (verbose) Console.WriteLine("Starting build...");
        var completeBuild = Stopwatch.StartNew();

        var parsingModules = Stopwatch.StartNew();
        
        List<LangModule> modules = [];
        foreach (var mod in options.Modules)
        {
            var module = new LangModule(mod.name);
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
                    err.SetFile(i);
                    var fileContent = File.ReadAllText(i);
                    var lex = lexer.Lex(fileContent);
                    var tree = parser.Parse(lex);
                    
                    namespaceNode.AddTree(tree);
                }
                
                if (options.DebugDumpParsedTrees)
                {
                    List<string> nmsp = [ ];
                    if (!string.IsNullOrEmpty(module.name)) nmsp.AddRange(module.name.Split('.'));
                    if (!string.IsNullOrEmpty(namespaceName)) nmsp.AddRange(namespaceName.Split('.'));
                        
                    File.WriteAllText(
                        $"./.abs-cache/debug/{string.Join('.', nmsp)}.generated.a",
                        namespaceNode.ContentToString());
                }

            }
            
            if (verbose) Console.WriteLine($"Done ({singleModule.Elapsed})");
        }
        err.SetFile(null);
        
        parsingModules.Stop();
        if (verbose) Console.WriteLine($"Modules parsed ({parsingModules.Elapsed})");

        if (err.ErrorCount > 0)
        {
            err.Dump();
            Environment.Exit(1);
        }
        
        var analysis = Stopwatch.StartNew();
        var progObj = analizer.Analize(
            [.. modules],
            dumpGlobalTable: options.DebugDumpAnalyzerIr,
            dumpEvaluatedData: options.DebugDumpAnalyzerIr);
        
        analysis.Stop();
        if (verbose) Console.WriteLine($"Analysis done ({analysis.Elapsed})");
        
        var compress = Stopwatch.StartNew();
        var program = compressor.CompressProgramObject(progObj);
        
        compress.Stop();
        if (verbose) Console.WriteLine($"Compression done ({compress.Elapsed})");

        var intermediateCompilation = Stopwatch.StartNew();

        var rproc = new RealizerProcessor()
        {
            DebugDumpPath = ".abs-cache/debug/realizer",
            Verbose = verbose
        };
        
        rproc.SelectProgram(program);
        rproc.SelectConfiguration(target.LanguageOutput);
        
        rproc.Start();
        var result = rproc.Compile();
        
        intermediateCompilation.Stop();
        Console.WriteLine($"Intermediate compilation done ({intermediateCompilation.Elapsed})");
        
        var binaryEmmission = Stopwatch.StartNew();
        
        target.CompilerInvoke(result);
        
        binaryEmmission.Stop();
        Console.WriteLine($"Binary emmission done ({binaryEmmission.Elapsed})");
        
        completeBuild.Stop();
        if (verbose) Console.WriteLine($"Build Finished ({completeBuild.Elapsed})");

    }

    private static void SetupBuildCache()
    {
        string[] directories = [
            ".abs-out",
            
            ".abs-cache",
            ".abs-cache/debug",
            ".abs-cache/debug/realizer",
            ".abs-cache/temp",
            ".abs-cache/modules",
        ];
        string[] reset = [
            ".abs-cache/debug",
            ".abs-cache/temp",
        ];

        foreach (var i in reset)
            if (Directory.Exists(i)) Directory.Delete(i, true);
        
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
                var i = Path.GetFileName(d);
                if (Regex.IsMatch(i, directorySearchPattern, _regexOptions, _regexTimeout))
                    queue.Enqueue(d);
            }
            
            var files = Directory.GetFiles(parent);
            var match = from f in files
                let i = Path.GetFileName(f)
                where Regex.IsMatch(i, fileSearchPattern, _regexOptions, _regexTimeout)
                select f;
            
            scripts.AddRange((parent, files));
        }
        
        return [.. scripts];
    }
    
}