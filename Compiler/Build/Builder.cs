using System.Text.RegularExpressions;
using Abstract.Core.Language;
using Abstract.Core.Src;
using Abstract.Parsing;
using static Abstract.Build.Builder;

namespace Abstract.Build;

public partial class Builder
{

    // No-return wrapper
    public static void Build() => Environment.Exit(__build__());

    private static int __build__()
    {
        var ctx = new BuildContext();

        var cacheDir = Path.Combine(Directory.GetCurrentDirectory(), ".abs-cache");
        var cacheDebugDir = Path.Combine(cacheDir, "debug");
        var cacheModulesDir = Path.Combine(cacheDir, "modules");
        var cacheDependencesDir = Path.Combine(cacheDir, "dependences");

        // Check if the cache directory is already created and create it if not
        if (!Directory.Exists(cacheDir)) Directory.CreateDirectory(cacheDir);
        if (!Directory.Exists(cacheDebugDir)) Directory.CreateDirectory(cacheDebugDir);
        if (!Directory.Exists(cacheModulesDir)) Directory.CreateDirectory(cacheModulesDir);
        if (!Directory.Exists(cacheDependencesDir)) Directory.CreateDirectory(cacheDependencesDir);

        ctx.cacheDir = cacheDir;

        var cwd = Path.GetFullPath("./");
        var scripts = GetScriptsList(cwd);

        List<SyntaxTree> trees = [];

        foreach (var i in scripts) {

            var content = i.Read();
            var hash = i.GetFileHashCode();

            Console.WriteLine($"Processing file {i.FilePath} ({hash:X16})");

            // Tokenize
            var tokens = Lexer.LexText(ctx, hash, content, true);

            // Build AST
            var tree = Parser.BuildTree(ctx, hash, tokens);
            trees.Add(tree);
        }

        Console.WriteLine($"{trees.Count} trees parsed");

        foreach (var i in trees) {
            
        }

        return 0;
    }

    private static Script[] GetScriptsList(string root) {
        List<Script> scripts = [];

        Queue<string> toSearch = [];
        toSearch.Enqueue(root);

        while (toSearch.Count > 0) {
            var cur = toSearch.Dequeue();

            var files = Directory.GetFiles(cur);
            foreach (var i in files) {
                if (Path.GetExtension(i) == ".a") {
                    scripts.Add(new Script(root, i));
                }
            }

            var directories = Directory.GetDirectories(cur);
            foreach (var i in directories) {
                var a = i[Math.Max(0, i.LastIndexOf(Path.DirectorySeparatorChar) + 1) ..];
                if(Regex.IsMatch(a, "^[a-zA-Z_][a-zA-Z0-9_]*$")) toSearch.Enqueue(i);
            }

        }

        return [.. scripts];
    }

}
