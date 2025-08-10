using System.Text.RegularExpressions;
using Abstract.Compiler.Build.Core;
using Abstract.Compiler.Build.Core.Resources;
using Abstract.Compiler.Parser;
using Abstract.Compiler.Parser.Core.Nodes.Control;

namespace Abstract.Compiler.Build;

public static class Builder
{

    private static RegexOptions _regexOptions = RegexOptions.Singleline;
    private static TimeSpan _regexTimeout = TimeSpan.FromMilliseconds(250);
    
    /// <summary>
    /// Executes a building routine using a `BuildOptions` structure.
    /// It returns if succeeded, or returns an exception if the build fails.
    /// </summary>
    /// <param name="options">The options for the build</param>
    public static void Build(BuildOptions options)
    {
        var verbose = options.Verbose;
        
        if (verbose) Console.WriteLine($"build_options = {options}");
        
        // Abstract uses directories as namespaces.
        // We will, firstly, list all the module's
        // file tree.
        if (verbose) Console.WriteLine($"\n # Searching sources\n");

        Dictionary<string, DirNode> modules = [];
        foreach (var m in options.Modules)
        {
            if (options.Verbose) Console.WriteLine($"Querying for module '{m.name}' in path {m.path}");

            var moduleRoot = new DirNode(m.name);
            modules.Add(m.name, moduleRoot);
            
            Queue<(DirNode parent, string path)> toIterate = [];
            toIterate.Enqueue((moduleRoot, m.path));
            
            while (toIterate.Count > 0)
            {
                var (curr_parent, curr_path) = toIterate.Dequeue();
                
                var files = Directory.GetFiles(curr_path);

                foreach (var file in files)
                {
                    if (!Regex.IsMatch(file, options.ScriptQueryRegex, _regexOptions, _regexTimeout))
                        continue;
                    
                    var nscript = new Script(file);
                    curr_parent.AppendScript(nscript);
                    if (options.Verbose) Console.WriteLine($"\tfound {file}");
                }
                
                var subdirs = Directory.GetDirectories(curr_path);

                foreach (var dir in subdirs)
                {
                    if (!Regex.IsMatch(dir, options.DirectoryQueryRegex, _regexOptions, _regexTimeout))
                        continue;
                    
                    var ndir = new DirNode(dir);
                    curr_parent.AppendSubdirectory(ndir);
                }
            }
        }
        
        // Breaking down tree into list of namespaces
        if (verbose) Console.WriteLine($"\n # Breaking down tree\n");

        foreach (var m in modules)
        {

            var module = new ModuleNode(m.Key);

            Queue<(NamespaceNode, DirNode)> toIterate = [];
            toIterate.Enqueue((module, m.Value));

            while (toIterate.Count > 0)
            {
                var (
                    currNamespace,
                    currDirnode
                    ) = toIterate.Dequeue();

                foreach (var dir in currDirnode.SubNodes)
                {
                    var nmsp = new NamespaceNode(dir.DirName);
                    toIterate.Enqueue((nmsp, dir));
                }
                
                foreach (var script in currDirnode.Scripts)
                {
                    if (verbose) {
                        var a = string.Join('.',[.. currNamespace.FullName, script.FileName]);
                        Console.Write($"Tokenizing {a}");
                    }
                    var tokens = Tokenizer.ParseScript(script);
                    if (verbose) Console.WriteLine($" ({tokens.Length} tokens)");

                    foreach (var i in tokens)
                    {
                        Console.WriteLine($"\t{i.type}\t\t{i.value}");
                    }
                }


            }
            
        }
        
    }
    
}
