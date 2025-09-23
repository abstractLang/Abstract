using System.Reflection;
using System.Text;
using Abstract.Cli.Build;
using Abstract.Module.Core;

namespace Abstract.Cli;

public class Program
{
    public static readonly List<IModule> modules = [];
    
    public static int Main(string[] args)
    {
        LoadExternalResources();
        return DigestArgs(args);
    }

    private static void LoadExternalResources()
    {
        var exeDir = AppContext.BaseDirectory;
        var dllFiles = Directory.GetFiles(exeDir, "*.dll").Where(
            e => Path.GetFileName(e).StartsWith("Abstract.Module.")
                 && Path.GetFileName(e) != "Abstract.Module.Core.dll");
        
        var assemblies = dllFiles.Select(Assembly.LoadFrom).ToArray();
        var moduleTypes = assemblies.SelectMany(a => a.GetTypes())
            .Where(t => t.IsAssignableTo(typeof(IModule)))
            .ToArray();

        foreach (var m in moduleTypes)
            modules.Add((IModule)Activator.CreateInstance(m)!);
    }

    private static int DigestArgs(string[] args)
    {
        if (args.Length < 1)
        {
            Help();
            return 1;
        }

        switch (args[0])
        {
            case "build" or "b":
                DigestBuildArgs(args[1..]);
                break;
                
            case "help" or "h" or "-help" or "--help" or "-h":
                Help();
                return 0;
            
            case "list" or "l":
                return List(args[1..]);
            
            default:
                Help();
                break;
        }
        
        return 1;
    }

    private static int DigestBuildArgs(string[] args)
    {
        var buildOps = new BuildOptions(args[0]);

        var i = 1;
        while(i < args.Length)
        {
            switch (args[i++])
            {
                case "-m" or "--module" when args.Length < i + 2:
                    throw new Exception("Expected module name and path");
                case "-m" or "--module":
                    var name = args[i++];
                    var path = args[i++];
                    buildOps.AppendModule(name, path);
                    break;

                case "-v" or "--verbose":
                    buildOps.Verbose = true;
                    break;
                
                case "-d" or "--debug":
                    var options = args[i++];
                    if (options == "all")
                    {
                        buildOps.DebugDumpParsedTrees = true;
                        buildOps.DebugDumpAnalyzerIr = true;
                        buildOps.DebugDumpCompressedModules = true;
                    }

                    var optionsList = options.Split(',');
                    foreach (var option in optionsList)
                    {
                        switch (option.Trim())
                        {
                            case "parsedTrees": buildOps.DebugDumpParsedTrees = true; break;
                            case "analyzedIR": buildOps.DebugDumpAnalyzerIr = true; break;
                            case "compressedModules": buildOps.DebugDumpCompressedModules = true; break;
                        }
                    }
                        
                    break;
                
                default:
                    Console.WriteLine($"Unknown argument '{args[--i]}'");
                    i++;
                    break;
            }
        }

        Builder.Execute(buildOps);
        
        return 0;
    }

    private static int List(string[] args)
    {
        var sb = new StringBuilder();
        switch (args[0])
        {
            case "help" or "h" or "-help" or "--help" or "-h":
                Console.WriteLine(
                    "Listable options:\n" +
                    "- modules\n" +
                    "- targets");
                break;
            
            case "modules":
                Console.WriteLine("Installed modules:");
                foreach (var m in modules)
                    sb.AppendLine($"- {m.Config.Name} v.{m.Config.Version} by {m.Config.Author}");
                Console.WriteLine(sb.ToString());
                break;
            
            case "targets":
                Console.WriteLine("Installed modules:");
                foreach (var t in modules.SelectMany(m => m.Config.Targets))
                    sb.AppendLine($"- {t.TargetName} ({t.TargetIdentifier}) - {t.TargetDescription}");
                Console.WriteLine(sb.ToString());
                break;
            
            
            default:
                Console.WriteLine("Unknown command");
                return 1;
        }

        return 0;
    }
    
    private static void Help()
    {
        Console.WriteLine("No argument provided.");
        Console.WriteLine("Try 'help' to more details.\n");

        Console.WriteLine("Compier options:");
        Console.WriteLine("\t- compile           Compiles the project (bruh)");
        Console.WriteLine("\t- list              Lists information about the compiler");
    }
}

