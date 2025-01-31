using Abstract.Build;
using Abstract.CompileTarget.Core;
using System.Reflection;

namespace Abstract.Cli;

public class Program
{
    public static int Main(string[] args)
    {
        LoadExternalResources();

#if DEBUG
        return ProcessCommand([
            "compile", "MyProgram",

            "-t", "elf",

            "-p", "Std",
                "Libs/Std/Compilation.a",
                "Libs/Std/Console.a",
                "Libs/Std/Math.a",
                "Libs/Std/Memory.a",
                "Libs/Std/Meta.a",
                "Libs/Std/Process.a",
                "Libs/Std/System.a",
                "Libs/Std/Types.a",

                "../../../../test-code/main.a",

            "-o", "../../../../test-code/bin/",
            "-d", "../../../../test-code/dbg/"
           ]);
#else
        return ProcessCommand(args);
#endif
    }

    public static void LoadExternalResources()
    {
        // Load plug-in targets
        var dirs = Directory.GetFiles("./");
        foreach (var dir in dirs)
        {
            if (dir.StartsWith("./Abstract.CompileTarget") && dir.EndsWith(".dll") && !dir.EndsWith("Core.dll"))
            {
                var assembly = Assembly.Load(File.ReadAllBytes(dir));
                var targetExtensions = assembly.GetTypes()
                    .Where(e => e.BaseType == typeof(CompileTargetExtension));
                
                foreach (var i in targetExtensions)
                {
                    CompileTargetExtension ext = (CompileTargetExtension)Activator.CreateInstance(i)!;
                    Builder._compileTargets.Add(ext.TargetID, ext);
                }

            }
        }
    }

    public static int ProcessCommand(string[] args)
    {
        if (args.Length < 1)
        {
            Help();
            return 1;
        }

        if (args[0] == "compile")
            return ExecuteCompilingProcess(args[1..], false, false, false);

        else if (args[0] == "run")
            return ExecuteCompilingProcess(args[1..], true, false, true);

        return 1;
    }

    public static int ExecuteCompilingProcess(string[] args, bool justElf, bool asBuild, bool execute)
    {
        BuildOptions buildOps = new();
        if (execute) buildOps.SetElfAsExecuteable();

        buildOps.ProgramName = args[0];

        string projectName = args[0];

        for (var i = 1; i < args.Length; i++)
        {
            if (args[i] == "-o") // output flag
            {
                if (args.Length < i + 1) return 1;

                string outputFile = args[++i];

                string dir = Path.GetDirectoryName(outputFile)!;
                buildOps.SetOutput(dir);
            }
            
            else if (args[i] == "-d") // debug output flag
            {
                if (args.Length < i + 1) return 1;

                string outputFile = args[++i];

                string dir = Path.GetDirectoryName(outputFile)!;
                buildOps.SetDebugOut(dir);
            }

            else if (args[i] == "-t") // target flag
            {
                if (args.Length < i + 1) return 1;
                buildOps.SetTarget(args[++i]);
            }

            else if (args[i] == "-p") // project flag
            {
                if (args.Length < i + 1) return 1;
                projectName = args[++i];
            }

            else buildOps.AddInputFile(projectName, args[i]);
        }


        Builder.Execute(buildOps); // no return
        return 0;
    }

    public static void Help()
    {
        Console.WriteLine("No argument provided.");
        Console.WriteLine("Try 'help' to more details.\n");

        Console.WriteLine("Compier options:");
        Console.WriteLine("\t- compile; Compile the project");
    }
}

