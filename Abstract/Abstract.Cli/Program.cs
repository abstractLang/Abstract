using Abstract.Build;
using System.Reflection;

namespace Abstract.Cli;

public class Program
{
    public static int Main(string[] args)
    {
        LoadExternalResources();

#if DEBUG
        return ProcessCommand([
            "compile", "Std",

            "-t", "elf",
            //"-nostd",

            "bin/Debug/net8.0/Libs/Std/Console.ah",
            "bin/Debug/net8.0/Libs/Std/Types.ah",
            "bin/Debug/net8.0/Libs/Std/Memory.ah",
            "bin/Debug/net8.0/Libs/Std/Process.ah",
            "bin/Debug/net8.0/Libs/Std/Math.a",

            "-o","../../test-code/bin/Std.elf"
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
                var types = assembly.GetTypes();

                foreach (var t in types)
                {
                    // TODO
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

        for (var i = 1; i < args.Length; i++)
        {
            if (args[i] == "-o") // output flag
            {
                if (args.Length < i + 1) return 1;

                string outputFile = args[i + 1];

                string dir = Path.GetDirectoryName(outputFile)!;
                string name = Path.GetFileName(outputFile);

                buildOps.SetOutput(dir, name);

                i++;
            }

            else if (args[i] == "-t")
            {
                if (args.Length < i + 1) return 1;
                buildOps.SetTarget(args[i + 1]);
                i++;
            }

            else buildOps.AddInputFile(args[i]);
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

