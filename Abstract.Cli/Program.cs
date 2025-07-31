using System.Diagnostics;
using Abstract.Compiler.Build;
using Abstract.Compiler.Build.Core;

namespace Abstract.Cli;

internal static class Program
{
    public static void Main(string[] args)
    {
        try
        {
            DigestArguments(args);
        }
        catch (Exception e)
        {
            ShowHelpAndExit(e);
        }
    }

    private static void DigestArguments(string[] args)
    {
        if (args.Length == 0) throw new Exception("No arguments provided");
        switch (args[0])
        {
            case "-h" or "--help" or "-h":
                ShowHelpAndExit(null);
                break;
            
            case "build":
                if (args.Length < 4) throw new Exception("Not enough arguments provided");
                DigestBuildArgs(args[1..]);
                break;
            
            default:
                throw new Exception($"Unknown argument '{args[0]}'");
        }
        
    }

    private static void DigestBuildArgs(string[] args)
    {

        // TODO check project name
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
                
                default:
                    Console.WriteLine($"Unknown argument '{args[--i]}'");
                    i++;
                    break;
            }
        }

        try { Builder.Build(buildOps); }
        catch (Exception e)
        {
            Console.WriteLine(e);
            Environment.Exit(1);
        }

    }

    private static void ShowHelpAndExit(Exception? ex)
    {
        
        Console.WriteLine("TODO help message here");

        if (ex != null)
        {
            Console.WriteLine("Error:");
            Console.WriteLine(ex?.Message);
        }

        Environment.Exit(ex == null ? 0 : 1);
    }
}
