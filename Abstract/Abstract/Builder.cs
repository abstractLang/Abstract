using Abstract.Src.Core;

namespace Abstract;

public class Builder
{

    // TODO build system is complex,
    // remember to document it better

    // No return wrapper
    public static void Build()
        => Environment.Exit(__build__());

    private static int __build__()
    {
        var root = Path.GetFullPath("test-code/");
        Console.WriteLine($"Getting all files from {root}...");

        List<Script> scripts = [];
        Queue<string> directories = [];

        directories.Enqueue(root);

        while (directories.Count > 0)
        {
            var curr = directories.Dequeue();

            var files = Directory.EnumerateFiles(curr);
            var subdirs = Directory.EnumerateDirectories(curr);

            foreach (var i in files)
            {
                var script = new Script(root, i[root.Length..]);
                scripts.Add(script);
                Console.WriteLine($" - {script}");
            }

            foreach (var subdir in subdirs) directories.Enqueue(subdir);
        }

        Console.WriteLine($"{scripts.Count} scripts found.");


        return 0;
    }

}

// Temportary, will put it in the right place soon
class Progress
{

    private string name = null!;
    private int total = 0;
    private int completed = 0;

    public string Name => name;
    public int Total => total;
    public int Completed => completed;

    public override string ToString() => $"{name} [{completed}/{total}]";
}
