using System.Text;
using Abstract.Src.Core;

namespace Abstract;

public static class Builder
{

    // TODO build system is complex,
    // remember to document it better

    // No return wrapper
    public static void Build()
        => Environment.Exit(__build__());

    private static ConsoleWrapper console = null!;

    private static int __build__()
    {
        var progress = new Progress("Building project");
        console = new ConsoleWrapper(progress);
        console.Reset();
        console.Start();

        var createExeNode = progress.Branch("Creating Executable");
        var exe = CreateExecutable(
            name: "test-prg",
            rootDirectory: "test-code/"
        );
        createExeNode.Done();

        var buildExeNode = progress.Branch("Building Executable");
        exe.Build();
        buildExeNode.Done();

        var installNode = progress.Branch("Installing Artifact", 999999999);
        for (var i = 0; i < 999999999; i++) installNode.CompleteOne();

        InstallArtifact(exe);
        installNode.Done();

        progress.Done();

        console.Stop();
        return 0;
    }


    // Temporary shit after here, will put it in the right place soon
    private class Progress(string name)
    {

        private string name = name;
        private int total = 1;
        private int completed = 0;
        private bool done = false;

        private Progress _parent = null!;
        private List<Progress> _children = [];

        public string Name => name;
        public int Total => total;
        public int Completed => completed;

        public Progress Branch(string name, int initialTotal = 0)
        {
            var p = new Progress(name)
            {
                _parent = this,
                total = initialTotal
            };
            _children.Add(p);
            return p;
        }
       
        public void Done()
        {
            done = true;
            _children.Clear();
        }
        public void SetCompleted(int value) => completed = value;
        public void SetTotal(int value) => total = value;
        public void CompleteOne() => completed++;

        public void End() => _parent._children.Remove(this);

        public bool IsAllDone() => _children.All(e => e.done);
        public bool IsComplete() => total == completed;

        public override string ToString()
        {
            var str = new StringBuilder();

            str.AppendLine($"{name} " + (done ? "DONE" : (total > 0 ? $"[{completed}/{total}]" : "")));
            foreach (var i in  _children.ToArray())
            {
                var l = i.ToString().Split(Environment.NewLine);
                foreach (var j in l) str.AppendLine($"\t{j}");
            }
            str.Length -= Environment.NewLine.Length;

            return str.ToString();
        }
    }
    private class ConsoleWrapper(Progress progressRoot)
    {
        private List<(char kind, string content)> _logs = [];
        private Progress _root = progressRoot;
        private int _cursorRoot = 0;

        private bool _drawing = false;

        private StringBuilder _buf = new();

        public void Reset()
        {
            _cursorRoot = Console.GetCursorPosition().Top + 1;
        }
    
        public void Start()
        {
            _drawing = true;
            Console.Write("\x1b[?25l");
            Task.Run(() => { while (_drawing) { Draw(); } });
        }
        public void Stop()
        {
            _drawing = false;
            Draw();
            Console.Write("\x1b[?25h");
            //Clean();
        }
    
        private void Draw()
        {
            _buf.Clear();
            int bl = 0;

            int cur = Console.GetCursorPosition().Top + 1;
            int width = Console.BufferWidth;

            _buf.Append($"\x1b[{_cursorRoot};0H"); // move cursor to root

            var rootLines = _root.ToString().Split(Environment.NewLine);

            _buf.AppendLine($"╔═ Build {new string('═', width - 10)}╗"); bl++;
            foreach (var i in rootLines)
            {
                var l = $"║ {i.Replace("\t", "    ").PadRight(width - 4)} ║"; bl++;
                _buf.AppendLine(l);
            }

            _buf.AppendLine($"╠═ Console {new string('═', width - 12)}╣"); bl++;

            foreach (var i in _logs.ToArray())
            {
                var mods = i.kind switch
                {
                    'e' => ("\x1b[31m", "\x1b[0m", "(/)"),
                    'w' => ("\x1b[33m", "\x1b[0m", "/!\\"),
                    's' => ("\x1b[32m", "\x1b[0m", ""),
                    _   => ("", "", "")
                };

                var staticLeft = $"│ ";
                var staticRight = $" │";
                var staticLen = staticLeft.Length + staticRight.Length + mods.Item3.Length;

                var baseContent = (i.content.Length + staticLen <= width
                    ? i.content
                    : i.content[..^(width - staticLen - 3)] + "...")
                    .PadRight(width - staticLen);

                _buf.AppendLine($"{staticLeft}{mods.Item1}{baseContent}{mods.Item3}{mods.Item2}{staticRight}"); bl++;
            }

            _buf.AppendLine($"└{new string('─', width - 2)}┘"); bl++;

            // clean trailing lines
            int point = _cursorRoot + bl;
            for (var i = point; i <= cur; i++) _buf.AppendLine(new string(' ', width));

            _buf.Length -= Environment.NewLine.Length;
            _buf.Append($"\x1b[{point - 1};0H"); // move cursor back

            Console.WriteLine(_buf.ToString());
        }


        public void WriteLog(object? value) =>     _logs.Add(('l', value?.ToString() ?? "null"));
        public void WriteErr(object? value) =>     _logs.Add(('e', value?.ToString() ?? "null"));
        public void WriteWarn(object? value) =>    _logs.Add(('w', value?.ToString() ?? "null"));
        public void WriteSuccess(object? value) => _logs.Add(('s', value?.ToString() ?? "null"));
    }

    private class Executable(string name, string path)
    {
        private string _name = name;
        private string _rootPath = path;
        private List<Script> _scripts = [];

        public void Scan()
        {
            _scripts.Clear();
            var root = Path.GetFullPath(_rootPath);

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
                    _scripts.Add(script);
                }

                foreach (var subdir in subdirs) directories.Enqueue(subdir);
            }

            console.WriteLog($"{_scripts.Count} scripts found to be parsed");
        }

        public void Build()
        {
            console.WriteErr("Build Not implemented!");
        }

        private static void ParseFile(int taskIdx, string filePath, CancellationToken cancelTkn)
        {

        }
    }

    private static Executable CreateExecutable(string name, string rootDirectory)
    {
        var newexe = new Executable(name, rootDirectory);
        newexe.Scan();
        return newexe;
    }
    private static void InstallArtifact(Executable exe)
    {
        console.WriteWarn("Installing...");
        console.WriteSuccess("Build finished!");
    }

}
