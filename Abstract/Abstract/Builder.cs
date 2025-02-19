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
        InstallArtifact(exe);
        for (var i = 0; i < 999999999; i++) installNode.CompleteOne();
        installNode.Done();

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

        public Progress Branch(string name, int initialTotal = 1)
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

            str.AppendLine($"{name} " + (done ? "DONE" : $"[{completed}/{total}]"));
            foreach (var i in  _children.ToArray())
            {
                var l = i.ToString().Split(Environment.NewLine);
                foreach (var j in l) str.AppendLine($"\t{j}");
            }
            str.Length--;

            return str.ToString();
        }
    }
    private class ConsoleWrapper(Progress progressRoot)
    {
        private List<string> _rawLogLines = [""];
        private Progress _root = progressRoot;
        private int _cursorRoot = 0;

        private bool _drawing = false;

        private StringBuilder _buf = new();

        public void Reset()
        {
            _cursorRoot = Console.GetCursorPosition().Top;
        }
    
        public void Start()
        {
            _drawing = true;
            Console.WriteLine("\x1b[?25l");
            Task.Run(() => { while (_drawing) { Draw(); } });
        }
        public void Stop()
        {
            _drawing = false;
            Console.WriteLine("\x1b[?25h");
            //Clean();
        }
    
        private void Draw()
        {
            // Clean
            _buf.Clear();
            int cur = Console.GetCursorPosition().Top + 1;

            _buf.Append($"\x1b[{_cursorRoot};0H");

            for (var i = _cursorRoot; i < cur; i++)
                _buf.AppendLine(new string(' ', Console.BufferWidth));

            _buf.Append($"\x1b[{_cursorRoot};0H");

            _buf.AppendLine($"{_root}");
            _buf.AppendLine($"{string.Join(Environment.NewLine, _rawLogLines)}");

            Console.WriteLine(_buf.ToString());
        }

        public void Write(object? value)
        {
            _rawLogLines[^1] += value?.ToString() ?? "";
        }
        public void WriteLine(object? value)
        {
            Write(value);
            _rawLogLines.Add("");
        }

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
        }

        public void Build()
        {
            Console.WriteLine("Build Requested!1;");
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

    }
}