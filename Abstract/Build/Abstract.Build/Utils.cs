using System.Text;
using System.Text.RegularExpressions;
using Abstract.Core.Src;
using Abstract.Parsing;
using static Abstract.Core.Builder.BuildNamespace;

namespace Abstract.Build;

public partial class Builder
{

    public class ConsoleWrapper(Progress progressRoot)
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
            Console.Write("\x1b[?25h");
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
                var temp = i.Replace("\t", "    ");
                var sanitizedSrc = Regex.Replace(temp, "\x1b\\[[0-9;]*[mK]", "");
                var l = $"║ {temp}{new string(' ', width - sanitizedSrc.Length - 4)} ║";
                _buf.AppendLine(l); bl++;
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


    public abstract class Step(string name) : IStep
    {
        private readonly Progress _progress = new(name);
        public Progress Progress => _progress;
        public string Name => _progress.Name;

        private readonly List<Step> _dependences = [];
        public Step[] Dependences => [.. _dependences];
        public bool HasDependences => _dependences.Count > 0 && !_dependences.Any(e => !e._progress.IsComplete());
        private bool _complete = false;
        public bool Complete => _complete;

        public void DependsOn(Step step) => _dependences.Add(step);

        public void Run()
        {
            // Solve dependences
            var depQueue = new Queue<Step>(_dependences);
            while (depQueue.Count > 0) {
                var s = depQueue.Dequeue();
                if (s.Complete) continue;
                _progress.Append(s._progress);
                s.Run();
            }
            // Run itself
            Make();

            Progress.Done();
        }
        protected abstract void Make();
    }

    #region Random Build Steps
    private class StepNode(string name) : Step(name)
    {
        protected override void Make()
        {
            
        }
    }

    private class InstallArtifactStep : Step
    {
        private Executable _artifact;

        public InstallArtifactStep(Executable artifact) : base("Installing Artifact")
        {
            _artifact = artifact;
            DependsOn(artifact.Step);
        }

        protected override void Make()
        {

        }
    }
    #endregion

    private class BuilderContext : IBuilder
    {
        private Step _installStep = new StepNode("Build");
        public Step GetInstallStep() => _installStep;

        // Build things
        public InstallArtifactStep AddInstallArtifact(Executable artifact) => new(artifact);
        public StepNode AddStep(string name) => new(name);
        public Executable CreateExecutable(string name, string rootDirectory) => new(name, rootDirectory);
    }

    private class Executable : IExecutable
    {

        private string _name;
        private string _rootDir;
        private BuildExecutableStep _step;
        public Step Step => _step;

        public Executable(string name, string rootDirectory)
        {
            _name = name;
            _rootDir = rootDirectory;
            _step = new(this);
        }

        public class BuildExecutableStep(Executable exe) : Step("Building executable") {
            private Executable _exeRef = exe;
            protected override void Make()
            {
                console.WriteLog("Scanning project...");
                List<Script> scripts = [];
                int ignored = 0;

                var root = Path.GetFullPath("./");

                Queue<string> dirs = new([root]);
                while (dirs.Count > 0)
                {
                    var curdir = dirs.Dequeue();
                    foreach (var i in Directory.GetFiles(curdir))
                    {
                        if (Path.GetExtension(i) == ".a") scripts.Add(new Script(root, i));
                        else ignored++;
                    }
                    foreach (var i in Directory.GetDirectories(curdir))
                        dirs.Enqueue(i);
                }

                console.WriteSuccess($"{scripts.Count} scripts found; {ignored} files ignored");
                console.WriteLog("Parsing files...");

                Progress[] ready = new Progress[scripts.Count];
                for (var i = 0; i < scripts.Count; i++)
                {
                    ready[i] = Progress.Branch($"Parsing {scripts[i]}");
                    var p = ready[i]; var s = scripts[i];
                    Task.Run(() => ParseScriptAsync(p, s));
                }

                while (true);
            }
        
            private void ParseScriptAsync(Progress progress, Script s)
            {
                Lexer.LexText(s.Read());
                progress.Done();
            }

        }
    }

}
