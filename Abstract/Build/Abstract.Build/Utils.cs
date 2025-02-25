using System.Text;
using System.Text.RegularExpressions;
using Abstract.Core.Src;
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
        private Progress _progress = new(name);
        public string Name => _progress.Name;

        private List<Step> _dependences = [];
        public Step[] Dependences = [];
        public bool HaveDependences => _dependences.Count == 0 || _dependences.All(e => e._progress.IsComplete());

        public void DependsOn(Step step) => _dependences.Add(step);

        public abstract void Run();
    }

    private class StepNode(string name) : Step(name)
    {
        public override void Run() {}
    }
    

    public class BuilderContext : IBuilder
    {
        private Step _installStep = new StepNode("Install");
        public Step GetInstallStep() => _installStep;
    }

    public class Executable : IExecutable
    {

        private string _name;
        private string _rootDir;
        private BuildExecutableStep _step;
        public Step Step => _step;

        private List<Script> _scripts = [];

        public Executable(string name, string rootDirectory)
        {
            _name = name;
            _rootDir = rootDirectory;
            _step = new(this);
        }


        public class BuildExecutableStep(Executable exe) : Step("Building executable") {
            private Executable _exeRef = exe;
            public override void Run()
            {
                console.WriteWarn("Todo Bueild executable step");
            }
        }

    }

    private static Executable CreateExecutable(string name, string rootDirectory)
    {
        return new Executable(name, rootDirectory);
    }
    private static void InstallArtifact(Executable exe)
    {
        console.WriteWarn("Installing...");
        console.WriteSuccess("Build finished!");
    }

}
