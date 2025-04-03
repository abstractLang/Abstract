using System.Text;
using System.Text.RegularExpressions;
using Abstract.Core.Src;
using Abstract.Parsing;
using static Abstract.Core.Builder.BuildNamespace;

namespace Abstract.Build;

public class ConsoleWrapper(Progress progressRoot)
{
    private List<(char kind, string content)> _logs = [];
    private Progress _root = progressRoot;
    private int _cursorRoot = 0;

    private bool _drawing = false;

    private StringBuilder _buf = new();

    public void Reset()
    {
#if DEBUG
        return;
#else
        _cursorRoot = Console.GetCursorPosition().Top + 1;
#endif
    }
    
    public void Start()
    {
#if DEBUG
        return;
#else
        _drawing = true;
        Console.Write("\x1b[?25l");
        Task.Run(() => { while (_drawing) { Draw(); } });
#endif
    }
    public void Stop()
    {
#if DEBUG
        return;
#else
        _drawing = false;
        Console.Write("\x1b[?25h");
#endif
    }

    private void Draw()
    {
#if DEBUG
        return;
#else
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
#endif
    }


    public void WriteLog(object? value) =>     _logs.Add(('l', value?.ToString() ?? "null"));
    public void WriteErr(object? value) =>     _logs.Add(('e', value?.ToString() ?? "null"));
    public void WriteWarn(object? value) =>    _logs.Add(('w', value?.ToString() ?? "null"));
    public void WriteSuccess(object? value) => _logs.Add(('s', value?.ToString() ?? "null"));
}
