using System.Diagnostics;
using System.Text;

namespace Abstract.CodeProcess.Core;

public class ErrorHandler
{

    private List<Exception> _general_errors = [];
    private Dictionary<string, List<Exception>> _file_errors = [];
    private string? _currentFile = null;
    
    public int ErrorCount { get; private set; } = 0;

    public void RegisterError(Exception ex)
    {
        if (_currentFile == null) _general_errors.Add(ex);
        else
        {
            if (!_file_errors.ContainsKey(_currentFile)) _file_errors.Add(_currentFile, []);
            _file_errors[_currentFile].Add(ex);
        }
        ErrorCount++;
    }
    public void SetFile(string? file) => _currentFile = file;

    
    public void Dump()
    {
        var s = new StringBuilder();

        s.AppendLine($"(/) {ErrorCount} errors:");

        foreach (var e in _general_errors)
        {
            s.AppendLine($"- {e.Message} {e.StackTrace}");
        }

        foreach (var f in _file_errors)
        {
            s.AppendLine($"{f.Key}:");
            foreach (var e in f.Value)
            {
                s.AppendLine($"- {e.Message}");
                Console.ForegroundColor = ConsoleColor.Gray;
                s.AppendLine($"    {e.StackTrace?.Split("\n", StringSplitOptions.RemoveEmptyEntries)[0]}");
                Console.ResetColor();
            }
        }

        Console.WriteLine(s.ToString());
    }
}
