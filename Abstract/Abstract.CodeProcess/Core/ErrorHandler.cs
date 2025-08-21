using System.Text;

namespace Abstract.CodeProcess.Core;

public class ErrorHandler
{

    private List<Exception> _errors = [];
    public int ErrorCount => _errors.Count;
    
    public void RegisterError(Exception ex) => _errors.Add(ex);

    
    public void Dump()
    {
        var s = new StringBuilder();

        s.AppendLine($"(/) {ErrorCount} errors:");

        foreach (var e in _errors)
        {
            s.AppendLine($"- {e.Message} {e.StackTrace?.Split("\n", StringSplitOptions.RemoveEmptyEntries)[0]}");
        }

        

        Console.WriteLine(s.ToString());
    }
}
