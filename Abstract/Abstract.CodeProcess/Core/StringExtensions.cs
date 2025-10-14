using System.Text;

namespace Abstract.CodeProcess.Core;

public static class StringExtensions
{
    public static string TabAll(this string? str)
    {
        if (str == null) return "<nil>";
        var sb = new StringBuilder();
        var lines = str.Split(Environment.NewLine);
        foreach (var l in lines)
        {
            if (string.IsNullOrEmpty(l)) sb.AppendLine();
            else sb.AppendLine($"\t{l}");
        }

        if (lines.Length > 0) sb.Length -= Environment.NewLine.Length;
        return sb.ToString();
    }
}
