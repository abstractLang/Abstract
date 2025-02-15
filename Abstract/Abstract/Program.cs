using System.Text;
using System.Text.RegularExpressions;

int consoleWidth = Console.WindowWidth;

Console.WriteLine(new string('-', consoleWidth));
Console.WriteLine(PadCenterAndWrap($@"

ABSTRACT COMPILER ver. 0.1.0
This software is still in development and may present errors.
If you spot a error, please help us to improve reporting it on:
{"\x1b[36;4m"}https://github.com/abstractLang/Abstract/issues{"\x1b[0;m"}

// TODO check if it's the last stable and report if not

".Trim(), consoleWidth));
Console.WriteLine(new string('-', consoleWidth));


static string PadCenterAndWrap(string source, int length)
{
    var str = new StringBuilder();

    List<string> srcLines = [.. source.Split(["\r\n", "\n", "\r"], StringSplitOptions.None)];
    for (var i = 0; i < srcLines.Count; i++)
    {
        if (srcLines[i].Length >= length)
        {
            str.Clear();
            var tokens = srcLines[i].Trim().Split(' ');

            var j = 0;
            while (str.Length + tokens[j].Length + 1 <= length)
                str.Append($"{tokens[j++]} ");

            srcLines[i] = str.ToString().Trim();
            srcLines.Insert(i + 1, string.Join(' ', tokens[j..]));
        }
    }

    str.Clear();
    foreach (var i in srcLines)
    {
        var sanitizedSrc = Regex.Replace(i, "\x1b\\[[0-9;]*[mK]", "");
        int spaces = length - sanitizedSrc.Length;
        int padLeft = spaces / 2 + i.Length;
        str.AppendLine(i.PadLeft(padLeft));
    }

    str.Length -= Environment.NewLine.Length;
    return str.ToString();
}
