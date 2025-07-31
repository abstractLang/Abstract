using System.Runtime.InteropServices;

namespace Abstract.Compiler.Build.Core.Resources;

public class Script(string path)
{

    public readonly string filePath = path;
    public readonly FileInfo fileInfo = new FileInfo(path);
    public string FileName => fileInfo.Name;
    public long FileSize => fileInfo.Length;


    public string Read() => File.ReadAllText(filePath);
    
    
    public override string ToString() => $"{FileName} ({BytesToString(FileSize)})";
    
    private static string BytesToString(long byteCount)
    {
        string[] suf = ["B", "KB", "MB", "GB", "TB", "PB", "EB"]; //Longs run out around EB
        if (byteCount == 0)
            return "0" + suf[0];
        long bytes = Math.Abs(byteCount);
        int place = Convert.ToInt32(Math.Floor(Math.Log(bytes, 1024)));
        double num = Math.Round(bytes / Math.Pow(1024, place), 1);
        return (Math.Sign(byteCount) * num) + suf[place];
    }
}
