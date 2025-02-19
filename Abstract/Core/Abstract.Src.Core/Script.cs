
namespace Abstract.Src.Core;

public class Script : Source
{

    private string _root;
    private string _path;
    private FileInfo _finfo;

    public string FilePath => $"{_root}{Path.DirectorySeparatorChar}{_path}";
    private string FileIdentifier => Path.DirectorySeparatorChar != '/'
        ? _path.Replace(Path.DirectorySeparatorChar, '/') : _path;

    public Script(string root, string path)
    {
        _path = path;
        _root = root;
        _finfo = new FileInfo(FilePath);
    }

    public override Memory<char> Read()
    {
        throw new FileNotFoundException(_path);
    }

    public override string ToString() => $"{FileIdentifier} ({FormatSize(_finfo.Length)})";


    private static string FormatSize(long bytes)
    {
        string[] units = ["B", "KiB", "MiB", "GiB", "TiB"];
        double size = bytes;
        int index = 0;

        while (size >= 1024 && index < units.Length - 1)
        {
            size /= 1024;
            index++;
        }

        return $"{size:F2} {units[index]}";
    }
}
