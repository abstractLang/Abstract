using System.Text;

public static class PMNamespace
{

    public static string[] GetParts(string nmsp)
    {
        return nmsp.Split(".");
    }

    public static string[] GetPartsFromDirectory(string path)
    {
        return path.Split(Path.PathSeparator);
    }

    public static string Combine(params string[] nmsp)
    {
        StringBuilder sb = new();

        foreach (var i in nmsp)
        {
            if (!string.IsNullOrWhiteSpace(i))
            {
                sb.Append(i);
                sb.Append('.');
            }
        }

        if (sb.Length > 0) sb.Length -= 1;
        return sb.ToString();
    }

}
