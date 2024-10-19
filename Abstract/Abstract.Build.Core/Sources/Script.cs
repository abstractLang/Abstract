namespace Abstract.Build.Core.Sources;

public abstract class Script (string path)
{

    public readonly string? Directory = path;
    public string Name => Path.GetFileName(Directory) ?? "";

    public long SizeInBytes => Directory == null ? new FileInfo(Directory!).Length : 0;

    public string GetContent() => Directory != null ? File.ReadAllText(Directory) : "";
    public byte[] GetContentInBytes => Directory != null ? File.ReadAllBytes(Directory) : [];

    public override string ToString() => $"{Name:16} {SizeInBytes/1024}Kb";

}
