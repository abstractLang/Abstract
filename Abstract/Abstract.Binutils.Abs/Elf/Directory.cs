using System.Text;

namespace Abstract.Binutils.Abs.Elf;


public class Directory {

    internal Directory(Directory parent, long index, string kind, string identifier, Stream? content = null)
    {
        _parent = parent!;
        this.kind = kind;
        this.identifier = identifier;
        this.content = content;
        this.index = index;
        parent?._children.Add(this);
    }

    private readonly Directory _parent;

    public readonly long index;
    public readonly string kind;
    public readonly string identifier;
    public readonly Stream? content = null;

    private readonly List<Directory> _children = [];


    public override string ToString()
    {
        var str = new StringBuilder();

        str.Append($"$({index:X}) #{kind} #{identifier} ");

        if (content == null) str.AppendLine($"({_children.Count} dirs)");
        else str.AppendLine($"({content.Length} bytes) {{");

        if (content == null)
        {
            foreach (var i in _children)
            {
                var lines = i.ToString().Split(Environment.NewLine)[..^1];
                foreach (var l in lines) str.AppendLine($"\t{l}");
            }
        }
        else
        {
            var bufOldPos = content.Position;
            content.Position = 0;

            for (var i = 0; i < content.Length; i += 16)
            {
                var buf = content.ReadArray((int)Math.Min(16, content.Length - i));

                str.Append($"\t${i:X8}: ");
                str.Append(string.Join(' ', buf.Select(e => $"{e:X2}")).PadRight(47));
                str.Append("    ");
                str.Append(string.Join("", buf.Select(e
                => !char.IsControl((char)e) ? (char)e : '_')));

                str.AppendLine();
            }
            content.Position = bufOldPos;

            str.AppendLine($"}}");
        }

        return str.ToString();
    }

}
