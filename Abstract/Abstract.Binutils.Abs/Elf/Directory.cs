using System.Text;
using Abstract.Binutils.Abs.Bytecode;

namespace Abstract.Binutils.Abs.Elf;


public class Directory {

    internal Directory(Directory parent, uint index, string kind, string identifier, Stream? content = null)
    {
        _parent = parent!;
        this.kind = kind;
        this.identifier = identifier;
        this.content = content;
        this.index = index;
        parent?._children.Add(this);
    }

    private readonly Directory _parent;

    public readonly uint index;
    public readonly string kind;
    public readonly string identifier;
    public readonly Stream? content = null;

    private readonly List<Directory> _children = [];
    public Directory[] Children => [.. _children];
    public int ChildrenCount => _children.Count;

    public Directory? GetChild(string kind, string identifier)
    {
        return _children.Find(e => e.kind == kind && e.identifier == identifier);
    }
    public Directory? GetChild(string kind)
    {
        return _children.Find(e => e.kind == kind);
    }
    public Directory[] GetChildren(string kind)
    {
        return [.. _children.Where(e => e.kind == kind)];
    }

    public override string ToString()
    {
        var str = new StringBuilder();

        str.Append($"({identifier} ({kind}) ({index:X}) ");

        if (content == null) str.Append($"({_children.Count} dirs)");
        else str.Append($"({content.Length} bytes)");

        if (content == null)
        {
            if (kind == "PARAM" && _children.Count == 2)
            {
                if (_children[0].kind != "TYPE" || _children[1].kind != "NAME") goto GeneralElse;
                str.Append($" ({_children[0].identifier}) {_children[1].identifier}");
            }
            else goto GeneralElse;
            goto Return;

            GeneralElse:
            if (_children.Count > 0) str.AppendLine(" {");
            foreach (var i in _children)
            {
                var lines = i.ToString().Split(Environment.NewLine)[..];
                foreach (var l in lines) str.AppendLine($"  {l}");
            }
            if (_children.Count > 0) str.Append('}');

        }
        else
        {
            if (content.Length > 0) str.AppendLine(" {");

            var bufOldPos = content.Position;
            content.Position = 0;
            
            if (kind == "CODE")
            {
                var datalump = _parent.GetChild("DATA", identifier);

                while (content.Position < content.Length)
                    DecodeOpcode(content, datalump!.content!, str);
            }
            else
            {
                for (var i = 0; i < content.Length; i += 16)
                {
                    var buf = content.ReadArray((int)Math.Min(16, content.Length - i));

                    str.Append($"\t${i:X8}: ");
                    str.Append(string.Join(' ', buf.Select(e => $"{e:X2}")).PadRight(47));
                    str.Append("    ");
                    str.Append(string.Join("", buf.Select(e
                    => !char.IsControl((char)e) ? (char)e : ' ')));

                    str.AppendLine();
                }
            }
            content.Position = bufOldPos;

            if (content.Length > 0) str.Append('}');
        }

        Return:
        str.Append(')');
        return str.ToString();
    }

    private static void DecodeOpcode(Stream code, Stream data, StringBuilder buf)
    {
        var inst = new StringBuilder();

        List<byte> bytes = [];
        bytes.AddRange(code.LookArray(1));
        
        buf.Append($"\t${code.Position:X4}:\t");
        
        // TODO
    }

}
