using System.Text;
using Abstract.Binutils.Abs.Bytecode;

namespace Abstract.Binutils.Abs.Elf;


public class Directory {

    internal Directory(ElfProgram elf, Directory parent, uint index, string kind, string identifier, Stream? content = null)
    {
        _program = elf;
        _parent = parent;
        this.kind = kind;
        this.identifier = identifier;
        this.content = content;
        this.index = index;
        parent?._children.Add(this);
    }

    private readonly ElfProgram _program;
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

        str.Append($"({kind} "
        + (string.IsNullOrEmpty(identifier) ? "" : $"\"{identifier}\" ")
        + $"(;{index:X};)");

        if (content == null)
        {
            
            if (_children.Count > 0)  str.AppendLine();
            foreach (var i in _children)
            {
                var lines = i.ToString().Split(Environment.NewLine)[..];
                foreach (var l in lines) str.AppendLine($"  {l}");
            }
            if (_children.Count > 0)
                str.Remove(str.Length-Environment.NewLine.Length, Environment.NewLine.Length);

        }
        else
        {
            var bufOldPos = content.Position;
            content.Position = 0;
            
            if (kind == "CODE")
            {
                str.AppendLine();

                var datalump = _parent.GetChild("DATA", identifier);

                while (content.Position < content.Length)
                    DecodeOpcode(content, datalump?.content!, str);
            }
            else if (kind == "PARAM")
            {
                var kind = content.ReadByte();
                if (kind == 0)
                {
                    var typeDir = _program.AllDirectories[content.ReadDirectoryPtr()];
                    var globalDir = typeDir.GetChild("GLOBAL");
                    str.AppendLine(" ($" + (globalDir == null ? $"{typeDir.index:X}" : globalDir.identifier) + ')');
                }
                else if (kind == 1)
                {
                    var fromDir = _program.AllDirectories[content.ReadDirectoryPtr()];
                    var globalDir = fromDir.GetChild("GLOBAL");
                    var idx = content.ReadU16();
                    str.AppendLine($" (generic ($"
                        + (globalDir == null ? "${fromDir.index:X}" : globalDir.identifier)
                        + $" {idx}))");
                }
                else str.AppendLine();
            }
            
            else if (kind == "META" && identifier == "structheader")
            {
                str.AppendLine($" (aligin {content.ReadU32()})");
            }
            
            else
            {
                str.AppendLine();
                for (var i = 0; i < content.Length; i += 16)
                {
                    var buf = content.ReadArray((int)Math.Min(16, content.Length - i));

                    str.Append($"(;${i:X8};)");
                    str.Append(string.Join(' ', buf.Select(e => $"{e:X2}")).PadRight(47));
                    str.Append("    ");
                    str.Append("(;" + string.Join("", buf.Select(e
                    => !char.IsControl((char)e) ? (char)e : ' ')) + ";)");

                    str.AppendLine();
                }
            }

            // delete last line feed
            str.Remove(str.Length - Environment.NewLine.Length, Environment.NewLine.Length);

            content.Position = bufOldPos;

        }

        str.Append(')');
        return str.ToString();
    }

    private static void DecodeOpcode(Stream code, Stream data, StringBuilder buf)
    {
        var inst = new StringBuilder();

        List<byte> bytes = [];
        bytes.AddRange(code.LookArray(1));
        
        buf.Append($"(;${code.Position:X4};)");

        code.ReadByte();
        // TODO
    }

}
