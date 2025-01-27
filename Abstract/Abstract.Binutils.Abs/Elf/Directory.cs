using System.Text;
using Abstract.Binutils.Abs.Bytecode;

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
        var instruction = Instructions.Get(code.ReadU8());
        
        buf.Append($"\t${code.Position:X4}:\t");
        inst.Append($"{instruction.b}");
        
        if (instruction.t != Types.Unspecified)
        {
            inst.Append(new string(' ', 10 - inst.Length));
            inst.Append($"\t{instruction.t.ToString().ToLower()}");
        }

        if (instruction.args != null && instruction.args.Length > 0)
        {
            int argsCount = 0;
            foreach (var arg in instruction.args)
            {
                inst.Append(new string(' ',Math.Max(0, 25 + (15 * argsCount++) - inst.Length) + '\t'));
                
                switch (arg)
                {
                    case "immi8":
                        bytes.AddRange(code.LookArray(1));
                        inst.Append(code.ReadI8());
                        break;
                    case "immi16":
                        bytes.AddRange(code.LookArray(2));
                        inst.Append(code.ReadI16());
                        break;
                    case "immi32":
                        bytes.AddRange(code.LookArray(4));
                        inst.Append(code.ReadI32());
                        break;
                    case "immi64":
                        bytes.AddRange(code.LookArray(8));
                        inst.Append(code.ReadI64());
                        break;
                    case "immi128":
                        bytes.AddRange(code.LookArray(16));
                        inst.Append(code.ReadI128());
                        break;

                    case "immu8":
                        bytes.AddRange(code.LookArray(1));
                        inst.Append(code.ReadU8());
                        break;
                    case "immu16":
                        bytes.AddRange(code.LookArray(2));
                        inst.Append(code.ReadU16());
                        break;
                    case "immu32":
                        bytes.AddRange(code.LookArray(4));
                        inst.Append(code.ReadU32());
                        break;
                    case "immu64":
                        bytes.AddRange(code.LookArray(8));
                        inst.Append(code.ReadU64());
                        break;
                    case "immu128":
                        bytes.AddRange(code.LookArray(16));
                        inst.Append(code.ReadU128());
                        break;
                    
                    case "immtype":
                        bytes.AddRange(code.LookArray(1));
                        inst.Append(((Types)code.ReadU8()).ToString());
                        break;

                    case "refstr":
                        bytes.AddRange(code.LookArray(4));
                        var strptr = code.ReadU32();
                        data.Position = strptr;
                        var strdata = data.ReadStringUTF8()
                            .Replace("\n", "\\n").Replace("\r", "\\r")
                            .Replace("\t", "\\t").Replace("" + char.ConvertFromUtf32(0), "\\0"); ;
                        inst.Append($"* -> \"{strdata}\"");
                        break;

                    case "reftype"
                    or "reffunc"
                    or "refstatic"
                    or "reffield"
                    or "reflocal":
                        bytes.AddRange(code.LookArray(4));
                        var index = code.ReadU32();
                        inst.Append($"${index:X}");
                        break;

                    default:
                        inst.Append($"[{arg} not handled]");
                        break;
                }
            }
        }

        
        buf.Append(string.Join(' ', bytes.Select(e => $"{e:X2}")).PadRight(30));
        buf.AppendLine("\t:\t" + inst.ToString());
    }

}
