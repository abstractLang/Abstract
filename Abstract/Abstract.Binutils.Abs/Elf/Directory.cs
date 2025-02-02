using System.Text;

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
                {
                    str.Append("  ");
                    DecodeOpcode(content, datalump?.content!, str);
                }
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

                    str.Append($"  (;${i:X8};) ");
                    str.Append(string.Join(' ', buf.Select(e => $"{e:X2}")).PadRight(47));
                    str.Append("    ");
                    str.Append("(;| " + string.Join("", buf.Select(e
                    => !char.IsControl((char)e) ? (char)e : ' ')) + " |;)");

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

    private void DecodeOpcode(Stream code, Stream data, StringBuilder buf)
    {
        var inst = new StringBuilder();

        //var startpos = code.Position;

        switch (code.ReadByte())
        {
            case 0x00 /* Nop */ :             inst.Append($"Nop"); break;
            case 0x01 /* Illegal */ :         inst.Append($"Illegal"); break;
            case 0x02 /*LdConst nullptr  */ : inst.Append($"LdConst nullptr"); break;
            
            case 0x03 /* LdConst i1  */ :     inst.Append($"LdConst i1   " + (code.ReadByte() == 0 ? "false" : "true")); break;
            case 0x04 /* LdConst i8  */ :     inst.Append($"LdConst i8   {code.ReadByte()}"); break;
            case 0x05 /* LdConst i16 */ :     inst.Append($"LdConst i16  {code.ReadI16()}"); break;
            case 0x06 /* LdConst i32 */ :     inst.Append($"LdConst i32  {code.ReadI32()}"); break;
            case 0x07 /* LdConst i64 */ :     inst.Append($"LdConst i64  {code.ReadI64()}"); break;
            case 0x08 /* LdConst i128 */ :    inst.Append($"LdConst i128 {code.ReadI128()}"); break;
            case 0x09 /* LdConst iptr */ :    inst.Append($"LdConst iptr {code.ReadI64()}"); break;
            case 0x0A /* LdConst ix */ :
                var c = code.ReadByte();

                inst.Append($"LdConst i{c:,4}");
                inst.Append(code.ReadI64());
                break;
            case 0x0B /* LdConst struct */ :
                var dptr = code.ReadDirectoryPtr();

                inst.Append($"LdConst struct {GetDirName(dptr)}");

                // FIXME shit that i do not want to do now
                // discover the aligin of `dir` and read
                // this number in bytes
                break;

            case 0x0C /* LdField */ :         inst.Append($"LdField      {GetDirName(code.ReadDirectoryPtr())}"); break;
            case 0x0D /* LdLocal  immu8 */ :  inst.Append($"LdLocal      {code.ReadI8()}"); break;
            case 0x0E /* LdLocal  immu16 */ : inst.Append($"LdLocal      {code.ReadI16()}"); break;

            case 0x0F /* LdType */ :          inst.Append($"LdType       {GetDirName(code.ReadDirectoryPtr())}"); break;

            case 0x10 /* SetField */ :        inst.Append($"SetField     {GetDirName(code.ReadDirectoryPtr())}"); break;
            case 0x11 /* SetLocal immu8 */ :  inst.Append($"SetLocal     {code.ReadI8()}"); break;
            case 0x12 /* SetLocal immu16 */ : inst.Append($"SetLocal     {code.ReadI16()}"); break;

            case 0x13 /* GetType */ :         inst.Append($"GetType"); break;

            case 0x14 /* New */ :             inst.Append($"New"); break;
            case 0x15 /* Destroy */ :         inst.Append($"Destroy"); break;

            case 0x16 /* Pop */ :             inst.Append($"Pop"); break;
            case 0x17 /* Dup */ :             inst.Append($"Dup"); break;

            // aaaaaaaaa someone help me this is a fucking hell

            case 0x18: inst.Append($"Call void    {GetDirName(code.ReadDirectoryPtr())}"); break;
            case 0x19: inst.Append($"Call i1      {GetDirName(code.ReadDirectoryPtr())}"); break;
            case 0x1A: inst.Append($"Call i8      {GetDirName(code.ReadDirectoryPtr())}"); break;
            case 0x1B: inst.Append($"Call i16     {GetDirName(code.ReadDirectoryPtr())}"); break;
            case 0x1C: inst.Append($"Call i32     {GetDirName(code.ReadDirectoryPtr())}"); break;
            case 0x1D: inst.Append($"Call i64     {GetDirName(code.ReadDirectoryPtr())}"); break;
            case 0x1E: inst.Append($"Call i128    {GetDirName(code.ReadDirectoryPtr())}"); break;
            case 0x1F: inst.Append($"Call iptr    {GetDirName(code.ReadDirectoryPtr())}"); break;
            case 0x20:
                c = code.ReadByte();
                inst.Append($"Call i{c:,6}  ${GetDirName(code.ReadDirectoryPtr())}"); break;
            case 0x21: inst.Append($"Call struct ${GetDirName(code.ReadDirectoryPtr())}"); break;

            case 0x22: inst.Append($"Conv i{code.ReadByte()} i{code.ReadByte()}"); break;

            case 0x23: inst.Append($"Add"); break;
            case 0x24: inst.Append($"Sub"); break;
            case 0x25: inst.Append($"Mul"); break;
            case 0x26: inst.Append($"Mul.u"); break;
            case 0x27: inst.Append($"Div"); break;
            case 0x28: inst.Append($"Div.u"); break;
            case 0x29: inst.Append($"Rem"); break;
            case 0x2A: inst.Append($"Rem.u"); break;
            case 0x2B: inst.Append($"Abs"); break;
            case 0x2C: inst.Append($"Or"); break;
            case 0x2D: inst.Append($"And"); break;
            case 0x2E: inst.Append($"Not"); break;
            case 0x2F: inst.Append($"Xor"); break;
            case 0x30: inst.Append($"Nand"); break;

            // aaaaaaaaa someone help me this is a fucking hell x2

            // FIXME added later. see it in the future
            case 0x3D:
                data.Position = code.ReadU32();
                var str = data.ReadStringUTF8()
                    .Replace("\n", "\\n").Replace("\t", "\\t")
                    .Replace("\r", "\\r").Replace("\"", "\\\"");
                inst.Append($"LdConst str  *\"{str}\"");
                break;

            default: inst.Append("Invalid"); break; 
        }
        
        //var endpos = code.Position;
        //var ilength = endpos - startpos;

        //code.Position = startpos;
        //buf.AppendLine($"(;${code.Position:X4} {string.Join(" ", code.ReadArray(ilength).Select(e => $"{e:X2}"))};)");
        //code.Position = endpos;

        buf.AppendLine($"{inst.ToString()}");
    }
    private string GetDirName(uint ptr)
    {
        var dir = _program.AllDirectories[ptr];
        var dirglobal = dir.GetChild("GLOBAL");

        if (dirglobal == null) return $"${ptr:X}";
        else return $"${dirglobal.identifier}";
    }

}
