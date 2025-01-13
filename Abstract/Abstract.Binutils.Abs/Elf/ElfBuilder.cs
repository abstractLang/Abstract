using System.Text;
using Abstract.Binutils.Abs.Bytecode;

namespace Abstract.Binutils.Abs.Elf;

public class ElfBuilder(string pname) : IDisposable {

    private DirectoryBuilder _root = new("ROOT", pname);
    public DirectoryBuilder Root => _root;

    #region Directory builders
    public abstract class DirBuilder(string kind, string identifier) : IDisposable {
        public string kind = kind.PadRight(8)[..8].TrimEnd();
        public string identifier = identifier;

        protected DirBuilder _parent = null!;
        protected List<DirBuilder> _children = [];
        public DirBuilder[] Children => [.. _children];
        public int ChildrenCount => _children.Count;

        public void AppendChild(params DirBuilder[] nodes)
        {
            foreach (var child in nodes)
            {
                child._parent = this;
                _children.Add(child);
            }
        }
        public DirBuilder? GetChild(string kind, string identifier)
        {
            return _children.FirstOrDefault(e => e.kind == kind && e.identifier == identifier);
        }
        public bool HasChild(string kind, string identifier)
        {
            return _children.Any(e => e.kind == kind && e.identifier == identifier);
        }
        public bool TryGetChild(string kind, string identifier, out DirBuilder dir)
        {
            dir = _children.FirstOrDefault(e => e.kind == kind && e.identifier == identifier)!;
            return dir != null;
        }

        public override string ToString() {
            var str = new StringBuilder();

            if (_children.Count > 0)
            {
                str.AppendLine($"#{kind.Replace(" ","")} {identifier} ({_children.Count} dirs)");

                var lines = ContentToString().Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries);
                foreach (var l in lines) str.AppendLine($"\t{l}");
            }
            else str.Append($"#{kind.Replace(" ","")} {identifier}");

            return str.ToString();
        }
        public string ContentToString()
        {
            var str = new StringBuilder();

            foreach (var i in _children)
                str.AppendLine($"{i.ToString()}");

            return str.ToString();
        }
    
        public virtual void Dispose()
        {
            foreach (var i in _children) i.Dispose();
            GC.SuppressFinalize(this);
        }
    }

    public class DirectoryBuilder(string kind, string identifier) : DirBuilder(kind, identifier) {

    }

    public class AttributesLumpBuilder() : LumpBuilder("CODE", "attrb") {
        public List<(string type, string name)> parameters = [];

        public override string ToString()
        {
            var str = new StringBuilder();

            str.Append( $"#{kind} {identifier} ");
            str.AppendLine($"({string.Join(", ", parameters.Select(e => $"{e.type} {e.name}"))})");

            return str.ToString();
        }
    }

    public class LumpBuilder : DirBuilder
    {
        public LumpBuilder(string kind, string identifier) : base(kind, identifier)
        {
            _memstream = new();
        }
        private MemoryStream _memstream;

        public Stream Content => _memstream;

        public override string ToString()
        {
            var str = new StringBuilder();

            str.AppendLine( $"#{kind} {identifier} ({_memstream.Length} bytes)");

            var bufOldPos = _memstream.Position;
            _memstream.Position = 0;
            str.AppendLine(new string('_', 20));
            for (var i = 0; i < _memstream.Length; i += 16)
            {
                var buf = _memstream.ReadArray((int)Math.Min(16, _memstream.Length - i));

                str.Append($"\t${i:X8}: ");
                str.Append(string.Join(' ', buf.Select(e => $"{e:X2}")).PadRight(47));
                str.Append("    ");
                str.Append(string.Join("", buf.Select(e
                => !char.IsControl((char)e) ? (char)e : '_')));

                str.AppendLine();
            }
            str.AppendLine(new string('_', 20));
            _memstream.Position = bufOldPos;


            return str.ToString();
        }
        public override void Dispose()
        {
            _memstream.Dispose();
            base.Dispose();
        }
    }
    
    public class CodeBuilder(string identifier) : LumpBuilder("CODE", identifier)
    {
        public LumpBuilder DataLump => (_parent.GetChild("DATA", identifier) as LumpBuilder)!;
        public bool HasDataLump => DataLump != null;
        private bool _baked = false;

        private List<(long ptr, string r)> _refsAndDeps = [];
        public (long ptr, string r)[] RefsAndDeps => [.. _refsAndDeps];

        public void WriteOpCode(Base opcode, Types type = Types.Unspecified) => WriteOpCode(Instructions.Get(opcode, type), []);
        public void WriteOpCode(Base opcode, Types type, params dynamic?[] values) => WriteOpCode(Instructions.Get(opcode, type), values);
        public void WriteOpCode(Base opcode, params dynamic?[] values) => WriteOpCode(Instructions.Get(opcode), values);
        public void WriteOpCode(InstructionInfo instruction) => WriteOpCode(instruction, []);
        public void WriteOpCode(InstructionInfo instruction, params dynamic?[] values)
        {
            if (_baked) throw new Exception();

            if (instruction.args != null && values.Length != instruction.args.Length)
                throw new Exception($"instruction \"{instruction}\" expects {instruction.args.Length},"
                + $" found {values.Length}");
            
            Content.WriteU8(instruction.opcode);

            for (var i = 0; i < values.Length; i++)
            {
                var argType = instruction.args![i];
                var argValue = values[i];

                switch (argType)
                {
                    case "immi8": if (argValue is not sbyte) goto Invalid; break;
                    case "immi16": if (argValue is not short) goto Invalid; break;
                    case "immi32": if (argValue is not int) goto Invalid; break;
                    case "immi64": if (argValue is not long) goto Invalid; break;
                    case "immi128": if (argValue is not Int128) goto Invalid; break;

                    case "immu8": if (argValue is not byte) goto Invalid; break;
                    case "immu16": if (argValue is not ushort) goto Invalid; break;
                    case "immu32": if (argValue is not uint) goto Invalid; break;
                    case "immu64": if (argValue is not ulong) goto Invalid; break;
                    case "immu128": if (argValue is not UInt128) goto Invalid; break;

                    case "immtype": if (argValue is not Types) goto Invalid; break;

                    

                    case "refstr"

                    or "reftype"
                    or "reffunc"

                    or "refstatic"
                    or "reffield"
                    or "reflocal":
                        if (argValue is not string) goto Invalid; break;

                    default:
                        throw new NotImplementedException(
                            $"immediate of type \"{argType}\" not implemented!");
                }

                goto Ok;
                Invalid: 
                throw new Exception($"value of type {argValue?.GetType().Name ?? "nil"} is not"
                    + $" assiginabble to {argType}");

                Ok:
                if (argValue is sbyte @v1) Content.WriteI8(v1);
                else if (argValue is short @v2) Content.WriteI16(v2);
                else if (argValue is int @v3) Content.WriteI32(v3);
                else if (argValue is long @v4) Content.WriteI64(v4);
                else if (argValue is Int128 @v5) Content.WriteI128(v5);

                else if (argValue is byte @v6) Content.WriteU8(v6);
                else if (argValue is ushort @v7) Content.WriteU16(v7);
                else if (argValue is uint @v8) Content.WriteU32(v8);
                else if (argValue is ulong @v9) Content.WriteU64(v9);
                else if (argValue is UInt128 @vA) Content.WriteU128(vA);

                else if (argValue is Types @vB) Content.WriteU8((byte)vB);

                else if (argValue is string @str)
                {
                    if (argType == "refstr")
                    {
                        var ptr = DataLump.Content.WriteStringUTF8(str);
                        Content.WriteU32(ptr);
                    }

                    else if (argType is "reftype" or "reffunc" or "refstatic" or "reffield" or "reflocal")
                    {
                        _refsAndDeps.Add((Content.Position, str));
                        Content.WriteU32((uint)(_refsAndDeps.Count - 1));
                    }
                }

            }
            
        }

        public void Bake() => _baked = true;
        public override string ToString()
        {
            var str = new StringBuilder();

            str.AppendLine($"#{kind} {identifier} ({Content.Length} bytes)");

            var oldpos = Content.Position;
            Content.Position = 0;

            str.AppendLine($"\t{new string('_', 20)}");
            while (Content.Position < Content.Length)
                DecodeOpcode(Content, DataLump.Content, _refsAndDeps, str);
            str.AppendLine($"\t{new string('_', 20)}");

            Content.Position = oldpos;

            return str.ToString();
        }
    
        private static void DecodeOpcode(Stream code, Stream data, List<(long, string)> rds, StringBuilder buf)
        {
            var inst = new StringBuilder();

            List<byte> bytes = [];
            bytes.AddRange(code.LookArray(1));
            var instruction = Instructions.Get(code.ReadU8());
            
            buf.Append($"\t${code.Position:X6}:\t");
            inst.Append($"{instruction.b}");
            
            if (instruction.t != Types.Unspecified)
            {
                inst.Append(new string(' ', 15 - inst.Length));
                inst.Append($"\t{instruction.t.ToString().ToLower()}");
            }

            if (instruction.args != null && instruction.args.Length > 0)
            {
                foreach (var arg in instruction.args)
                {
                    inst.Append(new string(' ', 25 - inst.Length));
                    
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
                            var strdata = data.ReadStringUTF8();
                            inst.Append($"\"{strdata}\"");
                            break;

                        case "reftype" or "reffunc" or "refstatic" or "reffield" or "reflocal":
                            bytes.AddRange([255, 255, 255, 255]);
                            var index = code.ReadU32();
                            inst.Append($"${rds[(int)index].Item2}");
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
    
    public class MetaBuilder(string kind, string identifier) : DirectoryBuilder(kind, identifier)
    {
        public override string ToString() => $"#{kind.Replace(" ","")} {identifier}";
    }
    #endregion

    public override string ToString() => _root.ToString();


    public void Dispose()
    {
        _root.Dispose();
        GC.SuppressFinalize(this);
    }
}
