using System.Text;
using Abstract.Binutils.Abs.Bytecode;

namespace Abstract.Binutils.Abs.Elf;

public class ElfBuilder(string pname) : IDisposable {

    private DirectoryBuilder _root = new("ROOT", pname);
    public DirectoryBuilder Root => _root;

    public bool hasentrypoint = false;
    public bool linkable = false;
    public bool hasdependence = false;


    #region Directory builders
    public abstract class DirBuilder(string kind, string identifier) : IDisposable {
        public string kind = kind.PadRight(8)[..8].TrimEnd();
        public string identifier = identifier;

        protected DirBuilder _parent = null!;
        protected List<DirBuilder> _children = [];
        public DirBuilder[] Children => [.. _children];
        public int ChildrenCount => _children.Count;

        public List<(LumpBuilder lump, long ptr)> _externReferences = [];

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

        public override string ToString()
        {
            var str = new StringBuilder();

            str.AppendLine($"#{kind} {identifier} ({Content.Length} bytes)");

            var oldpos = Content.Position;
            Content.Position = 0;

            str.AppendLine($"\t{new string('_', 20)}");
            while (Content.Position < Content.Length)
                DecodeOpcode(Content, DataLump.Content, str);
            str.AppendLine($"\t{new string('_', 20)}");

            Content.Position = oldpos;

            return str.ToString();
        }
    

        private static void DecodeOpcode(Stream code, Stream data, StringBuilder buf)
        {
            var inst = new StringBuilder();

            List<byte> bytes = [];
            bytes.AddRange(code.LookArray(1));
            
            buf.Append($"\t${code.Position:X6}:\t");
            
            // TODO
        }
    }
    
    public class MetaBuilder(string kind, string identifier) : DirectoryBuilder(kind, identifier)
    {
        public override string ToString() => $"#{kind.Replace(" ","")} {identifier}";
    }
    #endregion

    public override string ToString() {
        var str = new StringBuilder();

        str.AppendLine("(executable_and_linkable_format");

        str.AppendLine($"(has_entry_point {hasentrypoint})");
        str.AppendLine($"(linkable        {linkable})");
        str.AppendLine($"(has_dependency  {hasdependence})");

        str.Append(_root.ToString());
        str.Append(')');

        return str.ToString();
    }


    public void Dispose()
    {
        _root.Dispose();
        GC.SuppressFinalize(this);
    }
}
