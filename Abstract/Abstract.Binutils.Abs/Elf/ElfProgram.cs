using System.Text;
using static Abstract.Binutils.Abs.Elf.ElfBuilder;

namespace Abstract.Binutils.Abs.Elf;

public class ElfProgram {

    private string _programName = null!;

    private Directory _root = null!;
    private List<Directory> _allDirectories = [];

    private bool hasentrypoint = false;
    private bool linkable = false;
    private bool hasdependence = false;

    public string Name => _root.identifier;
    public Directory RootDirectory => _root;
    public Directory[] AllDirectories => [.. _allDirectories];


    public static ElfProgram Load(byte[] data) => Load(new MemoryStream(data));
    public static ElfProgram LoadFile(string path)
    {
        var stream = File.OpenRead(path);
        ElfProgram result = null!;

        try { result = Load(stream); }
        catch { stream.Close(); throw; }

        stream.Close();
        return result;
    }

    public static ElfProgram Load(Stream data)
    {
        

        return null!;
    }


    public static ElfProgram BakeBuilder(ElfBuilder builder)
    {
        ElfProgram elfProgram = new();

        elfProgram._programName = builder.Root.identifier;
        elfProgram._root = new(elfProgram, null!, 0, builder.Root.kind, builder.Root.identifier);
        elfProgram._allDirectories.Add((elfProgram._root));

        elfProgram.hasentrypoint = builder.hasentrypoint;
        elfProgram.hasdependence = builder.hasdependence;
        elfProgram.linkable = builder.linkable;

        Stack<(Directory curr, Queue<DirBuilder> toBake)> baking
            = new([(elfProgram._root, new(builder.Root.Children))]);

        Queue<(LumpBuilder, Stream)> _lumpsToBake = [];
        Dictionary<DirBuilder, List<(uint dirID, long ptr)>> _refsToSolve = [];

        while(baking.Count > 0)
        {
            var (curr, toBake) = baking.Peek();
            if (toBake.Count == 0) { baking.Pop(); continue; }
            var next = toBake.Dequeue();
            
            Directory dir;
            if (next is LumpBuilder @lump)
            {
                var data = new MemoryStream();
                lump.Content.Position = 0;
                lump.Content.CopyTo(data);

                dir = new(elfProgram, curr, (uint)elfProgram._allDirectories.Count, next.kind, next.identifier, data);
                _lumpsToBake.Enqueue((lump, data));
            }
            else
                dir = new(elfProgram, curr, (uint)elfProgram._allDirectories.Count, next.kind, next.identifier, null);
            elfProgram._allDirectories.Add(dir);

            foreach (var r in next._externReferences)
            {
                if (_refsToSolve.TryGetValue(r.lump, out var values))
                    values.Add((dir.index, r.ptr));
                else _refsToSolve.Add(r.lump, [(dir.index, r.ptr)]);
            }

            if (next.ChildrenCount > 0)
            {
                baking.Push((dir, new(next.Children)));
                continue;
            }
        }

        while(_lumpsToBake.Count > 0)
        {
            var (lump, stream) = _lumpsToBake.Dequeue();
            if (!_refsToSolve.TryGetValue(lump, out var deps)) continue;

            foreach (var (reference, ptr) in deps)
            {
                stream.Position = ptr;
                stream.WriteDirectoryPtr(reference);
            }
        }

        return elfProgram;
    }


    public override string ToString()
    {
        var str = new StringBuilder();

        str.AppendLine("(elf");

        str.AppendLine($"  (;directory count: {_allDirectories.Count};)");
        str.AppendLine($"  (flag \"has_entry_point\" ({hasentrypoint}))");
        str.AppendLine($"  (flag \"linkable\"        ({linkable}))");
        str.AppendLine($"  (flag \"has_dependency\"  ({hasdependence}))");

        str.AppendLine();

        var lines = _root.ToString().Split(Environment.NewLine);
        foreach (var i in lines) str.AppendLine($"  {i}");
        if (lines.Length > 0) str.Remove(str.Length-Environment.NewLine.Length, Environment.NewLine.Length);
        str.AppendLine(")");

        return str.ToString();
    }
}

