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
        elfProgram._root = new(null!, 0, builder.Root.kind, builder.Root.identifier);
        elfProgram._allDirectories.Add((elfProgram._root));

        elfProgram.hasentrypoint = builder.hasentrypoint;
        elfProgram.hasdependence = builder.hasdependence;
        elfProgram.linkable = builder.linkable;

        Stack<(Directory curr, Queue<DirBuilder> toBake)> baking
            = new([(elfProgram._root, new(builder.Root.Children))]);

        Queue<(LumpBuilder, Stream)> _toSolveReferences = [];

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

                dir = new(curr, elfProgram._allDirectories.Count, next.kind, next.identifier, data);
                _toSolveReferences.Enqueue((lump, data));
            }
            else
                dir = new(curr, elfProgram._allDirectories.Count, next.kind, next.identifier, null);
            elfProgram._allDirectories.Add(dir);

            if (next.ChildrenCount > 0)
            {
                baking.Push((dir, new(next.Children)));
                continue;
            }
        }

        while (_toSolveReferences.Count > 0)
        {
            var (curr, data) = _toSolveReferences.Dequeue();

            if (curr is CodeBuilder @code)
            {
                foreach (var (ptr, r) in code.RefsAndDeps)
                {
                    var dir = elfProgram._allDirectories.Find(e => e.identifier == r)!;
                    data.Position = ptr;
                    data.WriteU32((uint)dir.index);
                }
            }
        }

        return elfProgram;
    }


    public override string ToString()
    {
        var str = new StringBuilder();

        str.AppendLine("(executable_and_linkable_format");

        str.AppendLine($"(dir_count       {_allDirectories.Count})");
        str.AppendLine($"(has_entry_point {hasentrypoint})");
        str.AppendLine($"(linkable        {linkable})");
        str.AppendLine($"(has_dependency  {hasdependence})");

        str.AppendLine(_root.ToString());
        str.Append(')');

        return str.ToString();
    }
}

