namespace Abstract.Binutils.Abs.Elf;


internal interface IDirTreeNode {
    public ElfProgram Program { get; }
    public Directory Parent { get; }
    internal IDirTreeNode InternalParent { get; }

    public IEnumerator<Directory> Children { get; }
}

/*
Directory structure:
    default: 12 bytes
    | 4 bytes ptr - identifier
    | 4 bytes ptr - pointer
    | 4 bytes int - size
    extended: 20 bytes
    | 8 bytes ptr - identifier
    | 8 bytes ptr - pointer
    | 4 bytes int - size
*/
internal class DirectoryRoot(ElfProgram program) : IDirTreeNode {

    private readonly ElfProgram _program = program;
    private List<Directory> _children = [];


    public ElfProgram Program => _program;
    public IDirTreeNode Parent => null!;

     Directory IDirTreeNode.Parent => null!;
     IDirTreeNode IDirTreeNode.InternalParent => null!;
    IEnumerator<Directory> IDirTreeNode.Children => _children.GetEnumerator();

}
public class Directory
: IDirTreeNode {

    internal Directory(IDirTreeNode parent, string identifier, long pointer)
    {
        _parent = parent;
        this.identifier = identifier;
        this.pointer = pointer;
    }

    private readonly IDirTreeNode _parent;
    public readonly string identifier;
    public readonly long pointer;

    private List<Directory> _children = [];


    ElfProgram IDirTreeNode.Program => _parent != null ? _parent.Program : null!;
    Directory IDirTreeNode.Parent => (Directory)_parent;
    IDirTreeNode IDirTreeNode.InternalParent => _parent;

    IEnumerator<Directory> IDirTreeNode.Children => _children.GetEnumerator();

}
