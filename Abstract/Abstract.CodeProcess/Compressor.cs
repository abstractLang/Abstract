using Abstract.Binutils.Abs.Bytecode;
using Abstract.Binutils.Abs.Elf;
using Abstract.Build;
using Abstract.CompileTarget.Core.Enums;
using Abstract.Parser.Core.ProgData;
using Abstract.Parser.Core.ProgMembers;
using static Abstract.Binutils.Abs.Elf.ElfBuilder;

namespace Abstract.Parser;

public class Compressor(ErrorHandler errHandler)
{
    private readonly ErrorHandler _errHandler = errHandler;

    private Project _currentProject = null!;
    private ElfBuilder _builder = null!;
    private Dictionary<ProgramMember, DirBuilder> _memberDirectoryMap = null!;

    public ElfProgram[] DoCompression(ProgramRoot program, ExpectedELFFormat expectedElfFormat)
    {
        List<ElfProgram> elfs = [];

        // Each project should result in one ELF object
        foreach (var i in program.projects)
        {
            using var projectElf = CompressProject(i.Key, i.Value, program);
            var bakedElf = ElfProgram.BakeBuilder(projectElf);
            elfs.Add(bakedElf);
        }

        return LinkProgram([.. elfs], expectedElfFormat);
    }

    public ElfProgram[] LinkProgram(ElfProgram[] elfs, ExpectedELFFormat expectedElfFormat)
    {
        if (expectedElfFormat == ExpectedELFFormat.onePerProject) return elfs;

        

        return elfs;
    }

    private ElfBuilder CompressProject(string projectName, Project project, ProgramRoot program)
    {
        var elfBuilder = new ElfBuilder(projectName);
        var dirRoot = elfBuilder.Root;

        _builder = elfBuilder;
        _currentProject = project;
        _memberDirectoryMap = [];

        var projectdir = new DirectoryBuilder("PROJECT", project.identifier);
        //var includedir = new DirectoryBuilder("INCLUDE", "./");

        dirRoot.AppendChild(projectdir);

        ScanAllChildren(project, projectdir);
        CompressAllMembers();

        if (project == program.MainProject) elfBuilder.hasentrypoint = true;

        _memberDirectoryMap = null!;
        _currentProject = null!;
        _builder = null!;

        return elfBuilder;
    }

    #region Scan
    private void ScanAllChildren(ProgramMember parent, DirBuilder parentDir)
    {
        foreach (var i in parent.ChildrenNamespaces) ScanNamespace(i, parentDir);
        foreach (var i in parent.ChildrenFields) ScanField(i, parentDir);
        foreach (var i in parent.ChildrenFunctions) ScanFunctionGroup(i, parentDir);
        foreach (var i in parent.ChildrenTypes) ScanStructure(i, parentDir);
        foreach (var i in parent.ChildrenEnums) ScanEnum(i, parentDir);
    }

    private void ScanNamespace(Namespace member, DirBuilder parent)
    {
        var header = new DirectoryBuilder("NMSP", member.identifier);
        _memberDirectoryMap.Add(member, header);

        ScanAllChildren(member, header);

        parent.AppendChild(header);
    }
    private void ScanField(Field member, DirBuilder parent)
    {
        var header = new DirectoryBuilder("FIELD", member.identifier);
        _memberDirectoryMap.Add(member, header);

        parent.AppendChild(header);
    }
    private void ScanFunctionGroup(FunctionGroup member, DirBuilder parent)
    {
        if (member.IsSingle)
        {
            ScanFunction(member.Overloads[0], parent);
        }
        else
        {
            var header = new DirectoryBuilder("FGROUP", member.identifier);
            _memberDirectoryMap.Add(member, header);

            for (var i = 0; i < member.Overloads.Length; i++)
                ScanFunction(member.Overloads[i], header, i);

            parent.AppendChild(header);
        }
    }
    private void ScanFunction(Function member, DirBuilder parent, int overloadIdx = -1)
    {
        var header = new DirectoryBuilder("FUNC", 
            overloadIdx >= 0 ? $"#{overloadIdx:X}" : member.identifier);
        _memberDirectoryMap.Add(member, header);

        parent.AppendChild(header);
    }
    private void ScanStructure(Structure member, DirBuilder parent)
    {
        var header = new DirectoryBuilder("TYPE", member.identifier);

        ScanAllChildren(member, header);
        _memberDirectoryMap.Add(member, header);

        parent.AppendChild(header);
    }
    private void ScanEnum(Enumerator member, DirBuilder parent)
    {
        var header = new DirectoryBuilder("ENUM", member.identifier);
        _memberDirectoryMap.Add(member, header);

        parent.AppendChild(header);
    }
    #endregion

    #region Compress
    private void CompressAllMembers()
    {
        foreach (var i in _memberDirectoryMap)
        {
            if (i.Key is Namespace @a) CompressNamespace(a, i.Value);
            else if (i.Key is Field @b) CompressField(b, i.Value);
            else if (i.Key is FunctionGroup @c) CompressFunctionGroup(c, i.Value);
            else if (i.Key is Function @d) CompressFunction(d, i.Value);
            else if (i.Key is Structure @e) CompressStructure(e, i.Value);
            else if (i.Key is Enumerator @f) CompressEnum(f, i.Value);
        }
    }

    // These two are really basic lol
    private void CompressNamespace(Namespace member, DirBuilder dir)
    {
        AppendGlobal(member, dir);
    }
    private void CompressFunctionGroup(FunctionGroup member, DirBuilder dir)
    {
        AppendGlobal(member, dir);
    }

    // Complex things after here
    private void CompressField(Field member, DirBuilder dir)
    {
        AppendGlobal(member, dir);
    }
    private void CompressFunction(Function member, DirBuilder dir)
    {
        AppendGlobal(member, dir);
        var paramsLump = new DirectoryBuilder("META", "parameters");

        foreach (var i in member.baseParameters)
        {
            var param = new LumpBuilder("PARAM", i.name);

            if (i.type is SolvedTypeReference @solved)
            {
                param.Content.WriteByte((byte)ParameterTypes.solved);
                param.DirectoryReference(_memberDirectoryMap[solved.structure]);
            }
            else if (i.type is GenericTypeReference @generic)
            {
                param.Content.WriteByte((byte)ParameterTypes.generic);
                param.DirectoryReference(_memberDirectoryMap[generic.from]);
                param.Content.WriteU16((ushort)generic.parameterIndex);
            }
            
            paramsLump.AppendChild(param);
        }

        dir.AppendChild(paramsLump);
    }
    private void CompressStructure(Structure member, DirBuilder dir)
    {
        AppendGlobal(member, dir);
        var headerLump = new LumpBuilder("META", "structheader");

        headerLump.Content.WriteU32((uint)Math.Max(member.bitsize, member.aligin));

        dir.AppendChild(headerLump);
    }
    private void CompressEnum(Enumerator member, DirBuilder dir)
    {
        AppendGlobal(member, dir);
    }

    private void AppendGlobal(ProgramMember member, DirBuilder dir)
    {
        var global = new DirectoryBuilder("GLOBAL", member.GlobalReference);
        dir.AppendChild(global);
    }
    #endregion

    private enum ParameterTypes: byte
    {
        solved = 0,
        generic = 1
    }
}
