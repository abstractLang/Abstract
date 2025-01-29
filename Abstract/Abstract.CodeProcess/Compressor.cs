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

        var projectdir = new DirectoryBuilder("PROJECT", project.identifier);
        //var includedir = new DirectoryBuilder("INCLUDE", "./");

        dirRoot.AppendChild(projectdir);

        CompressAllChildren(project, projectdir);

        if (project == program.MainProject) elfBuilder.hasentrypoint = true;

        _currentProject = null!;
        _builder = null!;

        return elfBuilder;
    }

    private void CompressAllChildren(ProgramMember parent, DirBuilder parentDir)
    {
        foreach (var i in parent.ChildrenNamespaces) CompressNamespace(i, parentDir);
        foreach (var i in parent.ChildrenFields) CompressFields(i, parentDir);
        foreach (var i in parent.ChildrenFunctions) CompressFunctionGroup(i, parentDir);
        foreach (var i in parent.ChildrenTypes) CompressStructure(i, parentDir);
        foreach (var i in parent.ChildrenEnums) CompressEnum(i, parentDir);
    }


    private void CompressNamespace(Namespace member, DirBuilder parent)
    {
        var header = new DirectoryBuilder("NMSP", member.identifier);

        CompressAllChildren(member, header);

        parent.AppendChild(header);
    }
    private void CompressFields(Field member, DirBuilder parent)
    {
        var header = new DirectoryBuilder("FIELD", member.identifier);

        parent.AppendChild(header);
    }
    private void CompressFunctionGroup(FunctionGroup member, DirBuilder parent)
    {
        var header = new DirectoryBuilder("FGROUP", member.identifier);


        parent.AppendChild(header);
    }
    private void CompressFunction(Function member, DirBuilder parent)
    {
        var header = new DirectoryBuilder("FUNC", member.identifier);


        parent.AppendChild(header);
    }
    private void CompressStructure(Structure member, DirBuilder parent)
    {
        var header = new DirectoryBuilder("TYPE", member.identifier);

        CompressAllChildren(member, header);

        parent.AppendChild(header);
    }
    private void CompressEnum(Enumerator member, DirBuilder parent)
    {
        var header = new DirectoryBuilder("ENUM", member.identifier);


        parent.AppendChild(header);
    }
}
