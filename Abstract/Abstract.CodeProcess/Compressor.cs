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

        _currentProject = project;

        var projectdir = new DirectoryBuilder("PROJECT", project.identifier);
        //var includedir = new DirectoryBuilder("INCLUDE", "./");

        dirRoot.AppendChild(projectdir);

        if (project == program.MainProject) elfBuilder.hasentrypoint = true;

        _currentProject = null!;

        return elfBuilder;
    }
    
}
