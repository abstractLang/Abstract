using Abstract.Binutils.Abs.Bytecode;
using Abstract.Binutils.Abs.Elf;
using Abstract.Build;
using Abstract.CompileTarget.Core.Enums;
using Abstract.Parser.Core.Language.SyntaxNodes.Expression;
using Abstract.Parser.Core.Language.SyntaxNodes.Statement;
using Abstract.Parser.Core.Language.SyntaxNodes.Value;
using Abstract.Parser.Core.ProgData;
using Abstract.Parser.Core.ProgData.DataReference;
using Abstract.Parser.Core.ProgData.FunctionExecution;
using Abstract.Parser.Core.ProgMembers;
using static Abstract.Binutils.Abs.Elf.ElfBuilder;

namespace Abstract.Parser;

public class Compressor(ErrorHandler errHandler)
{

    private readonly ErrorHandler _errHandler = errHandler;

    private Project _currentProject = null!;
    private Dictionary<string,List<Dependence>> _dependences = [];

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
        _dependences.Clear();

        var projectdir = new DirectoryBuilder("PROJECT", project.identifier);
        //var includedir = new DirectoryBuilder("INCLUDE", "./");

        dirRoot.AppendChild(projectdir);
        AppendProgramMembers(projectdir, project);
        dirRoot.AppendChild(CompressDependences());

        if (project == program.MainProject) elfBuilder.hasentrypoint = true;
        if (_dependences.Count > 0) elfBuilder.hasdependence = true;

        _currentProject = null!;

        return elfBuilder;
    }
    

    private DirectoryBuilder[] CompressDependences()
    {
        List<DirectoryBuilder> importDir = [];

        foreach (var i in _dependences)
        {
            var elfref = new DirectoryBuilder("IMPORT", i.Key);
            foreach (var j in i.Value)
            {
                var parent = new MetaBuilder('I' + j.kind, j.name);
                elfref.AppendChild(parent);

                if (j.source is Function @func)
                {
                    for (var k = 0; k < func.baseParameters.Length; k++)
                    {
                        var (type, name) = func.baseParameters[k];
                        var p = new DirectoryBuilder("PARAM", $"{k:X4}");
                        
                        p.AppendChild(new DirectoryBuilder("TYPE", type.ToString()
                            ?? throw new Exception()));
                        p.AppendChild(new DirectoryBuilder("NAME", name));

                        parent.AppendChild(p);

                    }
                 
                    parent.AppendChild(new DirectoryBuilder("RET", func.baseReturnType.ToString() ?? throw new Exception()));
                }
            }

            importDir.Add(elfref);
        }

        return [.. importDir];
    }

    private void CompressProgramMember(DirectoryBuilder parent, ProgramMember member)
    {
        string kind = "UNKNOWN";
        if (member is Namespace) kind = "NMSP";
        else if (member is Structure) kind = "TYPE";
        else goto Others;

        var hdir = new DirectoryBuilder(kind, member.identifier);
        hdir.AppendChild(new MetaBuilder("GLOBAL", member.GlobalReference));

        AppendProgramMembers(hdir, member);
        parent.AppendChild(hdir);

        Others:

        if (member is Field @field)
        {
            var lump = new LumpBuilder("FIELD", member.identifier);
            lump.AppendChild(new MetaBuilder("GLOBAL", member.GlobalReference));
            parent.AppendChild(lump);
        }

        else if (member is FunctionGroup @funcGroup)
        {
            if (funcGroup.IsSingle)
            {
                var f = funcGroup.Overloads[0];

                var funcdir = new DirectoryBuilder("FUNC", f.identifier);
                funcdir.AppendChild(new MetaBuilder("GLOBAL", member.GlobalReference));
                CompressFunction(funcdir, f);
                parent.AppendChild(funcdir);
            }
            else
            {
                var dir = new DirectoryBuilder("FGROUP", member.identifier);
                
                for (var i = 0; i < funcGroup.Overloads.Length; i++)
                {
                    var f = funcGroup.Overloads[i];

                    var funcdir = new DirectoryBuilder("FUNC", $"#overload_{i}");
                    CompressFunction(funcdir, f);
                    dir.AppendChild(funcdir);
                }

                parent.AppendChild(dir);
            }
        }

    }

    private void CompressFunction(DirectoryBuilder builder, Function func)
    {

        var attrb = new LumpBuilder("META", "attrb");
        
        for(var i = 0; i < func.baseParameters.Length; i++)
        {
            var param = new DirectoryBuilder("PARAM", $"{i:X4}");

            var (type, name) = func.baseParameters[i];
            CheckUseOfReference(type);

            param.AppendChild(new DirectoryBuilder("TYPE", type.ToString() ?? throw new Exception()));
            param.AppendChild(new DirectoryBuilder("NAME", name));

            builder.AppendChild(param);
        }
        if (func.baseReturnType != null)
            builder.AppendChild(new DirectoryBuilder("RET", func.baseReturnType.ToString() ?? throw new Exception()));
        else Console.WriteLine($"{func} returning type was null");

        builder.AppendChild(attrb);

        // Code block
        if (func.HasAnyImplementation())
        {
            var bodyMeta = func.GetImplementation([.. func.baseParameters.Select(e => e.type)]).ctx;
            var body = func.FunctionBodyNode;

            var code = new CodeBuilder("main");
            var data = new LumpBuilder("DATA", "main");

            builder.AppendChild(code);
            builder.AppendChild(data);

            // TODO
            
        }
    }

    private void AppendProgramMembers(DirectoryBuilder builder, ProgramMember member)
    {
        foreach (var i in member.ChildrenNamespaces)
            CompressProgramMember(builder, i);

        foreach (var i in member.ChildrenFields)
            CompressProgramMember(builder, i);
        
        foreach (var i in member.ChildrenFunctions)
            CompressProgramMember(builder, i);

        foreach (var i in member.ChildrenTypes)
            CompressProgramMember(builder, i);

        foreach (var i in member.ChildrenEnums)
            CompressProgramMember(builder, i);
    }


    private void CheckUseOfReference(ProgramMember member)
    {
        var memberProj = member.ParentProject!;
        if (_currentProject != memberProj)
        {
            var projName = memberProj.identifier;
            if (_dependences.TryGetValue(projName, out var deps))
            {
                var r = new Dependence(GetMemberReferenceIdentifier(member), member);
                if (!deps.Contains(r)) deps.Add(r);
            }
            else _dependences.Add(projName,
                [new Dependence(GetMemberReferenceIdentifier(member), member)]);
        }
    }
    private void CheckUseOfReference(TypeReference type)
    {
        if (type is SolvedTypeReference @solved)
        {
            var memberProj = solved.structure.ParentProject!;
            if (_currentProject != memberProj)
            {
                var projName = memberProj.identifier;
                if (_dependences.TryGetValue(projName, out var deps))
                {
                    var r = new Dependence(GetMemberReferenceIdentifier(solved.structure), solved.structure);
                    if (!deps.Contains(r)) deps.Add(r);
                }
                else _dependences.Add(projName,
                    [new Dependence(GetMemberReferenceIdentifier(solved.structure), solved.structure)]);
            }
        }
        else if (type is GenericTypeReference) { /* ignore for now lol */ }

        else return;//throw new NotImplementedException();
    }

    private static (string kind, string identifier) GetMemberReferenceIdentifier(ProgramMember member)
    {
        if (member is Function @func)
        {
            return ("FUNC", $"{func.GlobalReference}({string
            .Join(", ", func.baseParameters.Select(e => e.type))})");
        }

        else if (member is Structure @struc)
        {
            return ("TYPE", $"{struc.GlobalReference}");
        }

        else throw new NotImplementedException();
    }


    private struct Dependence((string kind, string name) identifier, ProgramMember src) {
        public string kind = identifier.kind;
        public string name = identifier.name;
        public ProgramMember source = src;
    }
}
