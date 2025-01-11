using System.Text;
using Abstract.Binutils.Abs;
using Abstract.Binutils.Abs.Bytecode;
using Abstract.Binutils.Abs.Elf;
using Abstract.Build;
using Abstract.Parser.Core.Language.SyntaxNodes.Control;
using Abstract.Parser.Core.ProgMembers;
using static Abstract.Binutils.Abs.Elf.ElfBuilder;

namespace Abstract.Parser;

public class Compressor(ErrorHandler errHandler)
{

    private readonly ErrorHandler _errHandler = errHandler;

    public void DoCompression(ProgramRoot program, string outpsth)
    {
        // Each program should result in one ELF file
        foreach (var i in program.projects)
        {
            var projectElf = CompressProject(i.Key, i.Value);
            File.WriteAllText($"{outpsth}/{i.Key}.txt", projectElf.ToString());
        }

    }


    private ElfBuilder CompressProject(string projectName, Project project)
    {
        var elfBuilder = new ElfBuilder(projectName);
        var dirRoot = elfBuilder.Root;

        var projectdir = new DirectoryBuilder("PROJECT", project.identifier);

        var includedir = new DirectoryBuilder("INCLUDE", project.identifier);

        dirRoot.AppendChild(projectdir, includedir);
        
        AppendProgramMembers(projectdir, project);

        return elfBuilder;
    }

    private void CompressProgramMember(DirectoryBuilder parent, ProgramMember member)
    {
        string kind = "UNKNOWN";
        if (member is Namespace) kind = "NMSP";
        else if (member is Structure) kind = "TYPE";
        else goto Others;

        var hdir = new DirectoryBuilder(kind, member.identifier);
        AppendProgramMembers(hdir, member);
        parent.AppendChild(hdir);

        Others:

        if (member is Field @field)
        {
            var lump = new LumpBuilder("FIELD", member.identifier);
            parent.AppendChild(lump);
        }

        else if (member is FunctionGroup @funcGroup)
        {
            if (funcGroup.IsSingle)
            {
                var f = funcGroup.Overloads[0];

                var funcdir = new DirectoryBuilder("FUNCTION", f.identifier);
                CompressFuntion(funcdir, f);
                parent.AppendChild(funcdir);
            }
            else
            {
                var dir = new DirectoryBuilder("FGROUP", member.identifier);
                
                for (var i = 0; i < funcGroup.Overloads.Length; i++)
                {
                    var f = funcGroup.Overloads[i];

                    var funcdir = new DirectoryBuilder("FUNCTION", $"#overload_{i}");
                    CompressFuntion(funcdir, f);
                    dir.AppendChild(funcdir);
                }

                parent.AppendChild(dir);
            }
        }

    }

    private void CompressFuntion(DirectoryBuilder builder, Function func)
    {

        var attrb = new LumpBuilder("META", "attrb");
        var param = new ParametersLumpBuilder();
        
        foreach (var (type, name) in func.parameters)
            param.parameters.Add((type.ToString() ?? "a", name));

        builder.AppendChild(attrb);
        builder.AppendChild(param);

        // Code block
        if (func.FunctionBodyNode != null)
        {
            var code = new CodeBuilder("main");
            var data = new LumpBuilder("DATA", "main");

            builder.AppendChild(code);
            builder.AppendChild(data);

            code.WriteOpCode(Instructions.Get(Base.Nop));
            code.WriteOpCode(Instructions.Get(Base.LdConst, Types.i16), (short)100);
            code.WriteOpCode(Instructions.Get(Base.LdConst, Types.i32), 500);
            code.WriteOpCode(Instructions.Get(Base.LdConst, Types.Str), "Hello, World!");

            code.WriteOpCode(Instructions.Get(Base.Pop));
            code.WriteOpCode(Instructions.Get(Base.Pop));
            code.WriteOpCode(Instructions.Get(Base.Pop));

            code.WriteOpCode(Instructions.Get(Base.Ret));
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

}
