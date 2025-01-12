using System.Text;
using Abstract.Binutils.Abs;
using Abstract.Binutils.Abs.Bytecode;
using Abstract.Binutils.Abs.Elf;
using Abstract.Build;
using Abstract.Parser.Core.Language.SyntaxNodes.Control;
using Abstract.Parser.Core.Language.SyntaxNodes.Expression;
using Abstract.Parser.Core.Language.SyntaxNodes.Statement;
using Abstract.Parser.Core.Language.SyntaxNodes.Value;
using Abstract.Parser.Core.ProgData;
using Abstract.Parser.Core.ProgData.DataReference;
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
            var projectElf = CompressProject(i.Key, i.Value, program);
            File.WriteAllText($"{outpsth}/{i.Key}.txt", projectElf.ToString());
        }

    }


    private ElfBuilder CompressProject(string projectName, Project project, ProgramRoot program)
    {
        var elfBuilder = new ElfBuilder(projectName);
        var dirRoot = elfBuilder.Root;

        var projectdir = new DirectoryBuilder("PROJECT", project.identifier);
        //var includedir = new DirectoryBuilder("INCLUDE", "./");



        dirRoot.AppendChild(projectdir);
        dirRoot.AppendChild(CompressDependences(project, program));
        //dirRoot.AppendChild(includedir);
        
        AppendProgramMembers(projectdir, project);

        return elfBuilder;
    }

    private DirectoryBuilder[] CompressDependences(Project project, ProgramRoot prog)
    {
        List<DirectoryBuilder> importDir = [];

        Dictionary<string, List<ProgramMember>> elfs = [];

        foreach (var i in project.Dependences)
        {
            var root = i.GlobalReference.Step.step;
            if (!elfs.ContainsKey(root)) elfs.Add(root, []);
            elfs[root].Add(i);
        }

        foreach (var i in elfs)
        {
            var elfref = new DirectoryBuilder("IMPORT", i.Key);

            var depproj = prog.projects[i.Key];
            foreach (var j in i.Value)
            {
                CompressDependenceProgramMember(elfref, j);
            }

            importDir.Add(elfref);
        }

        return [.. importDir];
    }
    private void CompressDependenceProgramMember(DirectoryBuilder parent, ProgramMember member)
    {
        if (member is Namespace @nmsp)
        {
            var dir = new DirectoryBuilder("NMSP", nmsp.GlobalReference);
            parent.AppendChild(dir);
        }
        else if (member is Structure @struc)
        {
            var dir = new DirectoryBuilder("TYPE", struc.GlobalReference);
            parent.AppendChild(dir);
        }
        else if (member is FunctionGroup @fGroup)
        {
            var dir = new DirectoryBuilder("FGROUP", fGroup.GlobalReference);
            parent.AppendChild(dir);
        }
        else if (member is Function @func)
        {
            if (parent.TryGetChild("FGROUP", func.GlobalReference, out var group))
            {
                var dir = new DirectoryBuilder("FUNCTION", $"overload_{group.ChildrenCount}");
                var param = new ParametersLumpBuilder();

                foreach (var i in func.parameters)
                    param.parameters.Add((i.type.ToString()!, i.name));

                dir.AppendChild(param);
                group.AppendChild(dir);
            }
            else
            {
                var dir = new DirectoryBuilder("FUNCTION", func.GlobalReference);
                var param = new ParametersLumpBuilder();

                foreach (var i in func.parameters)
                    param.parameters.Add((i.type.ToString()!, i.name));

                dir.AppendChild(param);
                parent.AppendChild(dir);
            }
        }
    
        else throw new Exception();
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
        if (func.FunctionBodyNode?.EvaluatedData != null)
        {
            var bodyMeta = func.FunctionBodyNode.EvaluatedData;
            var body = func.FunctionBodyNode;

            var code = new CodeBuilder("main");
            var data = new LumpBuilder("DATA", "main");

            builder.AppendChild(code);
            builder.AppendChild(data);

            code.WriteOpCode(Base.EnterFrame, (short)bodyMeta.LocalVariables.Count);

            foreach (var i in body.Content)
            {
                if (i is StatementNode @statement) AssembleStatement(code, statement);
                else if (i is ExpressionNode @expression) AssembleExpression(code, expression);
            }

            code.WriteOpCode(Base.LeaveFrame);

            // code.WriteOpCode(Instructions.Get(Base.Nop));
            // code.WriteOpCode(Instructions.Get(Base.LdConst, Types.i16), (short)100);
            // code.WriteOpCode(Instructions.Get(Base.LdConst, Types.i32), 500);
            // code.WriteOpCode(Instructions.Get(Base.LdConst, Types.Str), "Hello, World!");

            // code.WriteOpCode(Instructions.Get(Base.Pop));
            // code.WriteOpCode(Instructions.Get(Base.Pop));
            // code.WriteOpCode(Instructions.Get(Base.Pop));

            // code.WriteOpCode(Instructions.Get(Base.Ret));
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


    private void AssembleStatement(CodeBuilder builder, StatementNode node)
    {
        Console.WriteLine(node.GetType().Name);
    }

    private void AssembleExpression(CodeBuilder builder, ExpressionNode node)
    {
        if (node is AssignmentExpressionNode @assignment)
        {
            if (assignment.ConvertRight is not null)
                AssembleCall(builder, assignment.ConvertRight, [assignment.Right]);
            else AssembleExpression(builder, assignment.Right);
            
            if (assignment.Left.DataReference is LocalDataRef @localData)
                builder.WriteOpCode(Base.SetLocal, (ushort)localData.GetIndex);
            
            else throw new Exception();
        }

        else if (node is FunctionCallExpressionNode @funcCall)
        {
            AssembleCall(builder, funcCall.FunctionTarget, funcCall.Arguments);
        }


        else AssembleLoadValie(builder, node);
    }

    private void AssembleLoadValie(CodeBuilder builder, ExpressionNode node)
    {
        if (node is IdentifierCollectionNode @identifier)
        {
            if (identifier.DataReference is LocalDataRef @local)
                builder.WriteOpCode(Base.LdLocal, (ushort)local.GetIndex);

            else throw new Exception();
        }

        else if (node is StringLiteralNode @strlit)
            builder.WriteOpCode(Base.LdConst, Types.Str, strlit.Value);

        else Console.WriteLine("Unhandled exp: " + node.GetType().Name);
    }


    private void AssembleCall(CodeBuilder builder, Function target, ExpressionNode[] args)
    {
        // Check if it is a hardcoded call
        var fref = target.GlobalReference.tokens;
        if (fref[0] == "Std")
        {
            if (fref[1] == "Compilation")
            {

                if (fref.Length == 5 && fref[2] == "Types"
                && fref[3].StartsWith("Comptime") && fref[4].StartsWith("as"))
                {
                    switch (fref[3..])
                    {
                        case ["ComptimeString", "asstring"]:
                            var value = (StringLiteralNode)args[0];
                            builder.WriteOpCode(Base.LdConst, Types.Str, value.Value);
                            return;

                        default: throw new Exception();
                    }
                }

            }
            else if (fref[1] == "Types" && fref.Length == 4)
            {
                if (fref[2].StartsWith("UnsignedInteger") || fref[2].StartsWith("SignedInteger"))
                {
                    if ((fref[3].StartsWith("cast_i")
                        || fref[3].StartsWith("cast_u"))
                        && target.parameters.Length == 1)
                    {
                        builder.WriteOpCode(Base.Conv,
                            GetAsmType(args[0].DataReference.refferToType),
                            GetAsmType(target.returnType));
                        
                        return;
                    }
                }
            }
        }


        // Load arguments on stack
        foreach (var i in args) AssembleExpression(builder, i);
        // Invoke the function
        builder.WriteOpCode(Base.Call, GetAsmType(target.returnType),
            $"{target.GlobalReference}({string.Join(", ", target.parameters.Select(e => e.type))})");

    }

    private static Types GetAsmType(TypeReference typeRef)
    {
        if (typeRef is SolvedTypeReference @solved)
        {
            if (!solved.structure.GlobalReference.ToString().StartsWith("Std.Types."))
                return Types.Struct;
            
            return solved.structure.GlobalReference.tokens[2..] switch {

                ["SignedInteger8"] => Types.i8,
                ["SignedInteger16"] => Types.i16,
                ["SignedInteger32"] => Types.i32,
                ["SignedInteger64"] => Types.i64,
                ["SignedInteger128"] => Types.i128,

                ["UnsignedInteger8"] => Types.u8,
                ["UnsignedInteger16"] => Types.u16,
                ["UnsignedInteger32"] => Types.u32,
                ["UnsignedInteger64"] => Types.u64,
                ["UnsignedInteger128"] => Types.u128,

                ["Single"] => Types.f32,
                ["Double"] => Types.f64,

                ["String"] => Types.Str,
                ["Character"] => Types.Str,

                ["Boolean"] => Types.Bool,

                ["Void"] => Types.Void,
                ["null"] => Types.Null,

                ["Collections", "Array"] => Types.Arr,

                _ => Types.Struct
            };
        }
        else return Types.Struct;;
    }

}
