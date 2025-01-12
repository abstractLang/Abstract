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

    private Project _currentProject = null!;
    private Dictionary<string, List<(string kind, string identifier)>> _dependences = [];

    public ElfProgram[] DoCompression(ProgramRoot program, string outpsth)
    {
        List<ElfProgram> elfs = [];

        // Each program should result in one ELF file
        foreach (var i in program.projects)
        {
            using var projectElf = CompressProject(i.Key, i.Value, program);
            var bakedElf = ElfProgram.BakeBuilder(projectElf);

            File.WriteAllText($"{outpsth}/{i.Key}.txt", bakedElf.ToString());
            elfs.Add(bakedElf);
        }

        return [.. elfs];
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
                elfref.AppendChild(new MetaBuilder(j.kind, j.identifier));

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

                var funcdir = new DirectoryBuilder("FUNCTION", f.identifier);
                funcdir.AppendChild(new MetaBuilder("GLOBAL", member.GlobalReference));
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
            code.WriteOpCode(Base.Ret);

            code.Bake();
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
        if (node is ReturnStatementNode @return)
        {
            builder.WriteOpCode(Base.Ret);
        }
        else Console.WriteLine("Unhandled stat: " + node.GetType().Name);
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

        else if (node is BinaryExpressionNode @binary)
        {
            if (binary.ConvertLeft is not null)
                AssembleCall(builder, binary.ConvertLeft, [binary.Left]);
            else AssembleExpression(builder, binary.Left);

            if (binary.ConvertRight is not null)
                AssembleCall(builder, binary.ConvertRight, [binary.Right]);
            else AssembleExpression(builder, binary.Right);
            
            AssembleCall(builder, binary.Operate, null!);
        }

        else if (node is FunctionCallExpressionNode @funcCall)
        {
            AssembleCall(builder, funcCall.FunctionTarget, funcCall.Arguments);
        }


        else AssembleLoadValue(builder, node);
    }

    private void AssembleLoadValue(CodeBuilder builder, ExpressionNode node)
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
        foreach (var i in args ?? []) AssembleExpression(builder, i);
        // Invoke the function
        CheckUseOfReference(target);
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
    private void CheckUseOfReference(ProgramMember member)
    {
        var memberProj = member.ParentProject!;
        if (_currentProject != memberProj)
        {
            var projName = memberProj.identifier;
            if (_dependences.TryGetValue(projName, out var deps))
            {
                var r = GetMemberReferenceIdentifier(member);
                if (!deps.Contains(r)) deps.Add(r);
            }
            else _dependences.Add(projName, [GetMemberReferenceIdentifier(member)]);
        }
    }

    private static (string kind, string identifier) GetMemberReferenceIdentifier(ProgramMember member)
    {
        if (member is Function @func)
        {
            return ("FUNCTION", $"{func.GlobalReference}({string
            .Join(", ", func.parameters.Select(e => e.type))})");
        }

        else throw new Exception();
    }

}
