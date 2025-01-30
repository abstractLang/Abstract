using Abstract.Binutils.Abs.Bytecode;
using Abstract.Binutils.Abs.Elf;
using Abstract.Build;
using Abstract.CompileTarget.Core.Enums;
using Abstract.Parser.Core.Language.SyntaxNodes.Control;
using Abstract.Parser.Core.Language.SyntaxNodes.Expression;
using Abstract.Parser.Core.Language.SyntaxNodes.Statement;
using Abstract.Parser.Core.ProgData;
using Abstract.Parser.Core.ProgData.FunctionExecution;
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
        _memberDirectoryMap = [];

        // Each project should result in one ELF object
        foreach (var i in program.projects)
        {
            using var projectElf = CompressProject(i.Key, i.Value, program);
            var bakedElf = ElfProgram.BakeBuilder(projectElf);
            elfs.Add(bakedElf);
        }

        CompressAllMembers();

        _memberDirectoryMap = null!;
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

        ScanAllChildren(project, projectdir);

        if (project == program.MainProject) elfBuilder.hasentrypoint = true;

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

        AppendGlobal(member, header);
        ScanAllChildren(member, header);

        parent.AppendChild(header);
    }
    private void ScanField(Field member, DirBuilder parent)
    {
        var header = new DirectoryBuilder("FIELD", member.identifier);
        _memberDirectoryMap.Add(member, header);

        AppendGlobal(member, header);

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

        AppendGlobal(member, header);

        parent.AppendChild(header);
    }
    private void ScanStructure(Structure member, DirBuilder parent)
    {
        var header = new DirectoryBuilder("TYPE", member.identifier);
        _memberDirectoryMap.Add(member, header);

        AppendGlobal(member, header);
        ScanAllChildren(member, header);

        parent.AppendChild(header);
    }
    private void ScanEnum(Enumerator member, DirBuilder parent)
    {
        var header = new DirectoryBuilder("ENUM", member.identifier);
        _memberDirectoryMap.Add(member, header);

        AppendGlobal(member, header);

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

    // Nothing to do here (i think) lol
    private void CompressNamespace(Namespace member, DirBuilder dir)
    {
    }
    private void CompressFunctionGroup(FunctionGroup member, DirBuilder dir)
    {
    }

    // Complex things after here
    private void CompressField(Field member, DirBuilder dir)
    {
        
    }
    private void CompressFunction(Function member, DirBuilder dir)
    {
        if (member.baseParameters.Length > 0)
        {
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

        if (member.HasAnyImplementation())
        {
            var code = new CodeBuilder("main");
            var data = new LumpBuilder("DATA", "main");

            ParseContext(member.implementations[0].value.ctx, code, data);

            dir.AppendChild(code);
            if (data.Content.Length > 0) dir.AppendChild(data);
        }
    }
    private void CompressStructure(Structure member, DirBuilder dir)
    {
        var headerLump = new LumpBuilder("META", "structheader");
        headerLump.Content.WriteU32((uint)Math.Max(member.bitsize, member.aligin));
        dir.AppendChild(headerLump);

        if (member.generics.Count > 0)
        {
            var paramsLump = new DirectoryBuilder("META", "parameters");
            foreach (var i in member.generics)
            {
                var param = new LumpBuilder("PARAM", i.Key);

                if (i.Value is SolvedTypeReference @solved)
                {
                    param.Content.WriteByte((byte)ParameterTypes.solved);
                    param.DirectoryReference(_memberDirectoryMap[solved.structure]);
                }
                else if (i.Value is GenericTypeReference @generic)
                {
                    param.Content.WriteByte((byte)ParameterTypes.generic);
                    param.DirectoryReference(_memberDirectoryMap[generic.from]);
                    param.Content.WriteU16((ushort)generic.parameterIndex);
                }

                paramsLump.AppendChild(param);
            }
            dir.AppendChild(paramsLump);
        }
    }
    private void CompressEnum(Enumerator member, DirBuilder dir)
    {

    }
    #endregion

    #region Parse executable contexts
    private void ParseContext(ExecutableContext ctx, CodeBuilder code, LumpBuilder data)
    {
        if (ctx.implementationNode is BlockNode @block)
            foreach (var i in @block.Content)
            {
                if (i is ExpressionNode @e) LoadExpression(e, code, data);
                else if (i is StatementNode @s) ParseStatement(s, code, data);
            }

        else Console.WriteLine($"unhandled context node {ctx}");
    }

    private void LoadExpression(ExpressionNode node, CodeBuilder code, LumpBuilder data)
    {
        if (node is FunctionCallExpressionNode @call) ParseCall(call, code, data);

        else Console.WriteLine($"Unhandled expression {node}");
    }
    private void ParseStatement(StatementNode node, CodeBuilder code, LumpBuilder data)
    {
        Console.WriteLine($"Unhandled statement {node}");
    }

    private void ParseCall(FunctionCallExpressionNode node, CodeBuilder code, LumpBuilder data)
    {
        // FIXME handle better all these exceptions

        var target = ((AbstractCallable)node.FunctionTarget).target;
        var returntype = node.DataReference.refferToType;
        Structure returnstruct = null!;

        foreach (var i in node.Arguments)
            LoadExpression(i, code, data);

        if (returntype is SolvedTypeReference @solved)
            returnstruct = solved.structure;

        else if (returntype is GenericTypeReference @generic)
        {
            if (generic.from == target)
                throw new NotImplementedException();

            else throw new NotImplementedException();
        }

        if (returnstruct == null) throw new Exception();

        var align = (uint)Math.Max(returnstruct.bitsize, returnstruct.aligin);
        var funcdir = _memberDirectoryMap[target];
        switch (align)
        {
            case 0:   code.Call_void(funcdir); break;

            case 1:   code.Call_i1(funcdir); break;
            case 8:   code.Call_i8(funcdir); break;
            case 16:  code.Call_i16(funcdir); break;
            case 32:  code.Call_i32(funcdir); break;
            case 64:  code.Call_i64(funcdir); break;
            case 128: code.Call_i128(funcdir); break;

            // FIXME implement uptr and iptr handler here

            default:
                code.Call_i_immu8((byte)align, funcdir);
                break;
        }
    }
    #endregion

    private void AppendGlobal(ProgramMember member, DirBuilder dir)
    {
        var global = new DirectoryBuilder("GLOBAL", member.GlobalReference);
        dir.AppendChild(global);
    }

    private enum ParameterTypes: byte
    {
        solved = 0,
        generic = 1
    }
}
