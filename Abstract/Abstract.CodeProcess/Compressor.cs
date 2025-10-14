using System.Diagnostics;
using System.Text;
using Abstract.CodeProcess.Core.Language.EvaluationData;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Expresions;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Macros;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Statements;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Values;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.CodeReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.FieldReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.FunctionReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin.Integer;
using Abstract.Realizer.Builder;
using Abstract.Realizer.Builder.Language.Omega;
using Abstract.Realizer.Builder.ProgramMembers;
using Abstract.Realizer.Builder.References;

using IntegerTypeReference = Abstract.Realizer.Builder.References.IntegerTypeReference;
using AbstractTypeReference = Abstract.Realizer.Builder.References.TypeReference;
using AnytypeTypeReference = Abstract.Realizer.Builder.References.AnytypeTypeReference;
using BuilderAnytypeTypeReference = Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin.AnytypeTypeReference;
using BuilderTypeReference = Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.TypeReference;
using BuilderStringTypeReference = Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin.StringTypeReference;
using BuilderReferenceTypeReference = Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin.ReferenceTypeReference;

using SliceTypeReference = Abstract.Realizer.Builder.References.SliceTypeReference;
using ReferenceTypeReference = Abstract.Realizer.Builder.References.ReferenceTypeReference;
using StringEncoding = Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin.StringEncoding;

namespace Abstract.CodeProcess;

public class Compressor
{

    private Dictionary<LangObject, ProgramMemberBuilder> _membersMap;
    
    public ProgramBuilder CompressProgramObject(ProgramObject programObject)
    {
        _membersMap = [];
        
        var realizerProgram = new ProgramBuilder();
        CreateModules(realizerProgram, programObject);
        _membersMap.TrimExcess();

        foreach (var (source, b) in _membersMap)
        {
            switch (b)
            {
                case BaseFunctionBuilder @builder:
                    UnwrapFunction(builder, (FunctionObject)source);
                    break;
                
                case StructureBuilder @builder:
                    UnwrapStruct(builder, (StructObject)source);
                    break;
            
                case StaticFieldBuilder @builder:
                    UnwrapStaticField(builder, (FieldObject)source);
                    break;
            
                case InstanceFieldBuilder @builder:
                    UnwrapInstanceField(builder, (FieldObject)source);
                    break;
            }

        }
    
        File.WriteAllTextAsync(".abs-cache/debug/compression.txt", realizerProgram.ToString());
        return realizerProgram;
    }

    private void CreateModules(ProgramBuilder prg, ProgramObject programObject)
    {
        foreach (var module in programObject.Modules)
        {
            var m = prg.AddModule(module.Name);
            foreach (var i in module.Children.Select(e => (NamespaceObject)e))
            {
                CreateMembersRecursive(m, null!, i);
            }
        }
    }
    
    private void CreateMembersRecursive(INamespaceOrStructureBuilder parent, LangObject? langParent, LangObject langObject)
    {
        switch (langObject)
        {
            case NamespaceObject @nsobj  when parent is NamespaceBuilder @parentnmsp:
            {
                var name = string.Join('.', nsobj.Global[(langParent?.Global.Length ?? 0) ..]);
                var nmsp = parentnmsp.AddNamespace(name);
                _membersMap.Add(langObject, nmsp);
                foreach (var i in nsobj.Children)
                    CreateMembersRecursive(nmsp, nsobj, i);
                
            } break;
            case StructObject @stobj     when parent is NamespaceBuilder @parentnmsp:
            {
                var name = string.Join('.', stobj.Global[(langParent?.Global.Length ?? 0) ..]);
                var s = parentnmsp.AddStructure(name);
                _membersMap.Add(langObject, s);
                
                foreach (var i in stobj.Children)
                    CreateMembersRecursive(s, stobj, i);
                
            } break;
            case TypedefObject @tdobj when parent is NamespaceBuilder @parentnmsp:
            {
                var name = string.Join('.', tdobj.Global[(langParent?.Global.Length ?? 0) ..]);
                TypeDefinitionBuilder td = parentnmsp.AddTypedef(name);
                _membersMap.Add(langObject, td);
            } break;
            case FunctionObject @fnobj   when parent is NamespaceBuilder @parentnmsp:
            {
                if (fnobj.Extern.domain is "__abstract.compiler.internal__") return;

                var name = string.Join('.', fnobj.Global[(langParent?.Global.Length ?? 0) ..]);
                
                BaseFunctionBuilder fn = fnobj.Extern == (null, null)
                    ? parentnmsp.AddFunction(name)
                    : parentnmsp.AddExternImportedFunction(name);
                
                _membersMap.Add(langObject, fn);
                
            } break;
            case FieldObject @vobj       when parent is NamespaceBuilder @parentnmsp:
            {
                var name = string.Join('.', vobj.Global[(langParent?.Global.Length ?? 0) ..]);
                var fn = parentnmsp.AddStaticField(name);
                _membersMap.Add(langObject, fn);
                
            } break;
            
            case FunctionObject @fnobj   when parent is StructureBuilder @parentstruc:
            {
                if (fnobj.Extern.domain is "__abstract.compiler.internal__") return;

                var name = string.Join('.', fnobj.Global[(langParent?.Global.Length ?? 0) ..]);
                var isvirt = fnobj.Abstract || fnobj.Virtual || fnobj.Override;
                
                BaseFunctionBuilder fn = isvirt
                    ? parentstruc.AddVirtualFunction(name, fnobj.VirtualIndex) 
                    : parentstruc.AddFunction(name);
                
                _membersMap.Add(langObject, fn);

            } break;
            case FieldObject @vobj       when parent is StructureBuilder @parentstruc:
            {
                var name = string.Join('.', vobj.Global[(langParent?.Global.Length ?? 0) ..]);
                var fn = parentstruc.AddField(name);
                _membersMap.Add(langObject, fn);
                
            } break;
            
            
            case FunctionGroupObject @fgobj:
            {
                foreach (var i in fgobj.Overloads)
                    CreateMembersRecursive(parent, langParent, i);
            } break;
            
            
            default: throw new UnreachableException();
        };
    }

    
    
    private void UnwrapFunction(BaseFunctionBuilder builder, FunctionObject source)
    {
        if (!source.Static) builder.AddParameter("self",
            new ReferenceTypeReference(new NodeTypeReference((StructureBuilder)GetObjectBuilder(source.Parent))));
        
        foreach (var p in source.Parameters)
        {
            AbstractTypeReference typeref = ConvType(p.Type);
            builder.AddParameter(p.Name, typeref);
        }
        
        builder.ReturnType = ConvType(source.ReturnType);

        switch (builder)
        {
            case FunctionBuilder @fb:
            {
                fb.ExportSymbol = source.Export;
                if (source.Body == null)
                {
                    if (builder is VirtualFunctionBuilder) break;
                    throw new Exception("Concrete static function must have a body");
                }
                UnwrapFunctionBody(fb, source);
               
            } break;
            
            case ImportedFunctionBuilder @ifb:
            {
                ifb.ImportDomain = source.Extern.domain;
                ifb.ImportSymbol = source.Extern.symbol;
                if (source.Body != null) throw new UnreachableException("Externaly imported functions cannot contains a body");
            } break;
        }
        
    }
    private void UnwrapStruct(StructureBuilder builder, StructObject source)
    {
        if (source.Extends is SolvedStructTypeReference @extendstruc)
            builder.Extends = (StructureBuilder)GetObjectBuilder(extendstruc.Struct);
        builder.VTableSize = (uint)source.VirtualTable.Length;
    }
    private void UnwrapStaticField(StaticFieldBuilder builder, FieldObject source)
    {
        builder.Type = source.Type switch
        {
            RuntimeIntegerTypeReference @inr => new IntegerTypeReference(inr.Signed, inr.BitSize),
            SolvedStructTypeReference @str => new NodeTypeReference((_membersMap[str.Struct] as StructureBuilder)!),
            UnsolvedTypeReference => throw new UnreachableException("Local type should not be unsolved at this step!"),
            _ => throw new NotImplementedException(),
        };
    }
    private void UnwrapInstanceField(InstanceFieldBuilder builder, FieldObject source)
    {
        builder.Type = ConvType(source.Type);
    }
    
    private void UnwrapFunctionBody(FunctionBuilder fnb, FunctionObject source)
    {
        var block = source.Body ?? throw new NullReferenceException();
        var builder = fnb.CreateOmegaBytecodeBlock("entry");
        
        UnwrapFunctionBody_Block(ref builder, block);
    }

    // Bruh switch hell
    private void UnwrapFunctionBody_Block(ref OmegaBlockBuilder builder, IRBlock block)
    {
        foreach (var i in block.Content)
            UnwrapFunctionBody_IRNode(ref builder, i);
    }
    private void UnwrapFunctionBody_IRNode(ref OmegaBlockBuilder builder, IRNode node)
    {
        switch (node)
        {
            case IRDefLocal @defl:
                builder.Writer.MacroDefineLocal(ConvType(defl.LocalVariable.Type));
                break;
            
            case IRInvoke @iv:
                UnwrapFunctionBody_Call(builder, iv);
                break;

            case IRIntegerLiteral @itlit:
            {
                if (itlit.Size == null) builder.Writer.LdConstIptr((ulong)itlit.Value);
                else builder.Writer.LdConstI(itlit.Size.Value, itlit.Value);
            } break;
            case IRStringLiteral @strlit:
            {
                if (strlit.Encoding is StringEncoding.Utf8 or StringEncoding.Undefined)
                    builder.Writer.LdStringUtf8(strlit.Data);

                else
                {
                    var data = strlit.Encoding switch
                    {
                        StringEncoding.Ascii => Encoding.ASCII.GetBytes(strlit.Data),
                        StringEncoding.Utf16 => Encoding.Unicode.GetBytes(strlit.Data),
                        StringEncoding.Utf32 => Encoding.UTF32.GetBytes(strlit.Data),
                        _ => throw new ArgumentOutOfRangeException()
                    };

                    builder.Writer.LdSlice(data);

                }
            } break;
            
            case IRSolvedReference @solvref:
                UnwrapFunctionBody_Load_Reference(builder, solvref.Reference);
                break;

            case IRReferenceAccess @refaccess:
            {
                UnwrapFunctionBody_Load_Reference(builder, refaccess.A);
                UnwrapFunctionBody_IRNode(ref builder, refaccess.B);
            } break;

            case IRAssign @assig:
            {
                UnwrapFunctionBody_Store(builder, assig.Target);
                UnwrapFunctionBody_IRNode(ref builder, assig.Value);
            } break;

            case IRBinaryExp @bexp:
            {
                switch (bexp.ResultType)
                {
                    case RuntimeIntegerTypeReference @rint:
                        builder.Writer.TypeInt(rint.Signed, rint.PtrSized ? null : rint.BitSize);
                        break;
                    
                    default: throw new UnreachableException();
                }
                
                switch (bexp.Operator)
                {
                    case IRBinaryExp.Operators.Add: builder.Writer.Add(); break;
                    case IRBinaryExp.Operators.Subtract: builder.Writer.Sub(); break;
                    case IRBinaryExp.Operators.Multiply: builder.Writer.Mul(); break;
                    case IRBinaryExp.Operators.Divide: builder.Writer.Div(); break;
                    case IRBinaryExp.Operators.Reminder: builder.Writer.Rem(); break;

                    case IRBinaryExp.Operators.Logical_And:
                    case IRBinaryExp.Operators.Bitwise_And: builder.Writer.And(); break;
                    case IRBinaryExp.Operators.Logical_Or:
                    case IRBinaryExp.Operators.Bitwise_Or: builder.Writer.Or(); break;
                    case IRBinaryExp.Operators.Bitwise_Xor: builder.Writer.Xor(); break;

                    case IRBinaryExp.Operators.Left_Shift:
                    case IRBinaryExp.Operators.Right_Shift:
                    default: throw new Exception();
                }
                
                UnwrapFunctionBody_IRNode(ref builder, bexp.Left);
                UnwrapFunctionBody_IRNode(ref builder, bexp.Right);
            } break;
            case IRCompareExp @cexp:
            {
                switch (cexp.ResultType)
                {
                    case RuntimeIntegerTypeReference @rint: builder.Writer.TypeInt(rint.Signed, 1); break;
                    
                    default: throw new UnreachableException();
                }
                
                switch (cexp.Operator)
                {
                    case IRCompareExp.Operators.LessThan: builder.Writer.CmpLr(); break;
                    case IRCompareExp.Operators.LessThanOrEqual: builder.Writer.CmpLe(); break;
                    case IRCompareExp.Operators.GreaterThan: builder.Writer.CmpGr(); break;
                    case IRCompareExp.Operators.GreaterThanOrEqual: builder.Writer.CmpGe(); break;

                    default: throw new Exception();
                }
                
                UnwrapFunctionBody_IRNode(ref builder, cexp.Left);
                UnwrapFunctionBody_IRNode(ref builder, cexp.Right);
            } break;
            case IRUnaryExp @unexp:
            {
                switch (unexp.Operation)
                {
                    case IRUnaryExp.UnaryOperation.Reference: UnwrapFunctionBody_Ref(builder, unexp.Value); break;
                    
                    case IRUnaryExp.UnaryOperation.PreIncrement:
                        UnwrapFunctionBody_Store(builder, unexp.Value);
                        UnwrapFunctionBody_FlagType(builder, unexp.ResultType!);
                        builder.Writer.Add();
                        UnwrapFunctionBody_IRNode(ref builder, unexp.Value);
                        builder.Writer.LdConstI(((RuntimeIntegerTypeReference)unexp.ResultType!).BitSize, 1);
                        break;
                    
                    case IRUnaryExp.UnaryOperation.PreDecrement: 
                        UnwrapFunctionBody_Store(builder, unexp.Value);
                        UnwrapFunctionBody_FlagType(builder, unexp.ResultType!);
                        builder.Writer.Sub();
                        UnwrapFunctionBody_IRNode(ref builder, unexp.Value);
                        builder.Writer.LdConstI(((RuntimeIntegerTypeReference)unexp.ResultType!).BitSize, 1);
                        break;
                    
                    default: throw new UnreachableException();
                }
            } break;
            
            case IRNewObject @newobj:
            {
                builder.Writer.LdNewObject((StructureBuilder)GetTypeReferenceBuilder(newobj.Type));
                foreach (var ia in newobj.InlineAssignments) UnwrapFunctionBody_IRNode(ref builder, ia);
            } break;
            
            case IRReturn @ret:
                builder.Writer.Ret(ret.Value != null);
                if (ret.Value != null) UnwrapFunctionBody_IRNode(ref builder, ret.Value);
                break;
            
            
            case IRIntExtend @ex:
                UnwrapFunctionBody_FlagType(builder, ex.toType);
                builder.Writer.Extend();
                UnwrapFunctionBody_IRNode(ref builder, ex.Value);
                break;
            case IRIntTrunc @tr:
                UnwrapFunctionBody_FlagType(builder, tr.toType);
                builder.Writer.Trunc();
                UnwrapFunctionBody_IRNode(ref builder, tr.Value);
                break;

            case IRIntCast @cast:
                UnwrapFunctionBody_FlagType(builder, cast.TargetType);
                builder.Writer.Conv();
                UnwrapFunctionBody_IRNode(ref builder, cast.Expression);
                break;

            case IRIf @if:
            {
                var function = builder.Parent;

                var iftrue = function.CreateOmegaBytecodeBlock("iftrue");
                var iffalse = @if.Else == null
                    ? function.CreateOmegaBytecodeBlock("continue")
                    : function.CreateOmegaBytecodeBlock("iffalse");
                var rest = @if.Else == null ? iffalse : function.CreateOmegaBytecodeBlock("continue");

                builder.Writer.BranchIf(iftrue.Index, iffalse.Index);
                UnwrapFunctionBody_IRNode(ref builder, @if.Condition);

                UnwrapFunctionBody_Block(ref iftrue, @if.Then);
                iftrue.Writer.Branch(rest.Index);

                if (@if.Else != null)
                {
                    switch (@if.Else)
                    {
                        case IRIf @subif: UnwrapFunctionBody_IRNode(ref iffalse, subif); break;
                        case IRElse @subelse: UnwrapFunctionBody_Block(ref iffalse, subelse.Then); break;
                        default: throw new UnreachableException();
                    }

                    iffalse.Writer.Branch(rest.Index);
                }

                builder = rest;
            } break;

            case IRWhile @while:
            {
                var function = builder.Parent;
                
                var check = function.CreateOmegaBytecodeBlock("check");
                var loop = function.CreateOmegaBytecodeBlock("loop");
                var step = @while.Step == null ? null : function.CreateOmegaBytecodeBlock("loopstep");
                var brk = function.CreateOmegaBytecodeBlock("break");

                builder.Writer.Branch(check.Index);

                check.Writer.BranchIf(loop.Index, brk.Index);
                UnwrapFunctionBody_IRNode(ref check, @while.Condition);
                
                if (step != null)
                {
                    loop.Writer.Branch(step.Index);
                    
                    UnwrapFunctionBody_Block(ref step, @while.Step!);
                    step.Writer.Branch(check.Index);
                }
                
                UnwrapFunctionBody_Block(ref loop, @while.Process);
                if (step == null) loop.Writer.Branch(check.Index);
                
                builder = brk;
            } break;
            
            default: throw new NotImplementedException();
        }
    }

    private void UnwrapFunctionBody_Store(OmegaBlockBuilder builder, IRExpression reference)
    {
        while (true)
        {
            switch (reference)
            {
                case IRSolvedReference @solved:
                    switch (solved.Reference)
                    {
                        case LocalReference @l:
                            builder.Writer.StLocal((short)l.Local.index);
                            break;

                        case SolvedFieldReference @f:
                            if (f.Field.Static) builder.Writer.StField((StaticFieldBuilder)GetObjectBuilder(f.Field));
                            else builder.Writer.StField((InstanceFieldBuilder)GetObjectBuilder(f.Field));
                            break;
                        
                        case ParameterReference @p:
                            if (p.Parameter.Type is not BuilderReferenceTypeReference @refe)
                                throw new Exception("Cannot assign to constant parameter");
                            builder.Writer.StLocalRef((short)p.Parameter.index);
                            break;

                        default: throw new NotImplementedException();
                    }
                    break;
                    
                case IRReferenceAccess @refaccess:
                {
                    UnwrapFunctionBody_Load_Reference(builder, refaccess.A);
                    reference = refaccess.B;
                } continue;

                default: throw new NotImplementedException();
            } break;
        }
    }
    private void UnwrapFunctionBody_Ref(OmegaBlockBuilder builder, IRExpression reference)
    {
        while (true)
        {
            switch (reference)
            {
                case IRSolvedReference @solved:
                    switch (solved.Reference)
                    {
                        case LocalReference @l:
                            builder.Writer.LdLocalRef((short)l.Local.index);
                            break;

                        case SolvedFieldReference @f:
                            throw new NotImplementedException();
                            //if (f.Field.Static) builder.Writer.LdFieldRef((StaticFieldBuilder)GetObjectBuilder(f.Field));
                            //else builder.Writer.LdFieldRef((InstanceFieldBuilder)GetObjectBuilder(f.Field));
                            //break;

                        default: throw new NotImplementedException();
                    }
                    break;
                    
                case IRReferenceAccess @refaccess:
                {
                    UnwrapFunctionBody_Load_Reference(builder, refaccess.A);
                    reference = refaccess.B;
                } continue;

                default: throw new NotImplementedException();
            } break;
        }
    }

    private void UnwrapFunctionBody_Call(OmegaBlockBuilder builder, IRInvoke invoke)
    {
        var reference = invoke.Target;
        if (reference is not IRSolvedReference @solved) throw new Exception();
        
        switch (solved.Reference)
        {
            case SolvedFunctionReference @f:
                if (f.Function.Extern.domain is "__abstract.compiler.internal__")
                {
                    switch (f.Function.Extern.symbol)
                    {
                        case "type_of":
                        {
                            builder.Writer.LdTypeRefOf();
                            UnwrapFunctionBody_IRNode(ref builder, invoke.Arguments[0]);
                        } break;
                        default: throw new Exception($"Invalid symbol {f.Function.Extern.symbol}");
                    }
                }
                else
                {
                    builder.Writer.Call((BaseFunctionBuilder)GetObjectBuilder(f.Function));
                    foreach (var i in invoke.Arguments) UnwrapFunctionBody_IRNode(ref builder, i);
                }
                break;
                    
            default: throw new NotImplementedException();
        }
    }

    
    private void UnwrapFunctionBody_Load_Reference(OmegaBlockBuilder builder, LanguageReference refe)
    {
        switch (refe)
        {
            case LocalReference @loc:
                builder.Writer.LdLocal((short)loc.Local.index);
                break;
            
            case ParameterReference @arg:
                builder.Writer.LdLocal((short)(-((short)arg.Parameter.index) - 1));
                break;

            case FieldReference @fie:
            {
                var field = ((SolvedFieldReference)fie).Field;
                
                if (((SolvedFieldReference)fie).Field.Static) builder.Writer.LdField((StaticFieldBuilder)GetObjectBuilder(field));
                else builder.Writer.LdField((InstanceFieldBuilder)GetObjectBuilder(field));
                
            } break;

            default: throw new NotFiniteNumberException();
        }
    }

    private void UnwrapFunctionBody_FlagType(OmegaBlockBuilder builder, BuilderTypeReference type)
    {
        switch (type)
        {
            case RuntimeIntegerTypeReference @intt:
                builder.Writer.TypeInt(intt.Signed, intt.PtrSized ? null : intt.BitSize);
                break;
        }
    }
    
    
    private ProgramMemberBuilder GetObjectBuilder(LangObject obj) => _membersMap[obj];
    private ProgramMemberBuilder GetTypeReferenceBuilder(BuilderTypeReference tref)
    {
        return tref switch
        {
            SolvedStructTypeReference @st => GetObjectBuilder(st.Struct),

            _ => throw new NotImplementedException(),
        };
    }

    private AbstractTypeReference ConvType(BuilderTypeReference tref)
    {
        return tref switch
        {
            RuntimeIntegerTypeReference @ri => new IntegerTypeReference(ri.Signed, ri.PtrSized ? null : ri.BitSize),
            BuilderStringTypeReference @sr => new SliceTypeReference(new IntegerTypeReference(false, 8)),
            BuilderReferenceTypeReference @rr => new ReferenceTypeReference(ConvType(rr.InternalType)),
            BooleanTypeReference => new IntegerTypeReference(false, 1),
            
            SolvedStructTypeReference @ss => new NodeTypeReference((StructureBuilder)GetObjectBuilder(ss.Struct)),
            SolvedTypedefTypeReference @st => new NodeTypeReference((TypeDefinitionBuilder)GetObjectBuilder(st.Typedef)),
            
            BuilderAnytypeTypeReference => new AnytypeTypeReference(),
            TypeTypeReference => new ProgramMemberTypeReference(),
            VoidTypeReference => null!,
            
            _ => throw new UnreachableException()
        };
    }
}
