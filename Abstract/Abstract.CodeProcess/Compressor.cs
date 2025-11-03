using System.Diagnostics;
using System.Numerics;
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
using Abstract.Realizer.Core.Intermediate.Values;

using IntegerTypeReference = Abstract.Realizer.Builder.References.IntegerTypeReference;
using AbstractTypeReference = Abstract.Realizer.Builder.References.TypeReference;
using AnytypeTypeReference = Abstract.Realizer.Builder.References.AnytypeTypeReference;
using BuilderAnytypeTypeReference = Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin.AnytypeTypeReference;
using BuilderTypeReference = Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.TypeReference;
using BuilderStringTypeReference = Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin.StringTypeReference;
using BuilderReferenceTypeReference = Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin.ReferenceTypeReference;
using BuilderNullableTypeReference = Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin.NullableTypeReference;
using NullableTypeReference = Abstract.Realizer.Builder.References.NullableTypeReference;
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
            case FieldObject @fobj       when parent is StructureBuilder @parentstruc:
            {
                var name = string.Join('.', fobj.Global[(langParent?.Global.Length ?? 0) ..]);
                if (fobj.Static) _membersMap.Add(langObject, parentstruc.AddStaticField(name));
                else _membersMap.Add(langObject, parentstruc.AddField(name));
                
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
        builder.Type = ConvType(source.Type);
        builder.Value = source.Value == null ? null : UnwrapConstant(source.Value);
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
    private bool UnwrapFunctionBody_Block(ref OmegaBlockBuilder builder, IRBlock block)
    {
        bool branched = false;
        foreach (var i in block.Content)
            branched = branched || UnwrapFunctionBody_IRNode(ref builder, i);
        return branched;
    }
    private bool UnwrapFunctionBody_IRNode(ref OmegaBlockBuilder builder, IRNode node)
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
                builder.Writer.LdConst(itlit.Size == null
                    ? new IntegerConstantValue(0, itlit.Value)
                    : new IntegerConstantValue(itlit.Size.Value, itlit.Value));
            } break;
            case IRStringLiteral @strlit:
            {
                var bytes = strlit.Encoding switch
                {
                    StringEncoding.Ascii => Encoding.ASCII.GetBytes(strlit.Data),
                    StringEncoding.Utf16 => Encoding.Unicode.GetBytes(strlit.Data),
                    StringEncoding.Utf32 => Encoding.UTF32.GetBytes(strlit.Data),
                    _ => Encoding.UTF8.GetBytes(strlit.Data)
                };
                
                var data = new SliceConstantValue(new IntegerTypeReference(false, 8),
                    bytes.Select(e => new IntegerConstantValue(8, e))
                    .ToArray<RealizerConstantValue>());
                
                var index = builder.Parent.AddDataBlock(data);
                
                builder.Writer.LdSlice(index);
            } break;
            
            case IRSolvedReference @solvref:
                UnwrapFunctionBody_Load_Reference(builder, solvref.Reference);
                break;

            case IRAccess @refaccess:
            {
                UnwrapFunctionBody_IRNode(ref builder, refaccess.A);
                UnwrapFunctionBody_IRNode(ref builder, refaccess.B);
            } break;

            case IRAssign @assig:
            {
                UnwrapFunctionBody_Store(builder, assig.Target);
                UnwrapFunctionBody_IRNode(ref builder, assig.Value);
            } break;

            case IRBinaryExp @bexp:
            {
                switch (bexp.Type)
                {
                    case RuntimeIntegerTypeReference @rint:
                        builder.Writer.TypeInt(rint.Signed, rint.PtrSized ? null : rint.BitSize);
                        break;
                    
                    default: throw new UnreachableException();
                }
                
                switch (bexp.Operator)
                {
                    case IRBinaryExp.Operators.AddWarpAround: builder.Writer.AllowOvf(); goto case IRBinaryExp.Operators.Add;
                    case IRBinaryExp.Operators.AddOnBounds: builder.Writer.Saturated(); goto case IRBinaryExp.Operators.Add;
                    case IRBinaryExp.Operators.Add: builder.Writer.Add(); break;
                    
                    case IRBinaryExp.Operators.SubtractWarpAround: builder.Writer.AllowOvf(); goto case IRBinaryExp.Operators.Subtract;
                    case IRBinaryExp.Operators.SubtractOnBounds: builder.Writer.Saturated(); goto case IRBinaryExp.Operators.Subtract;
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
                switch (cexp.Type)
                {
                    case BooleanTypeReference: builder.Writer.TypeInt(false, 1); break;
                    case RuntimeIntegerTypeReference @rint: builder.Writer.TypeInt(rint.Signed, rint.BitSize); break;
                    
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
                    {
                        UnwrapFunctionBody_Store(builder, unexp.Value);
                        UnwrapFunctionBody_FlagType(builder, unexp.Type!);
                        builder.Writer.Add();
                        UnwrapFunctionBody_IRNode(ref builder, unexp.Value);

                        var bs = ((RuntimeIntegerTypeReference)unexp.Type!).BitSize;
                        builder.Writer.LdConst(new IntegerConstantValue(bs, BigInteger.One));

                        //UnwrapFunctionBody_IRNode(ref builder, unexp.Value);
                    } break;

                    case IRUnaryExp.UnaryOperation.PreDecrement:
                    {
                        UnwrapFunctionBody_Store(builder, unexp.Value);
                        UnwrapFunctionBody_FlagType(builder, unexp.Type!);
                        builder.Writer.Sub();
                        UnwrapFunctionBody_IRNode(ref builder, unexp.Value);

                        var bs = ((RuntimeIntegerTypeReference)unexp.Type!).BitSize;
                        builder.Writer.LdConst(new IntegerConstantValue(bs, BigInteger.One));

                        //UnwrapFunctionBody_IRNode(ref builder, unexp.Value);

                    } break;

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
                return true;
            
            
            case IRIntExtend @ex:
                UnwrapFunctionBody_FlagType(builder, ex.Type);
                builder.Writer.Extend();
                UnwrapFunctionBody_IRNode(ref builder, ex.Value);
                break;
            case IRIntTrunc @tr:
                UnwrapFunctionBody_FlagType(builder, tr.Type);
                builder.Writer.Trunc();
                UnwrapFunctionBody_IRNode(ref builder, tr.Value);
                break;

            case IRIntCast @cast:
                UnwrapFunctionBody_FlagType(builder, cast.Type);
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

                if (!UnwrapFunctionBody_Block(ref iftrue, @if.Then))
                    iftrue.Writer.Branch(rest.Index);

                if (@if.Else != null)
                {
                    bool branch = false;
                    switch (@if.Else)
                    {
                        case IRIf @subif: UnwrapFunctionBody_IRNode(ref iffalse, subif); break;
                        case IRElse @subelse: branch = UnwrapFunctionBody_Block(ref iffalse, subelse.Then); break;
                        default: throw new UnreachableException();
                    }
                    if (!branch) iffalse.Writer.Branch(rest.Index);
                }

                builder = rest;
            } break;

            case IRWhile @while:
            {
                var function = builder.Parent;
                
                if (@while.Define != null) UnwrapFunctionBody_Block(ref builder, @while.Define);
                
                var check = function.CreateOmegaBytecodeBlock("check");
                var loop = function.CreateOmegaBytecodeBlock("loop");
                var step = @while.Step == null ? null : function.CreateOmegaBytecodeBlock("loopstep");
                var brk = function.CreateOmegaBytecodeBlock("break");

                
                builder.Writer.Branch(check.Index);

                check.Writer.BranchIf(loop.Index, brk.Index);
                UnwrapFunctionBody_IRNode(ref check, @while.Condition);
                
                if (step != null)
                {
                    UnwrapFunctionBody_Block(ref step, @while.Step!);
                    step.Writer.Branch(check.Index);
                }
                
                UnwrapFunctionBody_Block(ref loop, @while.Process);
                if (step == null) loop.Writer.Branch(check.Index);
                else loop.Writer.Branch(step.Index);
                
                builder = brk;
            } break;
            
            default: throw new NotImplementedException();
        }

        return false;
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
                    
                case IRAccess @refaccess:
                {
                    UnwrapFunctionBody_IRNode(ref builder, refaccess.A);
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
                    
                case IRAccess @refaccess:
                {
                    UnwrapFunctionBody_IRNode(ref builder, refaccess.A);
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
                        case "int_to_ref":
                        {
                            builder.Writer
                                .TypeReference()
                                .Conv();
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

            case SelfReference: builder.Writer.LdSelf(); break;

            case MetaReference @m:
            {
                switch (m.Type)
                {
                    case MetaReference.MetaReferenceType.Name:
                        builder.Writer.LdMeta(OmegaMetadataKind.StructureName); break;
                    case MetaReference.MetaReferenceType.ByteSize:
                        builder.Writer.LdMeta(OmegaMetadataKind.StructureSizeBytes); break;
                    case MetaReference.MetaReferenceType.BitSize:
                        builder.Writer.LdMeta(OmegaMetadataKind.StructureSizeBits); break;
                    case MetaReference.MetaReferenceType.Alignment:
                        builder.Writer.LdMeta(OmegaMetadataKind.StructureAlign); break;
                    
                    default: throw new ArgumentOutOfRangeException();
                }
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
            
            case BuilderReferenceTypeReference @reft:
                builder.Writer.TypeReference();
                break;
            
            default: throw new UnreachableException();
        }
    }


    private RealizerConstantValue UnwrapConstant(IRExpression exp)
    {
        return exp switch
        {
            IRIntegerLiteral @intlit => new IntegerConstantValue(intlit.Size ?? 0, intlit.Value),
            IRNullLiteral @null => new NullConstantValue(),
            _ => throw new UnreachableException(),
        };
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
            BuilderNullableTypeReference @nullable => new NullableTypeReference(ConvType(nullable.InternalType)),
            BooleanTypeReference => new IntegerTypeReference(false, 1),
            
            SolvedStructTypeReference @ss => new NodeTypeReference((StructureBuilder)GetObjectBuilder(ss.Struct)),
            SolvedTypedefTypeReference @st => new NodeTypeReference((TypeDefinitionBuilder)GetObjectBuilder(st.Typedef)),
            
            BuilderAnytypeTypeReference => new AnytypeTypeReference(),
            TypeTypeReference => new ProgramMemberTypeReference(),
            VoidTypeReference => null!,
            NoReturnTypeReference => new NoreturnTypeReference(),
            
                
            _ => throw new UnreachableException()
        };
    }
}
