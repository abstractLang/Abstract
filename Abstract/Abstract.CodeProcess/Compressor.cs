using System.Diagnostics;
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
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin.Integer;
using Abstract.Realizer.Builder;
using Abstract.Realizer.Builder.Language.Omega;
using Abstract.Realizer.Builder.ProgramMembers;
using Abstract.Realizer.Builder.References;
using IntegerTypeReference = Abstract.Realizer.Builder.References.IntegerTypeReference;
using AbstractTypeReference = Abstract.Realizer.Builder.References.TypeReference;
using BuilderTypeReference = Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.TypeReference;

namespace Abstract.CodeProcess;

public class Compressor
{

    private Dictionary<LangObject, ProgramMemberBuilder> _membersMap;
    
    public ProgramBuilder CompressProgramObject(ProgramObject programObject)
    {
        _membersMap = [];
        var realizerModule = new ProgramBuilder();
        
        CreateModules(realizerModule, programObject);
        _membersMap.TrimExcess();

        foreach (var (source, b) in _membersMap)
        {
            switch (b)
            {
                case BaseFunctionBuilder @builder:
                    UnwrapFunction(builder, (FunctionObject)source);
                    break;
                
                case StaticFieldBuilder @builder:
                    UnwrapStaticField(builder, (FieldObject)source);
                    break;
                
                case InstanceFieldBuilder @builder:
                    UnwrapInstanceField(builder, (FieldObject)source);
                    break;
            }

        }
        
        File.WriteAllTextAsync(".abs-cache/debug/compression.txt", realizerModule.ToString());
        return realizerModule;
    }

    private void CreateModules(ProgramBuilder prg, ProgramObject programObject)
    {
        foreach (var i in programObject.Namespaces)
        {
            var m = prg.AddModule(null!);
            CreateMembersRecursive(m, null!, i);
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
            case FunctionObject @fnobj   when parent is NamespaceBuilder @parentnmsp:
            {
                var name = string.Join('.', fnobj.Global[(langParent?.Global.Length ?? 0) ..]);
                
                BaseFunctionBuilder fn = fnobj.Extern == (null, null)
                    ? parentnmsp.AddFunction(name)
                    : parentnmsp.AddExternImportedFunction(name);
                
                _membersMap.Add(langObject, fn);
                
            } break;
            case FieldObject @vobj    when parent is NamespaceBuilder @parentnmsp:
            {
                var name = string.Join('.', vobj.Global[(langParent?.Global.Length ?? 0) ..]);
                var fn = parentnmsp.AddStaticField(name);
                _membersMap.Add(langObject, fn);
                
            } break;
            
            case FunctionObject @fnobj   when parent is StructureBuilder @parentstruc:
            {
                var name = string.Join('.', fnobj.Global[(langParent?.Global.Length ?? 0) ..]);
                var fn = parentstruc.AddField(name);
                _membersMap.Add(langObject, fn);
                
            } break;
            case FieldObject @vobj    when parent is StructureBuilder @parentstruc:
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

        foreach (var p in source.Parameters)
        {
            AbstractTypeReference typeref = p.Type switch
            {
                RuntimeIntegerTypeReference @inr => new IntegerTypeReference(inr.Signed, inr.BitSize),
                SolvedStructTypeReference @str => new NodeTypeReference((_membersMap[str.Struct] as StructureBuilder)!),
                UnsolvedTypeReference => throw new UnreachableException("parameter type should not be unsolved at this step!"),
                _ => throw new NotImplementedException(),
            };
            builder.AddParameter(p.Name, typeref);
        }

        switch (builder)
        {
            case FunctionBuilder @fb:
            {
                
                if (source.Body == null) return;
        
                // foreach (var l in source.Body.Locals)
                // {
                //     AbstractTypeReference typeref = l.Type switch
                //     {
                //         RuntimeIntegerTypeReference @inr => new IntegerTypeReference(inr.Signed, inr.BitSize),
                //         SolvedStructTypeReference @str => new NodeTypeReference((_membersMap[str.Struct] as StructureBuilder)!),
                //         UnsolvedTypeReference => throw new UnreachableException("Local type should not be unsolved at this step!"),
                //         _ => throw new NotImplementedException(),
                //     };
                //     
                // }
                UnwrapFunctionBody(fb.GetOrCreateOmegaBuilder(), source);
                
            } break;
            
            case ImportedFunctionBuilder @ifb:
            {
                ifb.ImportDomain = source.Extern.domain;
                ifb.ImportSymbol = source.Extern.symbol;
                if (source.Body != null) throw new UnreachableException("Externaly imported functions cannot contains a body");
            } break;
        }
        

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
        builder.Type = source.Type switch
        {
            RuntimeIntegerTypeReference @inr => new IntegerTypeReference(inr.Signed, inr.BitSize),
            SolvedStructTypeReference @str => new NodeTypeReference((_membersMap[str.Struct] as StructureBuilder)!),
            UnsolvedTypeReference => throw new UnreachableException("Local type should not be unsolved at this step!"),
            _ => throw new NotImplementedException(),
        };
    }
    
    private void UnwrapFunctionBody(OmegaBytecodeBuilder builder, FunctionObject source)
    {
        var block = source.Body ?? throw new NullReferenceException();
        UnwrapFunctionBody_IRNode(builder, block);
    }

    // Bruh switch hell
    private void UnwrapFunctionBody_IRNode(OmegaBytecodeBuilder builder, IRNode node)
    {
        switch (node)
        {
            case IRBlock @b:
                foreach (var i in  b.Content) UnwrapFunctionBody_IRNode(builder, i);
                break;
            
            case IRDefLocal @defl:
                builder.Writer.MacroDefineLocal(ConvType(defl.LocalVariable.Type));
                break;
            
            case IRInvoke @iv:
                UnwrapFunctionBody_Call(builder, iv.Target);
                foreach (var i in iv.Arguments) UnwrapFunctionBody_IRNode(builder, i);

                break;

            case IRIntegerLiteral @itlit:
            {
                if (itlit.PtrSized) builder.Writer.LdConstIptr((ulong)itlit.Value);
                //else if (itlit.Size == null) throw new Exception("Integer literal should have a assigned size");
                builder.Writer.LdConstI((byte)(itlit.Size ?? 255), itlit.Value);
            } break;

            case IRSolvedReference @solvref:
                UnwrapFunctionBody_Load_Reference(builder, solvref.Reference);
                break;

            case IRReferenceAccess @refaccess:
            {
                UnwrapFunctionBody_Load_Reference(builder, refaccess.A);
                UnwrapFunctionBody_IRNode(builder, refaccess.B);
            } break;

            case IRAssign @assig:
            {
                UnwrapFunctionBody_Store(builder, assig.Target);
                UnwrapFunctionBody_IRNode(builder, assig.Value);
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
                    default: throw new Exception();
                }
                
                UnwrapFunctionBody_IRNode(builder, bexp.Left);
                UnwrapFunctionBody_IRNode(builder, bexp.Right);
            } break;

            case IRNewObject @newobj:
            {
                builder.Writer.LdNewObject((TypeBuilder)GetTypeReferenceBuilder(newobj.Type));
                foreach (var ia in newobj.InlineAssignments) UnwrapFunctionBody_IRNode(builder, ia);
            } break;
            
            case IRReturn @ret:
                builder.Writer.Ret(ret.Value != null);
                if (ret.Value != null) UnwrapFunctionBody_IRNode(builder, ret.Value);
                break;
            
            case IRSignCast @sigcast:
                builder.Writer.Sigcast(sigcast.Signed);
                UnwrapFunctionBody_IRNode(builder, sigcast.Value);
                break;
            case IRIntExtend @ex:
                UnwrapFunctionBody_FlagType(builder, ex.toType);
                builder.Writer.Extend();
                UnwrapFunctionBody_IRNode(builder, ex.Value);
                break;
            case IRIntTrunc @tr:
                UnwrapFunctionBody_FlagType(builder, tr.toType);
                builder.Writer.Trunc();
                UnwrapFunctionBody_IRNode(builder, tr.Value);
                break;
            
            
            default: throw new NotImplementedException();
        }
    }

    private void UnwrapFunctionBody_Store(OmegaBytecodeBuilder builder, IRExpression reference)
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

    private void UnwrapFunctionBody_Call(OmegaBytecodeBuilder builder, IRExpression reference)
    {
        if (reference is not IRSolvedReference @solved) throw new Exception();
        
        switch (solved.Reference)
        {
            case SolvedFunctionReference @f:
                builder.Writer.Call((BaseFunctionBuilder)GetObjectBuilder(f.Function));
                break;
                    
            default: throw new NotImplementedException();
        }
    }

    
    private void UnwrapFunctionBody_Load_Reference(OmegaBytecodeBuilder builder, LanguageReference refe)
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

    private void UnwrapFunctionBody_FlagType(OmegaBytecodeBuilder builder, BuilderTypeReference type)
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
        switch (tref)
        {
            case SolvedStructTypeReference @ss: return new NodeTypeReference((StructureBuilder)GetObjectBuilder(ss.Struct));
            case RuntimeIntegerTypeReference @ri: return new IntegerTypeReference(ri.Signed, ri.PtrSized ? null : ri.BitSize);
            
            default: throw new UnreachableException();
        }
    }
}