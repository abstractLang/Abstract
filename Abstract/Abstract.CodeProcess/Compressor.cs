using System.Diagnostics;
using Abstract.CodeProcess.Core.Language.EvaluationData;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Expresions;
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
    
    public void CompressProgramObject(ProgramObject programObject)
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
                
                case FieldBuilder @builder:
                    UnwrapField(builder, (FieldObject)source);
                    break;
            }

        }
        
        File.WriteAllText(".abs-cache/debug/compression.txt", realizerModule.GetRoot().ToString());
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
                
                BaseFunctionBuilder fn = fnobj.Extern == null
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
        
                foreach (var l in source.Body.Locals)
                {
                    AbstractTypeReference typeref = l.Type switch
                    {
                        RuntimeIntegerTypeReference @inr => new IntegerTypeReference(inr.Signed, inr.BitSize),
                        SolvedStructTypeReference @str => new NodeTypeReference((_membersMap[str.Struct] as StructureBuilder)!),
                        UnsolvedTypeReference => throw new UnreachableException("Local type should not be unsolved at this step!"),
                        _ => throw new NotImplementedException(),
                    };
                    builder.AddLocal(typeref);
                }
                UnwrapFunctionBody(fb.GetOrCreateOmegaBuilder(), source);
                
            } break;
            
            case ImportedFunctionBuilder @ifb:
            {
                ifb.Symbol = source.Extern;
                if (source.Body != null) throw new UnreachableException("Externaly imported functions cannot contains a body");
            } break;
        }
        

    }

    private void UnwrapField(FieldBuilder builder, FieldObject source)
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
        UnwrapFunctionBody_IRNode(builder, source, block);
    }

    // Bruh switch hell
    private void UnwrapFunctionBody_IRNode(OmegaBytecodeBuilder builder, FunctionObject source, IRNode node)
    {
        switch (node)
        {
            case IRBlock @b:
                foreach (var i in  b.Content) UnwrapFunctionBody_IRNode(builder, source, i);
                break;
            
            case IRInvoke @iv:
                foreach (var i in iv.Arguments) UnwrapFunctionBody_IRNode(builder, source, i);
                UnwrapFunctionBody_Call_Reference(builder, source, iv.Target);

                break;

            case IRIntegerLiteral @itlit:
            {
                if (itlit.PtrSized) builder.Writer.LdConstIptr((ulong)itlit.Value);
                //else if (itlit.Size == null) throw new Exception("Integer literal should have a assigned size");
                else switch (itlit.Size)
                {
                    case 1:   builder.Writer.LdConstI1(!itlit.Value.IsZero); break;
                    case 8:   builder.Writer.LdConstI8((byte)itlit.Value); break;
                    case 16:  builder.Writer.LdConstI16((ushort)itlit.Value); break;
                    case 32:  builder.Writer.LdConstI32((uint)itlit.Value); break;
                    case 64:  builder.Writer.LdConstI64((ulong)itlit.Value); break;
                    case 128: builder.Writer.LdConstI128((UInt128)itlit.Value); break;
                    
                    default: builder.Writer.LdConstI((byte)(itlit.Size ?? 255), itlit.Value); break;
                }
            } break;

            case IRSolvedReference @solvref:
                UnwrtapFunctionBody_LoadReference(builder, solvref.Reference);
                break;

            case IRReferenceAccess @refaccess:
            {
                UnwrtapFunctionBody_LoadReference(builder, refaccess.A);
                UnwrapFunctionBody_IRNode(builder, source, refaccess.B);
            } break;

            case IRAssign @assig:
            {
                UnwrapFunctionBody_IRNode(builder, source, assig.Value);
                UnwrapFunctionBody_Store_Reference(builder, source, assig.Target);
            } break;

            case IRBinaryExp @bexp:
            {
                UnwrapFunctionBody_IRNode(builder, source, bexp.Left);
                UnwrapFunctionBody_IRNode(builder, source, bexp.Right);
                switch (bexp.Operator)
                {
                    case IRBinaryExp.Operators.Add: builder.Writer.Add(); break;
                    case IRBinaryExp.Operators.Subtract: builder.Writer.Sub(); break;
                    case IRBinaryExp.Operators.Multiply: builder.Writer.Mul(false); break;
                    case IRBinaryExp.Operators.Divide: builder.Writer.Div(false); break;
                    case IRBinaryExp.Operators.Reminder: builder.Writer.Rem(false); break;
                    default: throw new Exception();
                }
            } break;

            case IRNewObject @newobj:
            {
                builder.Writer.LdNewObject((TypeBuilder)GetTypeReferenceBuilder(newobj.Type));
                foreach (var ia in newobj.InlineAssignments) UnwrapFunctionBody_IRNode(builder, source, ia);
            } break;
            
            
            case IRSignCast @sigcast:
                UnwrapFunctionBody_IRNode(builder, source, sigcast.Value);
                builder.Writer.Sigcast(sigcast.Signed);
                break;
            case IRIntExtend @ex:
                UnwrapFunctionBody_IRNode(builder, source, ex.Value);
                builder.Writer.Extend((byte)ex.Size);
                break;
            case IRIntTrunc @tr:
                UnwrapFunctionBody_IRNode(builder, source, tr.Value);
                builder.Writer.Trunc((byte)tr.Size);
                break;
            
            
            default: throw new NotImplementedException();
        }
    }
    
    private void UnwrapFunctionBody_Store_Reference(OmegaBytecodeBuilder builder, FunctionObject source, IRExpression reference)
    {
        if (reference is not IRSolvedReference @solved) throw new Exception();
        
        switch (solved.Reference)
        {
            case LocalReference @l:
                builder.Writer.StLocal((short)l.Local.index);
                break;
            
            case SolvedFieldReference @f:
                builder.Writer.StField((FieldBuilder)GetObjectBuilder(f.Field));
                break;
            
            default: throw new NotImplementedException();
        }
    }
    private void UnwrapFunctionBody_Call_Reference(OmegaBytecodeBuilder builder, FunctionObject source, IRExpression reference)
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

    private void UnwrtapFunctionBody_LoadReference(OmegaBytecodeBuilder builder, LanguageReference refe)
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
                builder.Writer.LdField((FieldBuilder)GetObjectBuilder(((SolvedFieldReference)fie).Field));
                break;
                    
            default: throw new NotFiniteNumberException();
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
}
