using System.Diagnostics;
using Abstract.CodeProcess.Core.Language.EvaluationData;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Expresions;
using Abstract.CodeProcess.Core.Language.EvaluationData.IntermediateTree.Values;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.CodeReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.FunctionReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences;
using Abstract.Realizer.Builder;
using Abstract.Realizer.Builder.Language.Omega;
using Abstract.Realizer.Builder.ProgramMembers;
using Abstract.Realizer.Builder.References;
using TypeReference = Abstract.Realizer.Builder.References.TypeReference;
using IntReference = Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin.IntegerTypeReference;

namespace Abstract.CodeProcess;

public class Compressor
{

    private Dictionary<LangObject, ProgramMemberBuilder> _membersMap;
    
    public void CompressProgramObject(ProgramObject programObject)
    {
        _membersMap = [];
        var realizerModule = new ModuleBuilder("root");
        
        CreateMembers(realizerModule, programObject);

        foreach (var (source, builder) in _membersMap
                     .Where(e => e.Key is FunctionObject)
                     .Select(e => ((e.Key as FunctionObject)!, (e.Value as FunctionBuilder)!)))
        {
            UnwrapFunctions(builder, source);
        }
        
        File.WriteAllText(".abs-cache/debug/compression.txt", realizerModule.GetRoot().ToString());
    }

    private void CreateMembers(ModuleBuilder mod, ProgramObject programObject)
    {
        foreach (var i in programObject.Namespaces)
            CreateMembersRecursive(mod.GetRoot(), null!, i);
    }
    private void CreateMembersRecursive(NamespaceBuilder parent, LangObject? langParent, LangObject langObject)
    {
        switch (langObject)
        {
            case NamespaceObject @nsobj:
            {
                var name = string.Join('.', nsobj.Global[(langParent?.Global.Length ?? 0) ..]);
                var nmsp = parent.AddNamespace(name);
                _membersMap.Add(langObject, nmsp);
                foreach (var i in nsobj.Children)
                    CreateMembersRecursive(nmsp, nsobj, i);
                
            } break;

            case StructObject @stobj:
            {
                var name = string.Join('.', stobj.Global[(langParent?.Global.Length ?? 0) ..]);
                var s = parent.AddStructure(name);
                _membersMap.Add(langObject, s);

                foreach (var i in stobj.Children)
                    CreateMembersRecursive(parent, stobj, i);
                
            } break;
            
            case FunctionGroupObject @fgobj:
            {
                foreach (var i in fgobj.Overloads)
                    CreateMembersRecursive(parent, langParent, i);
            } break;
            
            case FunctionObject @fnobj:
            {
                var name = string.Join('.', fnobj.Global[(langParent?.Global.Length ?? 0) ..]);
                var fn = parent.AddFunction(name);
                _membersMap.Add(langObject, fn);
                
            } break;
            
            default: throw new NotImplementedException();
        };
    }


    private void UnwrapFunctions(FunctionBuilder builder, FunctionObject source)
    {

        foreach (var p in source.Parameters)
        {
            TypeReference typeref = p.Type switch
            {
                IntReference @inr => new IntegerTypeReference(inr.Signed, inr.BitSize),
                SolvedStructTypeReference @str => new NodeTypeReference((_membersMap[str.Struct] as StructureBuilder)!),
                UnsolvedTypeReference => throw new UnreachableException("parameter type should not be unsolved at this step!"),
                _ => throw new NotImplementedException(),
            };
            builder.AddParameter(p.Name, typeref);
        }

        if (source.Body != null)
            foreach (var l in source.Body.Locals)
            {
                TypeReference typeref = l.Type switch
                {
                    IntReference @inr => new IntegerTypeReference(inr.Signed, inr.BitSize),
                    SolvedStructTypeReference @str => new NodeTypeReference((_membersMap[str.Struct] as StructureBuilder)!),
                    UnsolvedTypeReference => throw new UnreachableException("Local type should not be unsolved at this step!"),
                    _ => throw new NotImplementedException(),
                };
                builder.AddLocal(typeref);
            }
        
        UnwrapFunctionBody(builder.GetOrCreateOmegaBuilder(), source);

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
                builder.Writer.LdConstIptr((ulong)itlit.Value);
                break;

            case IRSolvedReference @solvref:
            {

                switch (solvref.Reference)
                {
                    case LocalReference @loc:
                        builder.Writer.LdLocal((short)loc.Local.index); break;
                    case ParameterReference @arg:
                        builder.Writer.LdLocal((short)(-((short)arg.Parameter.index) - 1)); break;
                    
                    default: throw new NotFiniteNumberException();
                }
                
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
                    default: throw new Exception();
                }
            } break;
            
            default:
                throw new NotImplementedException();
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
            
            default: throw new NotImplementedException();
        }
    }
    private void UnwrapFunctionBody_Call_Reference(OmegaBytecodeBuilder builder, FunctionObject source, IRExpression reference)
    {
        if (reference is not IRSolvedReference @solved) throw new Exception();
        
        switch (solved.Reference)
        {
            case SolvedFunctionReference @f:
                builder.Writer.Call((FunctionBuilder)GetObjectBuilder(f.Function));
                break;
                    
            default: throw new NotImplementedException();
        }
    }
    private ProgramMemberBuilder GetObjectBuilder(LangObject obj) => _membersMap[obj];
}
