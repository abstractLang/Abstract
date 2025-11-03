using System.Diagnostics;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects.Attributes;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageObjects.CodeObjects;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.AttributeReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences;
using Abstract.CodeProcess.Core.Language.EvaluationData.LanguageReferences.TypeReferences.Builtin;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Expression;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Misc;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Value;

namespace Abstract.CodeProcess;


/*
 * Stage Two:
 *  Scans all the headers, unwraps the build-in
 *  attributes and evaluate header-level references.
 *  This step should be done early as it may dump
 *  more shit into `_globalReferenceTable`.
 */

public partial class Analyzer
{
    private void ScanHeadersMetadata()
    {
        foreach (var reference in _globalReferenceTable.ToArray())
        {
            var langObj = reference.Value;

            if (langObj is FunctionGroupObject @funcg)
            {
                foreach (var o in funcg.Overloads)
                    ProcessHeader(o);
            }
            else ProcessHeader(langObj);
        }
    }
    private void ProcessHeader(LangObject reference)
    {
        switch (reference)
        {
            case FunctionObject @a: UnwrapFunctionMeta(a); break;
            case StructObject @a: UnwrapStructureMeta(a); break;
        }
        
        // Handling quick inheritance
        if (reference is IStaticModifier @refStatic and not StructObject
            && reference.Parent is IStaticModifier @parentStatic)
        {
            refStatic.Static = parentStatic.Static;
        }
        
        // Handling builtin attributes
        foreach (var attr in reference.Attributes)
        {
            if (attr is not BuiltInAttributeReference @builtInAttribute) continue;

            switch (builtInAttribute.Attribute)
            {
                case BuiltinAttributes.Static: if (reference is IStaticModifier @s) s.Static = true; break;
                case BuiltinAttributes.Public: if (reference is IPublicModifier @p) p.Public = true; break;
                case BuiltinAttributes.Private: if (reference is IPublicModifier @p2) p2.Public = false; break;
                case BuiltinAttributes.Internal: if (reference is IInternalModifier @i) i.Internal = true; break;
                case BuiltinAttributes.Final: if (reference is StructObject @f) f.Final = true; break;
                case BuiltinAttributes.Abstract: if (reference is IAbstractModifier @a) a.Abstract = true; break;
                case BuiltinAttributes.Interface: if (reference is StructObject @i2) i2.Interface = true; break;
                case BuiltinAttributes.Virtual: if (reference is IVirtualModifier @v) v.Virtual = true; break;
                case BuiltinAttributes.Override: if (reference is IOverrideAttribute @o) o.Override = true; break;
                case BuiltinAttributes.ConstExp: if (reference is FunctionObject @c) c.ConstExp = true; break;
                
                case BuiltinAttributes.DefineGlobal:
                {
                    var node = builtInAttribute.syntaxNode;
                    // attribute node structure:
                    // <@/> <identifier/> <(> <args.../> <)/>
                    
                    if (node.Children.Length != 3) throw new Exception("'DefineGlobal' expected 1+ arguments");
                    var args = (node.Children[2] as ArgumentCollectionNode)!.Arguments;
                    
                    for (var argi = 0; argi < args.Length; argi++)
                    {
                        if (args[argi] is not StringLiteralNode @strlit)
                            throw new Exception($"'DefineGlobal' expected argument {argi} as ComptimeString");
                        
                        RegisterAlias(null, reference, strlit.RawContent);
                    }
                } break;

                case BuiltinAttributes.Extern:
                {
                    var node = builtInAttribute.syntaxNode;
                    
                    if (reference is not IExternModifier @externModifier)
                        throw new Exception($"Attribute {attr} is not suitable to {reference.GetType().Name}");
                    
                    if (node.Children.Length != 3) throw new Exception("'Extern' expected arguments");
                    var args = (node.Children[2] as ArgumentCollectionNode)!.Arguments;


                    switch (args.Length)
                    {
                        case 1:
                        {
                            if (args[0] is not StringLiteralNode @strlit)
                                throw new Exception("'Extern' expected argument 0 as ComptimeString");

                            externModifier.Extern = (null, strlit.RawContent);
                            break;
                        }
                        case 2:
                        {
                            if (args[0] is not StringLiteralNode @strlit1)
                                throw new Exception("'Extern' expected argument 0 as ComptimeString");
                            if (args[1] is not StringLiteralNode @strlit2)
                                throw new Exception("'Extern' expected argument 1 as ComptimeString");
                        
                            externModifier.Extern = (strlit1.RawContent, strlit2.RawContent);
                            break;
                        }
                        default:
                            throw new Exception($"'Extern' expected 1 or 2 arguments, found {args.Length}");
                    }
                } break;

                case BuiltinAttributes.Export:
                {
                    var node = builtInAttribute.syntaxNode;
                    
                    if (reference is not IExportModifier @exportModifier)
                        throw new Exception($"Attribute {attr} is not suitable to {reference.GetType().Name}");
                    
                    if (node.Children.Length != 3) throw new Exception("'Export' expected arguments");
                    var args = (node.Children[2] as ArgumentCollectionNode)!.Arguments;
                    
                    if (args.Length != 1) throw new Exception($"'Export' expected 1 arguments, found {args.Length}");
                    if (args[0] is not StringLiteralNode @strlit1)
                        throw new Exception("'Export' expected argument 0 as ComptimeString");

                    exportModifier.Export = strlit1.RawContent;
                } break;

                // TODO builtin attributes
                case BuiltinAttributes.Comptime:
                case BuiltinAttributes.Getter:
                    break;
                    
                case BuiltinAttributes.Align:
                case BuiltinAttributes.AllowAccessTo:
                case BuiltinAttributes.DenyAccessTo:
                case BuiltinAttributes.Inline:
                case BuiltinAttributes.Noinline:
                case BuiltinAttributes.Runtime:
                case BuiltinAttributes.CallConv:
                case BuiltinAttributes.Setter:
                case BuiltinAttributes.IndexerGetter:
                case BuiltinAttributes.IndexerSetter:
                case BuiltinAttributes.ExplicitConvert:
                case BuiltinAttributes.ImplicitConvert:
                case BuiltinAttributes.OverrideOperator:
                case BuiltinAttributes._undefined:
                default: throw new NotImplementedException();
            }
        }

    }

    private void UnwrapFunctionMeta(FunctionObject function)
    {
        var node = function.syntaxNode;
        var paramc = node.ParameterCollection;

        foreach (var i in paramc.Items)
        {
            var typeref = SolveShallowType(i.Type);
            var name = i.Identifier.Value;

            if (typeref is
                TypeTypeReference or
                AnytypeTypeReference) function.Generic = true;
            
            function.AddParameter(new ParameterObject(typeref, name));
        }

        function.ReturnType = node.ReturnType == null
            ? new VoidTypeReference()
            : SolveShallowType(node.ReturnType);
        
    }
    private void UnwrapStructureMeta(StructObject structure)
    {
        var node = structure.syntaxNode;

        if (node.Children.Length == 4)
        {
            var extendsImplements = new Queue<SyntaxNode>(((ExtendsImplementsNode)node.Children[2]).Children);

            AccessNode? extendsVal = null;
            List<AccessNode> implementsVal = [];

            if (extendsImplements.Count > 0 && extendsImplements.Dequeue() is TokenNode { Value: "extends" })
            {
                var identifier = (AccessNode)extendsImplements.Dequeue();
                if (identifier.Incomplete) throw new Exception($"Cannot complete identifier {identifier}");
                extendsVal = identifier;
            }

            if (extendsImplements.Count > 0 && extendsImplements.Dequeue() is TokenNode { Value: "implements" })
            {
                while (extendsImplements.Count > 0) throw new UnreachableException();
            }

            structure.Extends = extendsVal == null ? null : new UnsolvedTypeReference(extendsVal);
        }
        
    }
}
