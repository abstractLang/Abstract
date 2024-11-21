using Abstract.Build;
using Abstract.Build.Core.Exceptions;
using Abstract.Parser.Core.Exeptions.Evaluation;
using Abstract.Parser.Core.Language;
using Abstract.Parser.Core.Language.AbstractSyntaxTree;

namespace Abstract.Parser;

public partial class Evaluator (ErrorHandler err)
{
    private readonly ErrorHandler err = err;

    public ProgramRoot Evaluate(string programName, ScriptRoot[] ast)
    {
        ProgramRoot program = new ProgramRoot(programName);
        program.AppendChildren(ast);

        foreach (var scripts in ast)
        {
            RecursiveSearch(scripts);
        }

        // The first scan process some built-in attributes
        // related to members visibility and protection
        FirstScan();

        // The seccond scan process the types and references,
        // linking every symbol into it master symbolk
        SeccondScan();

        // The third scan evaluates type relations and 
        // matching
        ThirdScan();

        Console.WriteLine("Namespaces: ###");
        foreach (var i in _globalNamespaces) Console.WriteLine($"{i.Key}");
        Console.WriteLine("\nFunctions: ###");
        foreach (var i in _globalFunctions) Console.WriteLine($"{i.Key} ({i.Value.Count} elements)");
        Console.WriteLine("\nStructures: ###");
        foreach (var i in _globalStructures) Console.WriteLine($"{i.Key}");

        return program;
    }

    private void RecursiveSearch(ScopeNode<SyntaxNode> parent)
    {
        List<AttributeNode> attributesOnHold = [];
        foreach (var i in parent.ToList())
        {
            try
            {
                if (i is AttributeNode @attributeNode)
                {
                    attributesOnHold.Add(attributeNode);
                    parent.RemoveChild(i);
                    continue;
                }
                else
                {
                    i.Attributes.AddRange(attributesOnHold);
                    attributesOnHold.Clear();
                }

                if (parent is FunctionNode)
                {
                    if (i is not StatementNode && i is not VariableDeclarationNode)
                        throw new InvalidScoppingException(i);
                }
                else
                {
                    if (i is NamespaceNode @namespaceNode)
                    {
                        _globalNamespaces.Add(namespaceNode.GetGlobalSymbol(), namespaceNode);
                        RecursiveSearch(namespaceNode);
                    }
                    else if (i is FunctionNode @functionNode)
                    {
                        // functions have overloads!
                        // it need to be added in a list
                        // inside the respective identifier.

                        var identifier = functionNode.GetGlobalSymbol();

                        if (_globalFunctions.TryGetValue(identifier, out var a)) a.Add(functionNode);
                        else _globalFunctions.Add(identifier, [functionNode]);
                    }
                    else if (i is StructureNode @structNode)
                    {
                        _globalStructures.Add(structNode.GetGlobalSymbol(), structNode);
                        RecursiveSearch(structNode);
                    }
                    else if (i is VariableDeclarationNode @variableNode)
                    {
                        _globalVariables.Add(variableNode.GetGlobalSymbol(), variableNode);
                    }

                    else throw new InvalidScoppingException(i);
                }
            }
            catch (CompilerException ex) { err.RegisterError(null!, ex); }
        }
    }

    private void FirstScan()
    {

        foreach (var i in _globalNamespaces.Values.ToArray())
        {
            if (i.TryGetAttribute("defineGlobal", out var attribute))
            {
                var args = attribute.Arguments.Children;
                if (args.Length != 1) throw new NoFuncOverloadException("defineGlobal", 1, args.Length, attribute);

                if (args[0] is StringLiteralValueNode @value)
                    _globalNamespaces.Add(new ReferenceSymbol([value.value], i.Symbol), i);

                else if (args[0] is CollectionExpressionNode @list)
                    foreach (var j in list)
                    {
                        if (j is not StringLiteralValueNode @value2) throw new Exception();
                        _globalNamespaces.Add(new ReferenceSymbol([value2.value], i.Symbol), i);
                    }
            }
        }

        foreach (var j in _globalFunctions.Values.ToArray())
        {
            foreach (var i in j)
            {
                if (i.TryGetAttribute("defineGlobal", out var attribute))
                {
                    var args = attribute.Arguments.Children;
                    if (args.Length != 1) throw new NoFuncOverloadException("defineGlobal", 1, args.Length, attribute);

                    if (args[0] is StringLiteralValueNode @value)
                    {
                        if (_globalFunctions.TryGetValue(new ReferenceSymbol([value.value], i.Symbol), out var l)) l.Add(i);
                        else _globalFunctions.Add(new ReferenceSymbol([value.value], i.Symbol), [i]);
                    }

                    else if (args[0] is CollectionExpressionNode @list)
                        foreach (var k in list)
                        {
                            if (k is not StringLiteralValueNode @value2) throw new Exception();
                            if (_globalFunctions.TryGetValue(new ReferenceSymbol([value2.value], i.Symbol), out var l)) l.Add(i);
                            else _globalFunctions.Add(new ReferenceSymbol([value2.value], i.Symbol), [i]);
                        }
                }
            }
        }

        foreach (var i in _globalStructures.Values.ToArray())
        {
            if (i.TryGetAttribute("defineGlobal", out var attribute))
            {
                var args = attribute.Arguments.Children;
                if (args.Length != 1) throw new NoFuncOverloadException("defineGlobal", 1, args.Length, attribute);

                if (args[0] is StringLiteralValueNode @value)
                    _globalStructures.Add(new ReferenceSymbol([value.value], i.Symbol), i);

                else if (args[0] is CollectionExpressionNode @list)
                    foreach (var j in list)
                    {
                        if (j is not StringLiteralValueNode @value2) throw new Exception();
                        _globalStructures.Add(new ReferenceSymbol([value2.value], i.Symbol), i);
                    }
            }
        }

    }

    private void SeccondScan()
    {

        foreach (var i in _globalVariables)
        {
            ProcessReferenceToTypeNode(i.Value.ReturnType, i.Value);
        }

        foreach (var i in _globalStructures)
        {
            if (i.Value.ExtendsType != null)
                ProcessReferenceToTypeNode(i.Value.ExtendsType, i.Value);
        }

        foreach (var i in _globalFunctions)
        {
            foreach (var j in i.Value)
            {
                ProcessReferenceToTypeNode(j.ReturnType, j);
                foreach (var p in j.Parameters)
                    ProcessReferenceToTypeNode(p.ReturnType, j);
            }
        }

    }

    private void ThirdScan()
    {
        // TODO :3
    }

    private void ProcessReferenceToTypeNode(TypeNode typeNode, SyntaxNode parent)
    {
        if (typeNode is ReferenceTypeNode @reference)
        {
            var t = SearchForSymbol(reference.symbol, parent);
            reference.symbol = new ReferenceSymbol(reference.symbol, t);
        }

        // TODO process other kinds of type nodes like generics, arrays or
        // references here
        else if (typeNode is ArrayType @array)
        {
            // arrays are just a syntax sugar for
            // St.Types.Collections.Array(T)
            // so it can just throw back a
            // generic processed node
        }

        else throw new NotImplementedException();
    }

    private MasterSymbol SearchForSymbol(ISymbol symbol, SyntaxNode onNode)
    {

        var type = SearchForStructure(symbol, onNode);
        Console.WriteLine($"{symbol} -> {(type?.Symbol.ToString() ?? "null")}");

        return type?.Symbol!;
    }

    private StructureNode? SearchForStructure(ISymbol reference, SyntaxNode onNode)
    {
        if (_globalStructures.TryGetValue(reference, out var structure)) return structure;

        do
        {
            if (onNode is ScopeNode<SyntaxNode> @scope)
            {
                var res = scope.FirstOrDefault(e => e is StructureNode @struc && struc.Symbol == reference);
                if (res != null) return res as StructureNode;
            }

            onNode = onNode.Parent!;
        }
        while (onNode != null);

        return null;
    }

}
