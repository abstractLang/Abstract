using Abstract.Build;
using Abstract.Build.Core.Exceptions;
using Abstract.Parser.Core.Exeptions.Evaluation;
using Abstract.Parser.Core.Language.AbstractSyntaxTree;

namespace Abstract.Parser;

public partial class Evaluator (ErrorHandler err)
{
    private readonly ErrorHandler err = err;

    public void Evaluate(ScriptRoot[] ast)
    {
        ProgramRoot program = new ProgramRoot("Program");
        program.AppendChildren(ast);

        foreach (var scripts in ast)
        {
            RecursiveSearch(scripts);
        }

        Console.WriteLine("Namespaces: ###");
        foreach (var i in _globalNamespaces) Console.WriteLine($"{i.Key}");
        Console.WriteLine("\nFunctions: ###");
        foreach (var i in _globalFunctions) Console.WriteLine($"{i.Key} ({i.Value.Count} elements)");
        Console.WriteLine("\nStructures: ###");
        foreach (var i in _globalStructures) Console.WriteLine($"{i.Key}");

    }

    private void RecursiveSearch(ScopeNode<SyntaxNode> parent)
    {
        foreach (var i in parent)
        {
            try
            {
                if (parent is FunctionNode)
                {
                    if (i is not StatementNode && i is not VariableDeclarationNode)
                        throw new InvalidScoppingException(i);
                }
                else
                {
                    if (i is NamespaceNode @namespaceNode)
                    {
                        _globalNamespaces.Add(namespaceNode.GetGlobalSymbol().ToString(), namespaceNode);
                        RecursiveSearch(namespaceNode);
                    }
                    else if (i is FunctionNode @functionNode)
                    {
                        // functions have overloads!
                        // it need to be added in a list
                        // inside the respective identifier.

                        var identifier = functionNode.GetGlobalSymbol().ToString();

                        if (_globalFunctions.TryGetValue(identifier, out var a)) a.Add(functionNode);
                        else _globalFunctions.Add(identifier, [functionNode]);
                    }
                    else if (i is StructureNode @structNode)
                    {
                        _globalStructures.Add(structNode.GetGlobalSymbol().ToString(), structNode);
                        RecursiveSearch(structNode);
                    }
                    else if (i is VariableDeclarationNode @variableNode)
                    {
                        _globalVariables.Add(variableNode.GetGlobalSymbol().ToString(), variableNode);
                    }

                    else if (i is StatementNode)
                        throw new InvalidScoppingException(i);
                }
            }
            catch (CompilerException ex) { err.RegisterError(null!, ex); }
        }
    }
}
