using Abstract.Parser.Core.Language.AbstractSyntaxTree;

namespace Abstract.Parser;

public partial class Evaluator
{

    private Dictionary<string, NamespaceNode> _globalNamespaces = [];
    private Dictionary<string, List<FunctionNode>> _globalFunctions = [];
    private Dictionary<string, StructureNode> _globalStructures = [];
    private Dictionary<string, VariableDeclarationNode> _globalVariables = [];

}