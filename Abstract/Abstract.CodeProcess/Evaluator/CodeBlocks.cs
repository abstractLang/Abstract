using Abstract.Build.Core.Exceptions.Internal;
using Abstract.Build.Exceptions;
using Abstract.Parser.Core.Exceptions.Evaluation;
using Abstract.Parser.Core.Language.SyntaxNodes.Control;
using Abstract.Parser.Core.Language.SyntaxNodes.Expression;
using Abstract.Parser.Core.Language.SyntaxNodes.Statement;
using Abstract.Parser.Core.Language.SyntaxNodes.Value;
using Abstract.Parser.Core.ProgData;
using Abstract.Parser.Core.ProgData.DataReference;
using Abstract.Parser.Core.ProgData.DataReference.ComptimeConstants;
using Abstract.Parser.Core.ProgMembers;

namespace Abstract.Parser;

public partial class Evaluator
{

    private void ScanCodeBlocks()
    {
        typeMatchingLog.AppendLine("\n# Processing code blocks #\n");

        foreach (var funcGroup in program.functions.Values)
        {
            foreach (var function in funcGroup)
            {
                if (function.FunctionBodyNode == null) continue;
                if (function.IsGeneric) continue;

                EvalBlock(function.FunctionBodyNode, function);
            }
        }

    }

    private void EvalBlock(BlockNode blockNode, ProgramMember? parent)
    {
        var block = new ExecutableCodeBlock()
        {
            ProgramMemberParent = parent,
            BlockParent = null
        };
        blockNode.EvaluatedData = block;

        ScanBlock(blockNode, block);
    }
    private void EvalBlock(BlockNode blockNode, ExecutableCodeBlock parentBlock)
    {
        var block = new ExecutableCodeBlock()
        {
            ProgramMemberParent = parentBlock.ProgramMemberParent,
            BlockParent = parentBlock
        };
        blockNode.EvaluatedData = block;

        ScanBlock(blockNode, block);
    }
    private void ScanBlock(BlockNode blockNode, ExecutableCodeBlock block)
    {
        foreach (var i in blockNode.Children[1..^1])
        {
            if (i is StatementNode @stat)
                EvalStatement(stat, block);

            else if (i is ExpressionNode @exp)
                EvalExpression(exp, block);
        }
    }

    private void EvalStatement(StatementNode node, ExecutableCodeBlock currblock)
    {
        if (node is IfStatementNode @ifStatement)
        {
            EvalExpression(ifStatement.Condition, currblock);
            if (ifStatement.UsingBlock) EvalBlock(ifStatement.BlockThen, currblock);
            else EvalStatement(ifStatement.StatementThen, currblock);
        }
        else if (node is ElifStatementNode @elifStatement)
        {
           // Console.WriteLine(string.Join("\n", (object[])ifStatement.Children));
           Console.WriteLine("elif not implemented");
        }
        else if (node is ElseStatementNode @elseStatement)
        {
            if (elseStatement.UsingBlock) EvalBlock(elseStatement.BlockThen, currblock);
            else EvalStatement(elseStatement.StatementThen, currblock);
        }

        else if (node is ReturnStatementNode @returnStatement)
        {
            if (returnStatement.HasExpression)
                EvalExpression(returnStatement.Expression, currblock);
        }

        else Console.WriteLine($"statement {node} ({node.GetType().Name})");
    }

    private void EvalExpression(ExpressionNode node, ExecutableCodeBlock currblock)
    {
        if (node is ParenthesisExpressionNode @parenthesisExpression)
        {
            EvalExpression(parenthesisExpression.Content, currblock);
        }

        else if (node is AssignmentExpressionNode @assignmentExpression)
        {
            EvalAssiginment(assignmentExpression, currblock);
        }
        else if (node is BinaryExpressionNode @binaryExpression)
        {
            EvalBinaryOperation(binaryExpression, currblock);
        }
        else if (node is UnaryExpressionNode @unaryExpression)
        {
            EvalUnaryOperation(unaryExpression, currblock);
        }

        else if (node is LocalVariableNode @localVariable)
        {
            EvalLocal(localVariable, currblock);
        }

        else if (node is FunctionCallExpressionNode @funcCall)
        {
            EvalFunctionCall(funcCall, currblock);
        }
        else if (node is IdentifierCollectionNode @identifierCollection)
        {
            EvalIdentifier(identifierCollection, currblock);
        }

        else if (node is ValueNode @value) EvalValue(value, currblock);

        else Console.WriteLine($"expression {node} ({node.GetType().Name})");
    }

    private void EvalAssiginment(AssignmentExpressionNode node, ExecutableCodeBlock currblock)
    {
        EvalExpression(node.Left, currblock);
        EvalExpression(node.Right, currblock);
        
        if (node.Operator != "=") // Operate and assigin
        {
            throw new NotImplementedException();
        }

        Console.WriteLine($"{node.Left.DataReference} {node.Operator} {node.Right.DataReference}");
        node.evaluated = true;
    }

    private void EvalBinaryOperation(BinaryExpressionNode node, ExecutableCodeBlock currblock)
    {
        try {

            EvalExpression(node.Left, currblock);
            EvalExpression(node.Right, currblock);
            
            if (node.DataReference?.refferToType == null)
            {
                node.DataReference = new DataErrorRef();
                goto Return;
            }

            if (node.DataReference.refferToType is SolvedTypeReference @baseType)
            {
                var operatorOverloads = baseType.structure.SearchForOperators(node.Operator);
                if (operatorOverloads != null)
                {
                    // TODO
                    throw new NotImplementedInternalBuildException();
                }
                else throw new InvalidOperatorForType(node, baseType.structure.GlobalReference);
            }

        }
        catch (SyntaxException ex)
        {
            _errHandler.RegisterError(ex);
            node.DataReference = new DataErrorRef();
        }

        Return:
        Console.WriteLine($"{node.Left.DataReference} {node.Operator} {node.Right.DataReference} ({node.DataReference})");
        node.evaluated = true;
    }
    private void EvalUnaryOperation(UnaryExpressionNode node, ExecutableCodeBlock currblock)
    {
        EvalExpression(node.Expression, currblock);
        //Console.WriteLine($"unary {node} ({node.GetType().Name})");
        // TODO

        //node.evaluated = true;
    }

    private void EvalLocal(LocalVariableNode node, ExecutableCodeBlock currblock)
    {
        bool isConstant = node.IsConstant;
        TypeReference type = GetTypeFromTypeExpressionNode(node.TypedIdentifier.Type, currblock.ProgramMemberParent);
        string name = node.TypedIdentifier.Identifier.Value;

        currblock.AppendLocalReference(name, type, isConstant);
        node.DataReference = new LocalDataRef(currblock, name);

        node.evaluated = true;
    }

    private void EvalFunctionCall(FunctionCallExpressionNode node, ExecutableCodeBlock currblock)
    {
        try {
            // Evaluating called reference
            EvalIdentifier(node.FunctionReference, currblock);
            var idReference = node.FunctionReference.DataReference;

            // Evaluating argument references
            List<TypeReference> _argTypes = [];
            foreach (var i in node.Arguments)
            {
                EvalExpression(i, currblock);
                _argTypes.Add(i.DataReference.refferToType);
            }

            if (idReference is FunctionGroupRef @funcGroupRef)
            {
                var functionGroup = funcGroupRef.group;
                Function func = null!;
                foreach (var f in functionGroup)
                {
                    if (f.parameterTypes.Length != _argTypes.Count) continue;

                    for (var i = 0; i < f.parameterTypes.Length; i++)
                        if (!CanBeDirectlyAssignedTo(_argTypes[i], f.parameterTypes[i])) goto Break;

                    func = f;
                    break;

                    Break: continue;
                }

                if (func == null!) throw new NoOverloadForTypes(node, string.Join(", ", _argTypes));
                else node.FunctionTarget = func;
            }

            else throw new ReferenceNotCallableException(node);
        }
        catch (SyntaxException ex)
        {
            _errHandler.RegisterError(ex);
            node.DataReference = new DataErrorRef();
        }
        node.evaluated = true;
    }
    private void EvalIdentifier(IdentifierCollectionNode node, ExecutableCodeBlock currblock)
    {
        try
        {
            var res = currblock.TryGetReference(node.ToString()) ?? throw new ReferenceNotFound(node);
            node.DataReference = res;
        }
        catch (SyntaxException ex)
        {
            _errHandler.RegisterError(ex);
            node.DataReference = new DataErrorRef();
        }
        node.evaluated = true;
    }
    private void EvalValue(ValueNode node, ExecutableCodeBlock currblock)
    {
        if (node is StringLiteralNode @stringl)
        {
            var idx = currblock.AppendConstantReference(stringl.Value);
            node.DataReference = new StringConstRef(currblock, idx);
        }
        else if (node is IntegerLiteralNode @intl)
        {
            var idx = currblock.AppendConstantReference(intl.Value);
            node.DataReference = new IntegerConstRef(currblock, idx);
        }
        else if (node is FloatingLiteralNode @floatl)
        {
            var idx = currblock.AppendConstantReference(floatl.Value);
            node.DataReference = new FloatConstRef(currblock, idx);
        }
        else if (node is BooleanLiteralNode @booll)
        {
            var idx = currblock.AppendConstantReference(booll.Value);
            node.DataReference = new BooleanConstRef(currblock, idx);
        }
    
        else Console.WriteLine($"value {node} ({node.GetType().Name})");
    }
}
