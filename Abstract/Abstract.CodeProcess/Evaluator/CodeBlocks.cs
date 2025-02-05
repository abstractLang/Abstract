using Abstract.Build.Core.Exceptions;
using Abstract.Build.Core.Exceptions.Internal;
using Abstract.Build.Exceptions;
using Abstract.Parser.Core.Exceptions.Evaluation;
using Abstract.Parser.Core.Language.SyntaxNodes.Control;
using Abstract.Parser.Core.Language.SyntaxNodes.Expression;
using Abstract.Parser.Core.Language.SyntaxNodes.Misc;
using Abstract.Parser.Core.Language.SyntaxNodes.Statement;
using Abstract.Parser.Core.Language.SyntaxNodes.Value;
using Abstract.Parser.Core.ProgData;
using Abstract.Parser.Core.ProgData.DataReference;
using Abstract.Parser.Core.ProgData.DataReference.ComptimeConstants;
using Abstract.Parser.Core.ProgData.FunctionExecution;
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
                if (function.FunctionBodyNode == null)
                {
                    // TODO check if it is marked as @externInterface
                    continue;
                }
                if (function.IsGeneric)
                {
                    // generics are evaluated depending on how
                    // they are called, so generic functions
                    // needs to be lazy-evaluated
                    continue;
                }

                EvalBlock(function.FunctionBodyNode, function);
            }
        }

    }

    private void EvalBlock(BlockNode blockNode, Function function)
    {
        var block = new ExecutableContext(blockNode, null)
        {
            ProgramMemberParent = function
        };

        List<TypeReference> argTypes = [];
        foreach (var (type, name) in function.baseParameters)
        {
            block.AppendLocalParameter(name, type);
            argTypes.Add(type);
        }
        
        // Appending base implementation
        function.AppendImplementation([.. argTypes], block, function.baseReturnType);

        ScanBlock(blockNode, block);
    }
    private void EvalBlock(BlockNode blockNode, ExecutableContext parentBlock)
    {
        var block = new ExecutableContext(blockNode, parentBlock)
        {
            ProgramMemberParent = parentBlock.ProgramMemberParent
        };

        ScanBlock(blockNode, block);
    }
    private void ScanBlock(BlockNode blockNode, ExecutableContext block)
    {
        foreach (var i in blockNode.Children[1..^1])
        {
            try {
                if (i is StatementNode @stat)
                    EvalStatement(stat, block);

                else if (i is ExpressionNode @exp)
                    EvalExpression(exp, block);
            }
            catch (SyntaxException ex)
            { _errHandler.RegisterError(ex); }
            catch (BuildException ex)
            { _errHandler.RegisterError(null!, ex); }
        }
    }

    private void EvalStatement(StatementNode node, ExecutableContext currblock)
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

    private ExpressionNode EvalExpression(ExpressionNode node, ExecutableContext currblock)
    {
        try
        {

            if (node is ParenthesisExpressionNode @parenthesisExpression)
                return EvalExpression(parenthesisExpression.Content, currblock);

            else if (node is AssignmentExpressionNode @assignmentExpression)
                return EvalAssiginment(assignmentExpression, currblock);
            else if (node is BinaryExpressionNode @binaryExpression)
                return EvalBinaryOperation(binaryExpression, currblock);
            else if (node is UnaryExpressionNode @unaryExpression)
                return EvalUnaryOperation(unaryExpression, currblock);

            else if (node is LocalVariableNode @localVariable)
                return EvalLocal(localVariable, currblock);

            else if (node is FunctionCallExpressionNode @funcCall)
                return EvalFunctionCall(funcCall, currblock);
            else if (node is IdentifierCollectionNode @identifierCollection)
                return EvalIdentifier(identifierCollection, currblock);

            else if (node is TypeExpressionNode @typeExp)
                return EvalType(typeExp, currblock);

            else if (node is StringLiteralNode @stringLit)
            {
                if (stringLit.isSimple)
                {
                    var idx = currblock.AppendConstantReference(stringLit.RawContent);
                    node.DataReference = new StringConstRef(currblock, idx);
                }
                else
                {
                    foreach (var i in stringLit.Content)
                    {
                        if (i is StringInterpolationNode @interpolation)
                            EvalExpression(interpolation.Expression, currblock);

                        // TODO eval chars
                    }
                }
                return node;
            }
            else if (node is ValueNode @value)
                return EvalValue(value, currblock);

            Console.WriteLine($"expression {node} ({node.GetType().Name})");
        }
        catch (SyntaxException e) { _errHandler.RegisterError(e); }
        return null!;
    }

    private ExpressionNode EvalAssiginment(AssignmentExpressionNode node, ExecutableContext currblock)
    {
        EvalExpression(node.Left, currblock);
        EvalExpression(node.Right, currblock);
        
        if (node.Operator != "=") // Operate and assigin
        {
            throw new NotImplementedException();
        }
        else
        {
            if (CanBeAssignedTo(
                node.Right.DataReference.refferToType,
                node.Left.DataReference.refferToType,
                out var conversion
            )) {
                if (conversion != null)
                {
                    var convert = new FunctionCallExpressionNode();

                    convert.OverrideRange = node.Right.Range;
                    convert.Target = conversion;

                    var temp = node.Right;
                    node.ReplaceChild(node.Right, convert);
                    convert.EvalArguments = [temp];
                }
            }
            else
            {
                Console.WriteLine($"{node.Right.DataReference.refferToType} "
                + $"is not assigned to {node.Left.DataReference.refferToType}");

                throw new NotImplementedInternalBuildException();
            }
        }

        node.DataReference = node.Left.DataReference;
        node.evaluated = true;
        return node;
    }

    private FunctionCallExpressionNode EvalBinaryOperation(BinaryExpressionNode node, ExecutableContext currblock)
    {
        try {

            EvalExpression(node.Left, currblock);
            EvalExpression(node.Right, currblock);
            
            if (node.Left.DataReference?.refferToType == null)
                throw new Exception();

            if (node.Left.DataReference.refferToType is SolvedTypeReference @baseType)
            {
                var operatorOverloads = baseType.structure.SearchForOperators(node.Operator)
                    ?? throw new InvalidOperatorForTypeException(node, baseType.structure.GlobalReference);
                
                DataRef[] args = [node.Left.DataReference, node.Right.DataReference];

                var (function, toConvert) = TryGetOveloadIndirect(operatorOverloads, args);

                if (function == null)
                {
                    throw new InvalidOperatorOverloadException(node,
                        baseType.structure.GlobalReference);
                }

                // select and wrap left expression
                var expLeft = node.Left;
                if (toConvert[0] != null)
                {
                    var wraper = new FunctionCallExpressionNode();
                    wraper.OverrideRange = expLeft.Range;
                    wraper.Target = toConvert[0]!;

                    var temp = expLeft;
                    node.RemoveChild(expLeft);
                    wraper.AppendChild(temp);
                    wraper.EvalArguments = [temp];
                    wraper.DataReference = temp.DataReference;

                    expLeft = wraper;
                }
                
                // select and wrap right expression
                var expRight = node.Right;
                if (toConvert[1] != null)
                {
                    var wraper = new FunctionCallExpressionNode();
                    wraper.OverrideRange = expRight.Range;
                    wraper.Target = toConvert[1]!;

                    var temp = expRight;
                    node.RemoveChild(expRight);
                    wraper.AppendChild(temp);
                    wraper.EvalArguments = [temp];
                    wraper.DataReference = temp.DataReference;

                    expRight = wraper;
                }

                // wrap the entire expression
                var wraper2 = new FunctionCallExpressionNode();
                wraper2.OverrideRange = node.Range;
                wraper2.Target = function;
                wraper2.EvalArguments = [expLeft, expRight];
                node.Parent.ReplaceChild(node, wraper2);

                wraper2.DataReference = new DynamicDataRef(GetFunctionReturnType(function, args));

                wraper2.evaluated = true;
                return wraper2;
            }

            node.DataReference = new DataErrorRef();
        }
        catch (SyntaxException ex)
        {
            _errHandler.RegisterError(ex);
            node.DataReference = new DataErrorRef();
        }

        return null!;
    }
    private UnaryExpressionNode EvalUnaryOperation(UnaryExpressionNode node, ExecutableContext currblock)
    {
        EvalExpression(node.Expression, currblock);
        //Console.WriteLine($"unary {node} ({node.GetType().Name})");
        // TODO eval unary operators

        //node.evaluated = true;
        return node;
    }

    private LocalVariableNode EvalLocal(LocalVariableNode node, ExecutableContext currblock)
    {
        try
        {
            bool isConstant = node.IsConstant;
            TypeReference type = GetTypeFromTypeExpressionNode(node.TypedIdentifier.Type, currblock.ProgramMemberParent);
            string name = node.TypedIdentifier.Identifier.Value;

            currblock.AppendLocalVariable(name, type, isConstant, node);
            node.DataReference = new LocalDataRef(currblock, name);
        }
        catch (SyntaxException ex)
        {
            _errHandler.RegisterError(ex);
            node.DataReference = new DataErrorRef();
        }

        node.evaluated = true;
        return node;
    }

    private FunctionCallExpressionNode EvalFunctionCall(FunctionCallExpressionNode node, ExecutableContext currblock)
    {
        try {
            // Evaluating called reference
            EvalIdentifier(node.FunctionReference, currblock);
            var idReference = node.FunctionReference.DataReference;

            // Evaluating argument references
            List<DataRef> _args = [];
            List<ExpressionNode> _evaluated = [];
            foreach (var i in node.Arguments)
            {
                var j = EvalExpression(i, currblock);
                _evaluated.Add(j);
                _args.Add(j.DataReference);
            }
            node.EvalArguments = [.. _evaluated];

            if (idReference is FunctionGroupRef @funcGroupRef)
            {
                var functionGroup = funcGroupRef.group;

                (Function? func, Function?[] casts) = TryGetOveloadIndirect(functionGroup, [.. _args]);
                if (func is null)
                    throw new NoOverloadForTypes(node, string.Join(", ", _args.Select(e => e.refferToType?.ToString() ?? "!nil")));

                if (func.IsGeneric)
                { /* TODO implement some logic to mark an generic use and implementation */ }

                node.Target = func;
                node.DataReference = new DynamicDataRef(GetFunctionReturnType(func, [.. _args]));
            }
            else throw new ReferenceNotCallableException(node);
        }
        catch (SyntaxException ex)
        {
            _errHandler.RegisterError(ex);
            node.DataReference = new DataErrorRef();
        }

        node.evaluated = true;
        return node;
    }
    private IdentifierCollectionNode EvalIdentifier(IdentifierCollectionNode node, ExecutableContext currblock)
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

        return node;
    }
    private TypeExpressionNode EvalType(TypeExpressionNode node, ExecutableContext currblock)
    {
        TypeReference type = GetTypeFromTypeExpressionNode(node, currblock.ProgramMemberParent);
        node.DataReference = new TypeDataRef(type, SearchStructure("Std.Types.Type"));

        return node;
    }
    private ValueNode EvalValue(ValueNode node, ExecutableContext currblock)
    {
        if (node is IntegerLiteralNode @intl)
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

        return node;
    }
}
