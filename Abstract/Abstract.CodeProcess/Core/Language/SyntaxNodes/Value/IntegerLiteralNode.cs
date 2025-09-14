using System.Diagnostics;
using System.Globalization;
using System.Numerics;

namespace Abstract.CodeProcess.Core.Language.SyntaxNodes.Value;

public class IntegerLiteralNode(Token token) : ValueNode(token)
{

    public BigInteger Value
    {
        get
        {
            var numstr = token.ValueString();

            (BigInteger numBase, var str) = numstr.Length < 3
                ? (10, numstr)
                : numstr[0..2] switch {
                    "0b" => (2, numstr[2..]),
                    "0o" => (8, numstr[2..]),
                    "0x" => (16, numstr[2..]),
                    _ => (10, numstr)
                };
            
            if (numBase == 10)
                return BigInteger.Parse(str);

            return str.Select(c => c switch
                {
                    >= '0' and <= '9' => c - '0',
                    >= 'a' and <= 'f' => 10 + (c - 'a'),
                    >= 'A' and <= 'F' => 10 + (c - 'A'),
                    _ => throw new UnreachableException() // Lexer should not allow we reach it
                })
                .Aggregate(BigInteger.Zero, (current, digit) => current * numBase + digit);
        }
    }
    

    public override string ToString() => $"{Value}";
}
