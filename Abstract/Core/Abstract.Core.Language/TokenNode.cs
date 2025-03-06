using Abstract.Core.Language;

namespace Abstract.Core.Language;

public class TokenNode(Token tkn) : ISyntaxNode
{

    private ISyntaxNode _parent = null!;
    public ISyntaxNode Parent { get => _parent; set => _parent = value; }

    public NodeKind Kind => NodeKind.Token;

    private readonly Token token = tkn;

    public SourceRange Trivia => token.trivia;
    public SourceRange Value => token.value;
    public Token.Kind TokenKind => token.kind;

    public Token Before => token.before;
    public Token After => token.after;

    public override string ToString() => token.ToString();

    string ISyntaxNode.ToString()
    {
        throw new NotImplementedException();
    }
}
