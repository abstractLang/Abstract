namespace Abstract.Parsing.Core;

/// <summary>
/// A token sliced from the source code.
/// </summary>
public class Token
{
    /// <summary>
    /// Pseudo-tokens that prefix the token and do not
    /// represent important information.
    /// e.g.: spaces, tabs or comments.
    /// </summary>
    public readonly SourceRange trivia;
    /// <summary>
    /// The actual token value.
    /// </summary>
    public readonly SourceRange value;

    public Token before = null!;
    public Token after = null!;

    /// <summary>
    /// The token kind.
    /// </summary>
    public readonly Kind kind;

    public override string ToString() => $"{trivia.ToString()}{value.ToString()}";


    /// <summary>
    /// The list of general token kinds.
    /// </summary>
    public enum Kind : byte
    {
        Undefined = 0,
    }
}
