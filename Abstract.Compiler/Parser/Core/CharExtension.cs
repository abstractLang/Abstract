namespace Abstract.Compiler.Parser.Core;

public static class CharExtension
{
    public static bool IsValidOnIdentifier(this char c)
    {
        return char.IsLetterOrDigit(c) || c == '_';
    }
    public static bool IsValidOnIdentifierStarter(this char c)
    {
        return char.IsLetter(c) || c == '_';
    }

    private static readonly char[] LanguageSymbols = [
        '=', '+', '-', '*', '/', '!', '@', '$', '%', '&', '|', ':', ';', '.', '?', '<', '>'
    ];

    public static bool IsLanguageSymbol(this char c)
        => LanguageSymbols.Contains(c);

}
