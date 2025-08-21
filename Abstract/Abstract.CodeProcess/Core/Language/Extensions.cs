namespace Abstract.CodeProcess.Core.Language;

public static class CharExtensions
{

    public static bool IsValidOnIdentifier(this char c)
    {
        return char.IsLetterOrDigit(c) || c == '_';
    }
    public static bool IsValidOnIdentifierStarter(this char c)
    {
        return char.IsLetter(c) || c == '_';
    }

    private static readonly char[] _languageSymbols = [
        '=', '+', '-', '*', '/', '!', '@', '$', '%', '&', '|', ':', ';', '.', '?', '<', '>'
    ];

    public static bool IsLanguageSymbol(this char c)
        => _languageSymbols.Contains(c);

}
