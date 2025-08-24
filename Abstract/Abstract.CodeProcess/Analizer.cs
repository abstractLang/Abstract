using System.Collections;
using Abstract.CodeProcess.Core;
using Abstract.CodeProcess.Core.Language;
using Abstract.CodeProcess.Core.Language.SyntaxNodes.Base;

namespace Abstract.CodeProcess;

public class Analizer(ErrorHandler handler)
{
    private ErrorHandler _errorHandler = handler;

    private Dictionary<string[], SyntaxNode> _globalReferenceTable = new(new IdentifierComparer());
    
    public void Analize(Module[] modules)
    {
        SearchReferences(modules);


    }

    private void SearchReferences(Module[] modules)
    {

    }

 
    
    private class IdentifierComparer : IEqualityComparer<string[]>
    {
        public bool Equals(string[]? x, string[]? y)
        {
            if (x is null || y is null) return false;
            return x.SequenceEqual(y);
        }

        public int GetHashCode(string[] key)
        {
            var val = string.Join('.', key);
            return HashCode.Combine(val);
        }
    }
}
