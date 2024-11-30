using Abstract.Parser.Core.Language;
using Abstract.Parser.Core.Language.AbstractSyntaxTree;
using System.Collections;
using System.Diagnostics.CodeAnalysis;

namespace Abstract.Parser;

public partial class Evaluator
{

    private RefDict<NamespaceNode> _globalNamespaces = [];
    private RefDict<List<FunctionNode>> _globalFunctions = [];
    private RefDict<StructureNode> _globalStructures = [];
    private RefDict<TopLevelVariableDeclarationNode> _globalVariables = [];

    public class RefDict<T> : IDictionary<ISymbol, T>
    {

        private List<KeyValuePair<ISymbol, T>> _data = [];

        public ICollection<ISymbol> Keys => [.. _data.Select(x => x.Key)];
        public ICollection<T> Values => [.. _data.Select(x => x.Value)];
        public int Count => _data.Count;
        public bool IsReadOnly => false;

        public T this[ISymbol key] {
            get => _data.First(x => Enumerable.SequenceEqual(x.Key.Tokens, key.Tokens)).Value;
            set
            {
                var idx = _data.FindIndex(x => Enumerable.SequenceEqual(x.Key.Tokens, key.Tokens));
                var obj = _data[idx];
                _data[idx] = new(obj.Key, value);
            }
        }

        public void Add(ISymbol key, T value)
        {
            if (key is ReferenceSymbol @r)
            _data.Add(new(r, value));
        }

        public bool ContainsKey(ISymbol key) => _data.Any(e => Enumerable.SequenceEqual(e.Key.Tokens, key.Tokens));

        public bool Remove(ISymbol key)
        {
            _data.RemoveAt(_data.FindIndex(x => Enumerable.SequenceEqual(x.Key.Tokens, key.Tokens)));
            return true;
        }

        public bool TryGetValue(ISymbol key, [MaybeNullWhen(false)] out T value)
        {
            if (ContainsKey(key))
            {
                value = this[key];
                return true;
            }
            value = default;
            return false;
        }

        public void Add(KeyValuePair<ISymbol, T> item) { }

        public void Clear() => _data.Clear();

        public bool Contains(KeyValuePair<ISymbol, T> item) => false;

        public void CopyTo(KeyValuePair<ISymbol, T>[] array, int arrayIndex) { }

        public bool Remove(KeyValuePair<ISymbol, T> item) => false;

        public IEnumerator<KeyValuePair<ISymbol, T>> GetEnumerator() => _data.AsEnumerable().GetEnumerator();
        IEnumerator IEnumerable.GetEnumerator() => _data.GetEnumerator();

    }

}
