using System;

namespace Abstract.Core.Language;

/// <summary>
/// Represents a sliced value in a source script.
/// </summary>
/// <param name="value"> The sliced value. </param>
/// <param name="beggin"> The beginning index of the slice. </param>
public readonly struct SourceRange
{

    public readonly ReadOnlyMemory<char> value;

    public readonly ulong begin;
    [System.Diagnostics.CodeAnalysis.SuppressMessage("Style", "IDE1006:Estilos de Nomenclatura", Justification = "<Pendente>")]
    public readonly ulong end => begin + (ulong)value.Length;

    public SourceRange()
    {
        this.value = new();
        this.begin = 0;
    }
    public SourceRange(Memory<char> value, ulong begin)
    {
        this.value = value;
        this.begin = begin;
    }

    public override string ToString() => value.ToString();
}
