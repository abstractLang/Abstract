using System;

namespace Abstract.Parsing.Core;

/// <summary>
/// Represents a sliced value in a source script.
/// </summary>
/// <param name="value"> The sliced value. </param>
/// <param name="beggin"> The beggining index of the slice. </param>
public readonly struct SourceRange
{

    public readonly Memory<char> value;

    public readonly ulong beggin;
    [System.Diagnostics.CodeAnalysis.SuppressMessage("Style", "IDE1006:Estilos de Nomenclatura", Justification = "<Pendente>")]
    public readonly ulong end => beggin + (ulong)value.Length;

    public SourceRange()
    {
        this.value = new();
        this.beggin = 0;
    }
    public SourceRange(Memory<char> value, ulong beggin)
    {
        this.value = value;
        this.beggin = beggin;
    }

    public override string ToString() => value.ToString();
}
