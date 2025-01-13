using Abstract.CompileTarget.Core;
using Abstract.CompileTarget.Wasm.Wasm;

namespace Abstract.CompileTarget.Wasm.Wat;

public class WatTarget : CompileTargetExtension
{
    public override string TargetID => "wasm-text";
    public override string TargetName => "Webassembly Text Format";
    public override string TargetDescription => "//TODO";

    public override CompilerBase CompilerInstance => new WatCompiler();
}
