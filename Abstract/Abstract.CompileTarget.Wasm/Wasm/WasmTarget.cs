using Abstract.CompileTarget.Core;

namespace Abstract.CompileTarget.Wasm.Wasm;

public class WasmTarget : CompileTargetExtension
{
    public override string TargetID => "wasm";
    public override string TargetName => "Webassembly";
    public override string TargetDescription => "//TODO";

    public override CompilerBase CompilerInstance => new WasmCompiler();
}
