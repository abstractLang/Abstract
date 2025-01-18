import { Std, Settings } from './std.js';

const stdout = document.querySelector("#stdout");
const stdin = document.querySelector("#stdin");

const webassemblyMainPath = './MyProgram.wasm';

await _start();
async function _start() {

    const wasmcode = fetch(webassemblyMainPath);
    const rootlibs = { Std: Std };

    const wasminstance = (await WebAssembly.instantiateStreaming(wasmcode, rootlibs)).instance;

    const main = wasminstance.exports["MyProgram.main"];
    const mem = wasminstance.exports.mem;

    Settings.define_stdout(append_simple_stdout);
    Settings.define_memory(mem);
    
    append_stdout("control", "Program started\n");
    Settings.pre_main();
    main();
    append_stdout("control", "Program finished\n");
}

function append_simple_stdout(text) { append_stdout("", text); }

function append_stdout(classes, text)
{
    let oldtext = text;
    text = handle_escape(text);
    let clist = classes.split(" ").filter(e => e != "");

    const newline = document.createElement("span");
    if (clist.length > 0) newline.classList.add(clist);
    newline.innerHTML = text;

    console.log(oldtext, '\t', text);

    stdout.appendChild(newline);
}
function allow_stdin(mode)
{

    if (mode == "character")
    {
        append_stdout("control", "todo allow stdin");
    }
    else if (mode == "line")
    {
        append_stdout("control", "todo allow stdin");
    }

}

function handle_escape(text)
{
    // common escape characters
    text = text.replace(/\n/g, "<br>");
    text = text.replace(/\t/g, "&nbsp;&nbsp;&nbsp;&nbsp;");

    // placeholder CSI shit
    var csicolorpattern = /{Console\.CSIFGColor\.([a-zA-Z_][a-zA-Z0-9_]*)}/g;

    text = text.replace(csicolorpattern, (_, identifier) => `<span class="fg-${identifier}">`);
    text = text.replace("{Console.CSIGeneral.reset}", '</span>');

    return text;
}
