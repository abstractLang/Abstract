import { Std, Settings } from './std.js';

const stdout = document.querySelector("#stdout");
const stdin = document.querySelector("#stdin");

const webassemblyMainPath = 'cs.replace.webassembly.path';

await _start();
async function _start() {

    const wasmcode = fetch(webassemblyMainPath);
    const rootlibs = { Std: Std };

    const wasminstance = (await WebAssembly.instantiateStreaming(wasmcode, rootlibs)).instance;

    const main = wasminstance.exports.main;
    const mem = wasminstance.exports.mem;

    Settings.define_stdout(append_simple_stdout);
    Settings.define_memory(mem);

    // test
    
    append_stdout("control", "Program started\n");
    main();
    append_stdout("control", "Program finished\n");
}

function append_simple_stdout(text) { append_stdout("", text); }

function append_stdout(classes, text)
{
    text = text.replace("\n", "<br>");
    let clist = classes.split(" ").filter(e => e != "");

    const newline = document.createElement("span");
    if (clist.length > 0) newline.classList.add(clist);
    newline.innerHTML = text;

    console.log(text);

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
