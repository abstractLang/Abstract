/*
    ABSTRACT STD LIBRARY IMPLEMENTATION
    for webassembly target
*/

export const Settings = {
    define_memory: (e) => {
        memory = new Uint8Array(e.buffer);
        memoryView = new DataView(e.buffer);
    },
    define_stdout: (e) => stdout = e
}

var memory = undefined;
var memoryView = undefined;
var stdout = (e) => console.error("Not implemented! ", e);

export const Std = {

    "Console.write": Console_write,
    "Console.writeln": Console_writeln,

    "Types.String.concatenate": Types_String_concatenate

}

function Console_write(strptr) {
    var str = GetStringFromMemory(strptr);
    stdout(str);
}
function Console_writeln(strptr) {
    var str = GetStringFromMemory(strptr);
    stdout(str + '\n');
}


function Types_String_concatenate(strptr_a, strptr_b) {
    var str_a = GetStringFromMemory(strptr_a);
    var str_b = GetStringFromMemory(strptr_b);

    console.log(str_a + str_b);
    //str_a + str_b;

    // Allocation needed here :D
}

function GetStringFromMemory(strptr) {
    let strlen = memoryView.getInt32(strptr, false);
    var strbuf = memory.slice(strptr+4, strptr + 4 + strlen - 1);
    
    return String.fromCharCode.apply(null, strbuf);
}
