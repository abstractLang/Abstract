/*
    ABSTRACT STD LIBRARY IMPLEMENTATION
    for webassembly target
*/

export const Settings = {
    define_memory: (e) => {
        memory = new Uint8Array(e.buffer);
        memoryView = new DataView(e.buffer);
    },
    define_stdout: (e) => stdout = e,
    pre_main: () => {
        SetupAllocator()
    }
}

var memory = undefined;
var memoryView = undefined;
var stdout = (e) => console.error("Not implemented! ", e);

export const Std = {

    "Console.write": Console_write,
    "Console.writeln": Console_writeln,

    "Types.String.concatenate": Types_String_concatenate,

    "Compilation.Types.ComptimeString.concatenate": Types_String_concatenate,

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
    let str_a_length = memoryView.getInt32(strptr_a, false);
    let str_b_length = memoryView.getInt32(strptr_b, false);

    // len + str_a(wo \0) + str_b(w0 \0) + \0
    let finallength = 4 + str_a_length + str_b_length + 1;
    let finalptr = Malloc(finallength);

    let str_a = GetStringFromMemory(strptr_a);
    let str_b = GetStringFromMemory(strptr_b);

    console.log(str_a, str_b);

    let finalstr = str_a + str_b;
    let strbuffer = new TextEncoder().encode(finalstr);

    memoryView.setUint32(finalptr, strbuffer.length, false);

    for(let i = 0; i < strbuffer.length; i++)
        memoryView.setUint8(finalptr + 4 + i, strbuffer[i]);

    memoryView.setUint8(finalptr + 4 + strbuffer.length + 1, 0);

    return finalptr;
}

function GetStringFromMemory(strptr) {
    let strlen = memoryView.getInt32(strptr, false);
    var strbuf = memory.slice(strptr+4, strptr + 4 + strlen - 1);
    
    return String.fromCharCode.apply(null, strbuf);
}

const allocator = {
    root: undefined
}

function SetupAllocator() {
    var memstartptr = memoryView.getInt32(0, false);
    var dynmemlength = memory.length - memstartptr;

    var root = { ptr: memstartptr, len: dynmemlength, next: undefined };
    allocator.root = root;
}

function Malloc(bytesize) {
    let lastnode = undefined;
    let curnode = allocator.root;

    while (curnode != null)
    {
        if (curnode.len == bytesize)
        {
            if (lastnode) lastnode.next = curnode.next;
            return curnode.ptr;
        }
        else if (curnode.len > bytesize)
        {
            let ptrbase = curnode.ptr;
            curnode.ptr += bytesize;
            curnode.len -= bytesize;
            return ptrbase;
        }
        else if (curnode.len < bytesize)
        {
            lastnode = curnode;
            curnode = curnode.next;
        }
    }

    console.error("No more memory available for", bytesize, "bytes!");
    return 0;
}

function Mresize() {

}

function Mfree() {

}

