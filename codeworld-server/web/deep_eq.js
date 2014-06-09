// Written by luite.  Deep comparison of Haskell values, in
// JavaScript.

function isHeapObject(o) {
    return typeof o === 'object' && o !== null && typeof o.f === 'function';
}

function isByteArray(o) {
    return typeof o === 'object' && typeof o.len === 'number'
        && typeof o.buf === 'object' && typeof o.u8 === 'object'
        && o.buf instanceof ArrayBuffer && o.u8 instanceof Uint8Array;
}

function deepEq(a0, b0) {
    var eq = true;
    var seenA = [];
    var seenB = [];
    var workA = [a0];
    var workB = [b0];
    function addWork(wa, wb) {
        // fixme this is inefficient
        for(var j=seenA.length-1;j>=0;j--) if(wa === seenA[j]) return;
        for(var j=seenB.length-1;j>=0;j--) if(wb === seenB[j]) return;
        workA.push(wa);
        seenA.push(wa);
        workB.push(wb);
        seenB.push(wb);
    }
    while(eq && workA.length > 0) {
        var a = workA.pop();
        var b = workB.pop();
        if(a === null || b === null
           || a === undefined || b === undefined
           || typeof a === 'number'   || typeof b === 'number'
           || typeof a === 'string'   || typeof b === 'string'
           || typeof a === 'boolean'  || typeof b === 'boolean'
           || typeof a === 'function' || typeof b === 'function') {
            eq = a === b;
        } else if(isHeapObject(a) && isHeapObject(b)) {
            eq = a.f === b.f;
            if(eq) {
                addWork(a.d1, b.d1);
                if(typeof a.d2 === 'object' && a.d2 !== null && a.d2.hasOwnProperty('d1')
                   && !isHeapObject(a.d2)) {
                    for(var p in a.d2) addWork(a.d2[p], b.d2[p]);
                } else {
                    addWork(a.d2, b.d2);
                }
            }
        } else if(a instanceof Array && b instanceof Array) {
            eq = a.length === b.length;
            if(eq) for(var i=0;i<a.length;i++) addWork(a[i], b[i]);
        } else if(isByteArray(a) && isByteArray(b)) {
            eq = a.len === b.len;
            var n = a.len;
            while(eq && --n >= 0) {
                eq = a.u8[n] === b.u8[n];
            }
        } else {
            eq = a === b;
        }
    }
    return eq;
}

function getThunks(o) {
    function addWork(x) {
        // fixme this is inefficient
        for(var i=seen.length-1;i>=0;i--) if(seen[j] === x) return;
        work.push(x);
    }
    var work = [];
    var seen = [];
    var thunks = [];
    addWork(o);
    while(work.length > 0) {
        var o = work.pop();
        if(isHeapObject(o)) {
            if((o.f.t === 0 && o.f !== h$unbox_e) || o.f.t === 5) { // thunk or blackhole
                thunks.push(o);
            } else {
                addWork(o.d1);
                if(typeof o.d2 === 'object' && o.d2 !== null && o.d2.hasOwnProperty('d1') && !isHeapObject(o.d2)) {
                    for(var p in o.d2) addWork(o.d2[p]);
                } else {
                    addWork(o.d2);
                }
            }
        }
    }
    return thunks.length === 0 ? null : thunks;
}
