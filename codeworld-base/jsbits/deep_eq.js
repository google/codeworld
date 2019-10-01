/*
  Copyright 2019 The CodeWorld Authors. All rights reserved.

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
*/

#include <ghcjs/rts.h>  // lgtm [js/syntax-error]

function cw$getThunks(o) {
    function isHeapObject(o) {
        return typeof o === 'object' && o !== null && typeof o.f === 'function';
    }
    function addWork(x) {
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
            if ((o.f.t === CLOSURE_TYPE_THUNK && o.f !== h$unbox_e) ||
                o.f.t === CLOSURE_TYPE_BLACKHOLE) {
                thunks.push(o);
            } else {
                addWork(o.d1);
                if (typeof o.d2 === 'object' && o.d2 !== null &&
                        o.d2.hasOwnProperty('d1') && !isHeapObject(o.d2)) {
                    for(var p in o.d2) addWork(o.d2[p]);
                } else {
                    addWork(o.d2);
                }
            }
        }
    }
    return thunks.length === 0 ? null : thunks;
}

function cw$deepEq(a0, b0) {
    if (a0 === b0) return true;
    var eq = true;
    var seen = [];
    var workA = [a0];
    var workB = [b0];
    function isHeapObject(o) {
        return typeof o === 'object' && o !== null && typeof o.f === 'function';
    }
    function isByteArray(o) {
        return typeof o === 'object' && typeof o.len === 'number'
            && typeof o.buf === 'object' && typeof o.u8 === 'object'
            && o.buf instanceof ArrayBuffer && o.u8 instanceof Uint8Array;
    }
    function addWork(wa, wb) {
        if (wa === wb) return;
        if (typeof wa === 'object' || typeof wb === 'object') {
            for(var j=seen.length-1;j>=0;j--) if(wa === seen[j][0] && wb === seen[j][1]) return;
            seen.push([wa,wb]);
        }
        workA.push(wa);
        workB.push(wb);
    }
    while (eq && workA.length > 0) {
        var a = workA.pop();
        var b = workB.pop();
        while (isHeapObject(a) && a.f === h$unbox_e) a = a.d1;
        while (isHeapObject(b) && b.f === h$unbox_e) b = b.d1;
        if(a === null || b === null
           || a === undefined || b === undefined
           || typeof a === 'number'   || typeof b === 'number'
           || typeof a === 'string'   || typeof b === 'string'
           || typeof a === 'boolean'  || typeof b === 'boolean'
           || typeof a === 'function' || typeof b === 'function') {
            eq = a === b;
        } else if (isHeapObject(a) && isHeapObject(b)) {
            eq = a.f === b.f;
            if (eq) {
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
