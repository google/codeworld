
/*
 * Copyright 2017 The CodeWorld Authors. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
"use strict";

window.debugMode = false;
window.debugActiveCB = null;

window.infobox = null;

function initDebugMode(getStackAtPoint, active) {
    var canvas = document.getElementById("screen");

    infobox = document.createElement("div");
    infobox.id = "infobox";
    document.body.appendChild(infobox);

    window.debugActiveCB = active;

    canvas.addEventListener("click", function (evt) {
        if (!debugMode) return;

        var ret = getStackAtPoint({
            x: evt.clientX,
            y: evt.clientY,
        });

        var stack = ret.stack;
        if (stack) {
            var pic, i;
            var printable = false;

            var table = document.createElement("table");
            table.classList.add("stack-list");

            infobox.innerHTML = "";
            for (i=stack.length-1;i>=0;i--) {
                pic = stack[i];
                if (!pic) {
                  continue;
                }

                printable = true;

                var row = createSrcLink(pic);
                table.appendChild(row);
            }

            if (printable) {
                infobox.appendChild(table);

                infobox.style.left = evt.clientX + "px";
                infobox.style.top  = evt.clientY + "px";

                infobox.style.display = "block";

                if (evt.clientX + infobox.offsetWidth >= 500) {
                    infobox.style.left = (500 - infobox.offsetWidth) + "px";
                }

                if (evt.clientY + infobox.offsetHeight >= 500) {
                    infobox.style.top = (500 - infobox.offsetHeight) + "px";
                }
            } else {
                // If user clicks on a coordinatePlane, stack may contain
                // only null
                infobox.style.display = "none";
            }
        } else {
            infobox.style.display = "none";
        }
    });

    canvas.onblur = (function (evt) {
        infobox.style.display = "none";
    });
}

function createSrcLink(pic) {
    var tr = document.createElement("tr");
    tr.classList.add("stack-item");
    tr.addEventListener("click", function () {
        parent.codeworldEditor.setSelection(
            { line: pic.srcLoc.startLine - 1, ch: pic.srcLoc.startCol - 1 },
            { line: pic.srcLoc.endLine - 1, ch: pic.srcLoc.endCol - 1 },
            { origin: "+debug" });
    });

    var shapeName = document.createElement("td");
    shapeName.classList.add("shape-name");
    shapeName.appendChild(document.createTextNode(pic.name));
    tr.appendChild(shapeName);

    var shapeLine = document.createElement("td");
    shapeLine.classList.add("shape-loc");
    shapeLine.appendChild(document.createTextNode("Line " + pic.srcLoc.startLine));
    tr.appendChild(shapeLine);

    var shapeCol = document.createElement("td");
    shapeCol.classList.add("shape-loc");
    shapeCol.appendChild(document.createTextNode("Column " + pic.srcLoc.startCol));
    tr.appendChild(shapeCol);

    return tr;
}

function startDebugMode() {
    if (!infobox) {
        throw new Error("Can't start debugMode: isPointInPath not registered via initDebugMode!");
    }
    window.debugMode = true;
    window.debugActiveCB(true);
    parent.updateUI();
}

function stopDebugMode() {
    if (infobox) {
        infobox.style.display = "none";
    }
    window.debugMode = false;
    window.debugActiveCB(false);
    parent.updateUI();
}

function toggleDebugMode() {
    if (window.debugMode) {
        stopDebugMode();
    } else {
        startDebugMode();
    }
}
