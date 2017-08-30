
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

window.debugActive = false;
window.debugAvailable = false;

// These functions are provided by a debugmode-supported entrypoint when
// calling initDebugMode
window.debugGetNode = null;
window.debugSetActive = null;
window.debugGetPicture = null;
window.debugHighlightShape = null;
window.debugSelectShape = null;

window.infobox = null;
window.debugPic = null;

function initDebugMode(getNode, setActive, getPicture, highlightShape, selectShape) {
    window.debugGetNode = getNode;
    window.debugSetActive = setActive;
    window.debugGetPicture = getPicture;
    window.debugHighlightShape = highlightShape;
    window.debugSelectShape = selectShape;

    var canvas = document.getElementById("screen");

    infobox = document.createElement("div");
    infobox.id = "infobox";
    document.body.appendChild(infobox);

    canvas.onblur = (function (evt) {
        infobox.style.display = "none";
    });

    window.debugAvailable = true;
}

function createSrcLink(pic) {
    var tr = document.createElement("tr");
    tr.classList.add("stack-item");
    tr.addEventListener("click", function () {
        parent.codeworldEditor.setSelection(
            { line: pic.startLine - 1, ch: pic.startCol - 1 },
            { line: pic.endLine - 1, ch: pic.endCol - 1 },
            { origin: "+debug" });
    });

    var shapeName = document.createElement("td");
    shapeName.classList.add("shape-name");
    shapeName.appendChild(document.createTextNode(pic.name));
    tr.appendChild(shapeName);

    var shapeLine = document.createElement("td");
    shapeLine.classList.add("shape-loc");
    shapeLine.appendChild(document.createTextNode("Line " + pic.startLine));
    tr.appendChild(shapeLine);

    var shapeCol = document.createElement("td");
    shapeCol.classList.add("shape-loc");
    shapeCol.appendChild(document.createTextNode("Column " + pic.startCol));
    tr.appendChild(shapeCol);

    return tr;
}

function startDebugMode() {
    if (!window.debugAvailable) {
        throw new Error("Debug mode is not available.");
    }
    window.debugActive = true;
    window.debugSetActive(true);
    window.debugCurrentPic = debugGetPicture();
    parent.updateUI();
}

function stopDebugMode() {
    if (infobox) {
        infobox.style.display = "none";
    }
    window.debugActive = false;
    window.debugSetActive(false);
    window.debugCurrentPic = null;
    parent.updateUI();
}

function toggleDebugMode() {
    if (window.debugActive) {
        stopDebugMode();
    } else {
        startDebugMode();
    }
}

document.getElementById("screen").addEventListener("click", function (evt) {
    if (!window.debugActive) return;

    var nodeId = window.debugGetNode({
        x: evt.clientX,
        y: evt.clientY
    });

    if (nodeId<0) {
        infobox.style.display = "none";
        return;
    }

    var pic, i;
    var printable = false;

    var table = document.createElement("table");
    table.classList.add("stack-list");

    infobox.innerHTML = "";

    var currentNode = debugCurrentPic;
    while (true) {
        if (currentNode.type == "pictures") {
            for (i=currentNode.pictures.length-1;nodeId<currentNode.pictures[i].id;i--);
            currentNode = currentNode.pictures[i];
            continue;
        }

        printable = true;

        var row = createSrcLink(currentNode);
        table.appendChild(row);

        if ( currentNode.type == "color" || currentNode.type == "translate" ||
             currentNode.type == "scale" || currentNode.type == "rotate" ) {
            currentNode = currentNode.picture;
        } else if (currentNode.id == nodeId) {
            break;
        } else {
            console.log(debugCurrentPic, currentNode);
            throw new Error("Unable to find node " + nodeId + ".");
        }
    }

    if (printable) {
        infobox.appendChild(table);

        var showFullTree = document.createElement("a");
        showFullTree.innerHTML = "Show full tree.";
        showFullTree.href = "#";

        infobox.appendChild(document.createElement("br"));
        infobox.appendChild(showFullTree);

        showFullTree.addEventListener("click", fullTreeMode);

        infobox.style.left = evt.clientX + "px";
        infobox.style.top  = evt.clientY + "px";

        infobox.style.display = "block";

        if (evt.clientX + infobox.offsetWidth >= 500) {
            infobox.style.left = (500 - infobox.offsetWidth) + "px";
        }

        if (evt.clientY + infobox.offsetHeight >= 500) {
            infobox.style.top = (500 - infobox.offsetHeight) + "px";
        }

        window.debugHighlightShape(false,nodeId);
    } else {
        // If user clicks on a coordinatePlane, stack may contain
        // only null
        infobox.style.display = "none";
    }
});

function fullTreeMode() {
    infobox.innerHTML = "";

    var ul = document.createElement("ul");
    appendPicTree(debugCurrentPic, ul);
    infobox.appendChild(ul);
}

function appendPicTree(tree,to) {
    var li = document.createElement("li");
    var ul, i;


    if (tree.pictures) {
        li.appendChild(document.createTextNode("Pictures\n"));
        ul = document.createElement("ul");
        for (i=0;i<tree.pictures.length;i++) {
            console.log(i, tree.pictures[i]);
            appendPicTree(tree.pictures[i], ul);
        }
        li.appendChild(ul);
    } else if (tree.picture) {
        li.appendChild(document.createTextNode(tree.name+"\n"));
        ul = document.createElement("ul");
        appendPicTree(tree.picture, ul);
        li.appendChild(ul);
    } else {
        li.appendChild(document.createTextNode(tree.name+"\n"));
    }

    to.appendChild(li);
}

window.addEventListener("blur", function (evt) {
    if (!window.debugMode) return;

    window.infobox.style.display = "none";
});
document.getElementById("screen").addEventListener("mousemove", function (evt) {
    if (!window.debugActive) return;

    var nodeId = window.debugGetNode({
        x: evt.clientX,
        y: evt.clientY
    });

    window.debugHighlightShape(true,nodeId);
});

document.getElementById("screen").addEventListener("mouseout", function (evt) {
    if (!window.debugActive) return;

    window.debugHighlightShape(true,-1);
});
