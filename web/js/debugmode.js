
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

(function () {
    let available = false;
    let active = false;

    // Checked by parent.updateUI
    window.debugAvailable = false;
    window.debugActive = false;

    // These functions are provided by a debugmode-supported entrypoint when
    // calling initDebugMode
    //  dGetNode :: { x :: Double, y :: Double } -> Int
    //   Returns the nodeId of the shape at the coordinate (x,y) of the canvas.
    //   Negative return indicates no shape at that point.
    let dGetNode = null;
    //  dSetActive :: Bool -> ()
    //   Indicates to the entry point when debugmode has been turned off and on
    let dSetActive = null;
    //  dGetPicture :: () -> Object
    //   Gets an object showing the current state of the Picture being drawn. Is
    //   only called directly after dSetActive(true).
    let dGetPicture = null;
    //  dHighlightShape :: (Bool, Int) -> ()
    //   Indicates to the entry point should highlight or select a shape or tree
    //   of shapes. A true value indicates highlight (change color and bring to
    //   front) and false indicates select (change color and do not change
    //   position). A negative value indicates to stop highlighting or selecting.
    //   At most one shape may be highlighted and one shape selected at a time.
    let dHighlightShape = null;

    let infobox = null;
    let cachedPic = null;
    let canvas = null;

    function showInfobox(x, y) {
        let nodeId = dGetNode({x,y});

        if (nodeId < 0) {
            infobox.style.display = "none";
            dHighlightShape(false, -1);
            return;
        }

        let stack = getPictureStack(cachedPic, nodeId);
        if ( !stack ) throw new Error("Got nonexistent nodeId");

        let stackInterface = getStackTable(stack);
        infobox.appendChild(stackInterface);

        infobox.style.display = "block";
        infobox.style.left = x + "px";
        infobox.style.top  = y + "px";

        let infoboxWidth  = infobox.offsetWidth,
            infoboxHeight = infobox.offsetHeight;

        if ( x + infoboxWidth >= 500) {
            infobox.style.left = (500 - infoboxWidth) + "px";
        }

        if ( y + infoboxHeight >= 500) {
            infobox.style.top = (500 - infoboxHeight) + "px";
        }

        dHighlightShape(false, nodeId);
    }

    function getPictureStack(pic, toId) {
        let stack = [];

        let current = pic;
        while ( current.id <= toId ) {
            if ( current.id == toId ) {
                stack.push(current);
                return stack;
            } else if ( current.picture ) {
                stack.push(current);
                current = current.picture;
            } else if ( current.pictures ) {
                // Deliberately leave out entries for Pictures
                let i = current.pictures.length - 1;
                while ( toId < current.pictures[i].id ) i--;
                current = current.pictures[i];
            } else {
                return null;
            }
        }

        return null;
    }

    function getStackTable(stack) {
        let table = document.createElement("table");
        table.classList.add("stack-list");

        infobox.innerHTML = "";

        stack.forEach(function (pic) {
            table.appendChild( createStackRow(pic) );
        });

        return table;
    }

    function createStackRow(pic) {
        let tr = document.createElement("tr");
        tr.classList.add("stack-item");
        tr.addEventListener("click", function () {
            parent.codeworldEditor.setSelection(
                { line: pic.startLine - 1, ch: pic.startCol - 1 },
                { line: pic.endLine - 1, ch: pic.endCol - 1 },
                { origin: "+debug" });
        });

        let shapeInfo = document.createElement("td");
        shapeInfo.classList.add("shape-info");
        shapeInfo.innerHTML = "&#x24d8;";
        tr.appendChild(shapeInfo);

        shapeInfo.addEventListener("click", function () {
            openTreeDialog(pic.id);
        });

        let shapeName = document.createElement("td");
        shapeName.classList.add("shape-name");
        shapeName.appendChild(document.createTextNode(pic.name));
        tr.appendChild(shapeName);

        let shapeLine = document.createElement("td");
        shapeLine.classList.add("shape-loc");
        shapeLine.appendChild(document.createTextNode("Line " + pic.startLine));
        tr.appendChild(shapeLine);

        let shapeCol = document.createElement("td");
        shapeCol.classList.add("shape-loc");
        shapeCol.appendChild(document.createTextNode("Column " + pic.startCol));
        tr.appendChild(shapeCol);

        return tr;
    }

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

    function openTreeDialog(id) {
        parent.initTreeDialog(cachedPic, function (n) {
            dHighlightShape(true, n);
        });
        parent.openTreeDialog(id);
    }

    function closeTreeDialog() {
        parent.destroyTreeDialog();
    }

    // Globals

    function initDebugMode(getNode, setActive, getPicture, highlightShape) {
        dGetNode = getNode;
        dSetActive = setActive;
        dGetPicture = getPicture;
        dHighlightShape = highlightShape;

        if (available) {
            infobox.style.display = "none";
        } else {
            canvas = document.getElementById("screen");

            infobox = document.createElement("div");
            infobox.id = "infobox";
            document.body.appendChild(infobox);

            canvas.addEventListener("blur", function (evt) {
                infobox.style.display = "none";
            });

            canvas.addEventListener("mousemove", function (evt) {
                if (active) {
                    let nodeId = dGetNode({
                        x: evt.clientX,
                        y: evt.clientY
                    });

                    dHighlightShape(true, nodeId);
                }
            });

            canvas.addEventListener("mouseout", function (evt) {
                if (active) {
                    dHighlightShape(true, -1);
                }
            });
            
            canvas.addEventListener("click", function (evt) {
                if (active) {
                    showInfobox(evt.clientX, evt.clientY);
                }
            });

            available = true;
            window.debugAvailable = true;
        }

    }
    window.initDebugMode = initDebugMode;

    function startDebugMode() {
        if (!available) {
            throw new Error("Debug mode is not available.");
        }

        active = true;
        dSetActive(true);
        cachedPic = dGetPicture();

        window.debugActive = true;
        parent.updateUI()
    }
    window.startDebugMode = startDebugMode;

    function stopDebugMode() {
        if (active) {
            infobox.style.display = "none";
        }

        active = false;
        dSetActive(false);
        cachedPic = null;

        closeTreeDialog();

        window.debugActive = false;
        parent.updateUI();
    }
    window.stopDebugMode = stopDebugMode;

    function toggleDebugMode() {
        if (active) {
            stopDebugMode();
        } else {
            startDebugMode();
        }
    }
    window.toggleDebugMode = toggleDebugMode;
})();
