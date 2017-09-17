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
    let dialog = null,
        content = null,
        fullPic = null,
        currentPic = null,
        drawShape = null,
        highlight = null,
        marker = null,
        open = false,
        canvas = null;

    function openDialog() {
        dialog.dialog("open");
        open = true;
    }

    function closeDialog() {
        dialog.dialog("close");
        open = false;
    }

    function buildNestedList(id) {
        let go = function (p, to, open) {
            let ul = document.createElement("ul"),
                span = document.createElement("span"),
                toggleButton = document.createElement("span"),
                collapsed = false;

            let collapse = function () {
                ul.style.display = "none";
                toggleButton.innerHTML = "&#x25B6;";
                collapsed = true;
            }

            let decollapse = function () {
                ul.style.display = "";
                toggleButton.innerHTML = "&#x25BC;";
                collapsed = false;
            }

            if ( p.picture || p.pictures ) {
                toggleButton.classList.add("collapse-button");
                toggleButton.addEventListener("click", function (evt) {
                    if (collapsed) {
                        decollapse();
                    } else {
                        collapse();
                    }
                });

                if (open) {
                    decollapse();
                } else {
                    collapse();
                }
            } else {
                toggleButton.classList.add("collapse-spacer");
            }
            span.appendChild(toggleButton);

            let link = createPicLink(p);
            p.link = link;
            if (open) {
                link.classList.add("piclink-selected");
            }
            span.appendChild( link );
            to.appendChild(span);

            if (p.picture) {
                let li = document.createElement("li");
                go(p.picture, li, open);
                ul.appendChild(li);
                to.appendChild(ul);
            } else if (p.pictures) {
                for (let i=0;i<p.pictures.length;i++) {
                    let li = document.createElement("li"),
                        op = open&&(id > p.pictures[i].id)&&(i==p.pictures.length-1||id<p.pictures[i+1].id);
                    go(p.pictures[i], li, op);
                    ul.appendChild(li);
                }
                to.appendChild(ul);
            }
        }

        let pic = getPicNode(id),
            ul = document.createElement("ul"),
            li = document.createElement("li");

        go(fullPic, li, true);
        ul.appendChild(li);
        return ul;
    }

    function getPicNode(id, cb) {
        let current = fullPic;
        if (!cb) cb = function (x) {};

        while (current.id <= id) {
            cb(current);

            if (current.id == id) {
                return current;
            } else if (current.picture) {
                current = current.picture;
            } else if (current.pictures) {
                let i = current.pictures.length - 1;
                while ( current.pictures[i].id > id ) i--;
                current = current.pictures[i];
            } else {
                return null;
            }
        }
    }

    function getSimpleName(pic) {
        if (pic.type == "pictures") {
            return "pictures";
        } else {
            return pic.name;
        }
    }

    function createPicLink(pic) {
        let a = document.createElement("a");

        a.appendChild( document.createTextNode( getSimpleName(pic) ) );
        a.href = "javascript: void(0);";
        a.classList.add("treedialog-piclink");
        a.addEventListener("click", function (evt) {
            if (marker) marker.clear();

            getPicNode(currentPic.id, function (node) {
                node.link.classList.remove("piclink-selected");
            });
            getPicNode(pic.id, function (node) {
                node.link.classList.add("piclink-selected");
            });

            currentPic = pic;
            dialog.dialog("option", "title", getSimpleName(pic) );
            drawShape(canvas, pic.id);
            if (!pic.pictures) {
                codeworldEditor.setSelection(
                    { line: pic.startLine - 1, ch: pic.startCol - 1 },
                    { line: pic.endLine - 1, ch: pic.endCol - 1 },
                    { origin: "+treedialog" });
            }
        });
        a.addEventListener("mouseover", function (evt) {
            highlight(true, pic.id);

            if (pic.type != "pictures") {
                if (marker) marker.clear();
                marker = codeworldEditor.markText(
                    { line: pic.startLine - 1, ch: pic.startCol - 1 },
                    { line: pic.endLine - 1, ch: pic.endCol - 1 },
                    { origin: "+treedialog", className: "marked" })
            }
        });
        a.addEventListener("mouseout", function (evt) {
            highlight(true, -1);

            if (marker) {
                marker.clear();
                marker = null;
            }
        });
        return a;
    }

    function buildDrawingPreview() {
        let canvas = document.createElement("canvas");

        canvas.width = 250;
        canvas.height = 250;

        return canvas;
    }

    // Globals
    
    function initTreeDialog(pic, highlt, draw) {
        fullPic = pic;
        highlight = highlt;
        drawShape = draw;

        let div = document.createElement("div");
        dialog = $(div).dialog({
            dialogClass: "treedialog",
            title: "Picture Browser",
            closeText: "",
            autoOpen: false,
            height: 650,
            width: 650,
            close: function () {
                open = false;
                highlight(true,-1);
            }
        });

        content = document.createElement("div");
        content.classList.add("treedialog-content");
        dialog.append(content);
    }
    window.initTreeDialog = initTreeDialog;

    function openTreeDialog(id) {
        if (!open) {
            openDialog();
        }

        // Select should probably look better before this is enabled
        // highlight(false, id);
        
        let picture = getPicNode(id);
        currentPic = picture;

        content.innerHTML = "";
        
        canvas = buildDrawingPreview();
        content.appendChild( canvas );
        content.appendChild( buildNestedList(id) );

        drawShape(canvas, id);

        dialog.dialog("option", "title", getSimpleName(picture) );
    }
    window.openTreeDialog = openTreeDialog;

    function closeTreeDialog() {
        closeDialog();
    }
    window.closeTreeDialog = closeTreeDialog;

    function destroyTreeDialog() {
        if (open) {
            closeDialog();
        }
        if (dialog) {
            dialog.remove();
        }
        if (highlight) {
            highlight(false,-1);
        }
        dialog = null;
        content = null;
        highlight = null;
    }
    window.destroyTreeDialog = destroyTreeDialog;
})();
