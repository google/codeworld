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
        highlight = null,
        marker = null,
        open = false;

    function openDialog() {
        let div = document.createElement("div");
        dialog = $(div).dialog({
            dialogClass: "treedialog",
            title: "Picture Browser",
            closeText: ""
        });

        content = document.createElement("div");
        content.classList.add("treedialog-content");
        dialog.append(content);

        open = true;
    }

    function closeDialog() {
        dialog.dialog("close");
        dialog.remove();
        open = false;
    }

    function buildNestedList(id) {
        let go = function (p, to, c) {
            let ul = document.createElement("ul"),
                span = document.createElement("span"),
                collapsed = false,
                collapse = document.createElement("span");

            if ( p.picture || p.pictures ) {
                collapse.classList.add("collapse-button");
                collapse.innerHTML = "&#x25BC;";
                collapse.addEventListener("click", function (evt) {
                    if (collapsed) {
                        ul.style.display = "";
                        collapse.innerHTML = "&#x25BC;";
                        collapsed = false;
                    } else {
                        ul.style.display = "none";
                        collapse.innerHTML = "&#x25b6;";
                        collapsed = true;
                    }
                });
                if ( c ) {
                    ul.style.display = "none";
                    collapse.innerHTML = "&#x25b6;";
                    collapsed = true;
                }
            } else {
                collapse.classList.add("collapse-spacer");
            }
            span.appendChild(collapse);

            span.appendChild( createPicLink(p) );
            to.appendChild(span);

            if (p.picture) {
                let li = document.createElement("li");
                go(p.picture, li, false);
                ul.appendChild(li);
                to.appendChild(ul);
            } else if (p.pictures) {
                for (let i=0;i<p.pictures.length;i++) {
                    let li = document.createElement("li");
                    go(p.pictures[i], li, false);
                    ul.appendChild(li);
                }
                to.appendChild(ul);
            }
        }

        let pic = getPicNode(id),
            ul = document.createElement("ul"),
            li = document.createElement("li");

        go(pic, li, true);
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
            openTreeDialog(pic.id);
            if (marker) marker.clear();
        });
        a.addEventListener("mouseover", function (evt) {
            highlight(pic.id);

            if (pic.type != "pictures") {
                if (marker) marker.clear();
                marker = parent.codeworldEditor.markText(
                    { line: pic.startLine - 1, ch: pic.startCol - 1 },
                    { line: pic.endLine - 1, ch: pic.endCol - 1 },
                    { origin: "+treedialog", className: "marked" })
            }
        });
        a.addEventListener("mouseout", function (evt) {
            highlight(-1);

            if (marker) {
                marker.clear();
                marker = null;
            }
        });
        return a;
    }

    function num2str(d) {
        let n = Math.floor(d).toString(),
            p = (d - n).toString().slice(2,5);

        while ( p.length>0 && p[p.length-1] == "0" ) p = p.slice(0,p.length-1);

        if (p.length == 0) {
            return n;
        } else {
            return n + "." + p;
        }
    }

    function buildPicPath(id) {
        let span = document.createElement("span");
        let first = true;

        getPicNode(id, function (pic) {
            if (first) {
                first = false;
            } else {
                span.appendChild( document.createTextNode(" / ") );
            }

            span.appendChild( createPicLink(pic) );
        });

        return span;
    }

    function optTxt(txt) {
        let span = document.createElement("span");
        span.appendChild( document.createTextNode( txt ) );
        span.classList.add("tree-opt");
        return span;
    }

    function buildFullDescription(pic) {
        let span = document.createElement("span"),
            txt = function (t) {
                span.appendChild( document.createTextNode(t) );
            },
            opt = function (t) {
                span.appendChild( optTxt(t) );
            },
            pts = function (ps) {
                if (ps.length == 0) {
                    return opt("(none)");
                } else {
                    let t = "";
                    for (let i=0;i<ps.length;i+=2) {
                        if ( i!= 0 ) t+= ",";
                        t += "("+num2str(ps[i])+","+num2str(ps[i+1])+")";
                    }
                    return opt(t);
                }
            },
            piclink = function (p) {
                span.appendChild( createPicLink(p) );
            };

        if ( pic.type == "polygon" ) {
            txt("This is a ");
            opt("polygon");
            txt(" which is ");
            opt(pic.smooth?"smooth":"not smooth");
            txt(" and contains the points ");
            pts(pic.points);
            txt(".");
        } else if ( pic.type == "path" ) {
            txt("This is a ");
            opt("path");
            txt(" of width ");
            opt(num2str(pic.width));
            txt(" which is ");
            opt(pic.closed?"closed":"not closed");
            txt(" and ");
            opt(pic.smooth?"smooth":"not smooth");
            txt(" and contains the points ");
            pts(pic.points);
            txt(".");
        } else if ( pic.type == "sector" ) {
            txt("This is a ");
            opt("sector");
            txt(" of radius ");
            opt(pic.radius);
            txt(" starting at ");
            opt(num2str(180*pic.startAngle/Math.PI) + "\u00B0");
            txt(" and ending at ");
            opt(num2str(180*pic.endAngle/Math.PI) + "\u00B0");
            txt(".");
        } else if ( pic.type == "arc" ) {
            txt("This is an ");
            opt("arc");
            txt(" of width ");
            opt(num2str(pic.width));
            txt(" and of radius ");
            opt(num2str(pic.radius));
            txt(" starting at ");
            opt(num2str(180*pic.startAngle/Math.PI) + "\u00B0");
            txt(" and ending at ");
            opt(num2str(180*pic.endAngle/Math.PI) + "\u00B0");
            txt(".");
        } else if ( pic.type == "text") {
            txt("This is ");
            opt("text");
            txt(" with font ");
            opt(pic.font);
            txt(" of the string ");
            opt(pic.text);
            txt(".");
        } else if ( pic.type == "color" ) {
            txt("This is a ");
            opt("colored");
            txt(" picture of ");
            opt(num2str(100*pic.red) + "%");
            txt(" red, ");
            opt(num2str(100*pic.green) + "%");
            txt(" green, and ");
            opt(num2str(100*pic.blue) + "%");
            txt(" blue. The picture being colored is a ");
            piclink(pic.picture);
            txt(".");
        } else if ( pic.type == "translate" ) {
            txt("This is a ");
            opt("translated");
            txt(" picture of ");
            opt(num2str(pic.x));
            txt(" units on the x-axis and ");
            opt(num2str(pic.y));
            txt(" units on the y-axis. The picture being translated is a ");
            piclink(pic.picture);
            txt(".");
        } else if ( pic.type == "scale" ) {
            txt("This is a ");
            opt("scaled");
            txt(" picture of ");
            opt(num2str(pic.x));
            txt(" units on the x-axis and ");
            opt(num2str(pic.y));
            txt(" units on the y-axis. The picture being scaled is a ");
            piclink(pic.picture);
            txt(".");
        } else if ( pic.type == "rotate" ) {
            txt("This is a ");
            opt("rotated");
            txt(" picture of ");
            opt(num2str(180*pic.angle/Math.PI) + "\u00B0");
            txt(". The picture being rotated is a ");
            piclink(pic.picture);
            txt(".");
        } else if ( pic.type == "pictures" ) {
            if ( pic.pictures.length == 0 ) {
                txt("This is an empty picture.");
            } else {
                txt("This is ");
                opt(pic.pictures.length);
                txt(" combined pictures. In order of foreground to background they are ");
                piclink(pic.pictures[0]);
                for (let i=1;i<pic.pictures.length;i++) {
                    if ( i == pic.pictures.length - 1) {
                        txt(" and ");
                    } else {
                        txt(", ");
                    }
                    piclink(pic.pictures[i]);
                }
                txt(".");
            }
        } else {
            txt("This is a ");
            opt(pic.type);
            txt(".");
        }

        return span;
    }

    // Globals
    
    function initTreeDialog(pic, highlt) {
        fullPic = pic;
        highlight = highlt;
    }
    window.initTreeDialog = initTreeDialog;

    function openTreeDialog(id) {
        if (!open) {
            openDialog();
        }

        content.innerHTML = "";

        let picture = getPicNode(id);

        let path = document.createElement("div"),
            pathtitle = document.createElement("span");
        pathtitle.appendChild( document.createTextNode("Full Path: ") );
        pathtitle.classList.add("pathtitle");
        path.appendChild(pathtitle);
        path.appendChild( buildPicPath(id) );
        path.classList.add("tree-fullpath");
        content.appendChild(path);

        let header = document.createElement("div"),
            headertitle = document.createElement("h2");
        headertitle.appendChild( document.createTextNode(getSimpleName(picture)) );
        header.appendChild(headertitle);
        header.appendChild(buildFullDescription(picture));
        content.appendChild(header);

        let tree = document.createElement("div"),
            treetitle = document.createElement("h3");
        treetitle.appendChild( document.createTextNode("Tree") );
        tree.appendChild(treetitle);
        tree.appendChild(buildNestedList(id));
        content.appendChild(tree);
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
        dialog = null;
        content = null;
        highlight = null;
    }
    window.destroyTreeDialog = destroyTreeDialog;
})();
