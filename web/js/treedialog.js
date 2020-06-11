/*
 * Copyright 2020 The CodeWorld Authors. All rights reserved.
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
'use strict';

(() => {
    let dialog = null,
        content = null,
        fullPic = null,
        currentPic = null,
        marker = null,
        open = false;

    function openDialog() {
        dialog.dialog('open');
        open = true;
    }

    function closeDialog() {
        dialog.dialog('close');
        open = false;
    }

    function buildNestedList(id) {
        const go = (p, to, open) => {
            const ul = document.createElement('ul');
            const span = document.createElement('span');
            const toggleButton = document.createElement('span');

            let collapsed = false;
            const collapse = () => {
                ul.style.display = 'none';
                toggleButton.innerHTML = '&#x25B6;';
                collapsed = true;
            };
            const decollapse = () => {
                ul.style.display = '';
                toggleButton.innerHTML = '&#x25BC;';
                collapsed = false;
            };

            if (p.picture || p.pictures) {
                toggleButton.classList.add('collapse-button');
                toggleButton.addEventListener('click', evt => {
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
                toggleButton.classList.add('collapse-spacer');
            }
            span.appendChild(toggleButton);

            const link = createPicLink(p);
            p.link = link;
            if (open) {
                link.click();
            }
            span.appendChild(link);
            to.appendChild(span);

            if (p.picture) {
                const li = document.createElement('li');
                go(p.picture, li, open);
                ul.appendChild(li);
                to.appendChild(ul);
            } else if (p.pictures) {
                for (let i = 0; i < p.pictures.length; i++) {
                    const li = document.createElement('li');
                    const op = open && (id >= p.pictures[i].id) &&
                        (i === p.pictures.length - 1 || id < p.pictures[i + 1].id);
                    go(p.pictures[i], li, op);
                    ul.appendChild(li);
                }
                to.appendChild(ul);
            }
        };

        const ul = document.createElement('ul');
        const li = document.createElement('li');

        go(fullPic, li, true);
        ul.appendChild(li);
        return ul;
    }

    function getPicNode(id, cb) {
        let current = fullPic;
        if (!cb) cb = x => {};

        while (current.id <= id) {
            cb(current);

            if (current.id === id) {
                return current;
            } else if (current.picture) {
                current = current.picture;
            } else if (current.pictures) {
                let i = current.pictures.length - 1;
                while (current.pictures[i].id > id) i--;
                current = current.pictures[i];
            } else {
                return null;
            }
        }
    }

    function createPicLink(pic) {
        const a = document.createElement('a');

        a.appendChild(document.createTextNode(pic.name));
        a.href = 'javascript: void(0);';
        a.classList.add('treedialog-piclink');
        a.addEventListener('click', evt => {
            select(pic.id);

            if (marker) marker.clear();

            getPicNode(currentPic.id, node => {
                node.link.classList.remove(
                    'piclink-selected');
            });
            getPicNode(pic.id, node => {
                node.link.classList.add('piclink-selected');
            });

            currentPic = pic;
            dialog.dialog('option', 'title', pic.name);
            if (pic.startLine && pic.startCol && pic.endLine && pic
                .endCol) {
                codeworldEditor.setSelection({
                    line: pic.startLine - 1,
                    ch: pic.startCol - 1
                }, {
                    line: pic.endLine - 1,
                    ch: pic.endCol - 1
                }, {
                    origin: '+treedialog'
                });
            }
        });
        a.addEventListener('mouseover', evt => {
            highlight(pic.id);

            if (pic.startLine && pic.startCol && pic.endLine && pic
                .endCol) {
                if (marker) marker.clear();
                marker = codeworldEditor.markText({
                    line: pic.startLine - 1,
                    ch: pic.startCol - 1
                }, {
                    line: pic.endLine - 1,
                    ch: pic.endCol - 1
                }, {
                    origin: '+treedialog',
                    className: 'marked'
                });
            }
        });
        a.addEventListener('mouseout', evt => {
            highlight(-1);

            if (marker) {
                marker.clear();
                marker = null;
            }
        });
        return a;
    }

    // Globals

    function highlight(nodeId) {
        const runner = document.getElementById('runner');
        runner.contentWindow.postMessage({
            type: 'debugHighlight',
            nodeId: nodeId
        }, '*');
    }

    function select(nodeId) {
        const runner = document.getElementById('runner');
        runner.contentWindow.postMessage({
            type: 'debugSelect',
            nodeId: nodeId
        }, '*');
    }

    function cancelDebug() {
        const runner = document.getElementById('runner');
        runner.contentWindow.postMessage({
            type: 'stopDebug'
        }, '*');
    }

    function initTreeDialog(pic) {
        fullPic = pic;

        const div = document.createElement('div');
        dialog = $(div).dialog({
            dialogClass: 'treedialog',
            title: 'Picture Browser',
            closeText: '',
            autoOpen: false,
            height: 650,
            width: 650,
            close: () => {
                open = false;
                highlight(-1);
                select(-1);
                cancelDebug();
            }
        });

        content = document.createElement('div');
        content.classList.add('treedialog-content');
        dialog.append(content);
    }
    window.initTreeDialog = initTreeDialog;

    function selectNode(id) {
        if (!open) {
            openDialog();
        }

        select(id);

        const picture = getPicNode(id);
        currentPic = picture;

        content.innerHTML = '';

        content.appendChild(buildNestedList(id));

        dialog.dialog('option', 'title', picture.name);
    }

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
        highlight(-1);
        dialog = null;
        content = null;
    }
    window.destroyTreeDialog = destroyTreeDialog;

    window.addEventListener('message', event => {
        if (!event.data.type) return;

        if (event.data.type === 'debugActive') {
            initTreeDialog(event.data.fullPic);
            selectNode(0);
        }
        if (event.data.type === 'nodeClicked') {
            selectNode(event.data.nodeId);
        }
        if (event.data.type === 'nodeHovered') {
            // For now, do nothing.
        } else if (event.data.type === 'debugFinished') {
            destroyTreeDialog();
        }
    });
})();
