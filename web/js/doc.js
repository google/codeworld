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

window.env = parent;
const params = new URLSearchParams(window.location.search);

let position = {
    scrollLeft: 0,
    scrollTop: 0,
    path: params.get('path')
};

function getPositionKey() {
    const shelf = params.get('shelf');
    if (shelf) {
        return `guide.position.${shelf}`;
    } else {
        return 'guide.default-position';
    }
}

function savePosition() {
    sessionStorage.setItem(getPositionKey(), JSON.stringify(position));
}

function loadPosition() {
    const savedPosition = sessionStorage.getItem(getPositionKey());
    if (savedPosition && savedPosition !== 'undefined') {
        position = JSON.parse(savedPosition);
    }
}

function setTheme(el) {
    if (params.get('theme')) {
        el.classList.add(params.get('theme'));
    }
}

window.onscroll = event => {
    position.scrollLeft = event.target.scrollingElement.scrollLeft;
    position.scrollTop = event.target.scrollingElement.scrollTop;
};

(() => {
    loadPosition();

    let shelf = {};
    const contents = {};

    function linkCodeBlocks(elem, linkable = true) {
        window.codeworldKeywords = {};
        registerStandardHints(() => {
            const pres = elem.getElementsByTagName('pre');
            for (let i = 0; i < pres.length; ++i) {
                (() => {
                    const pre = pres[i];

                    // Markdeep buries the class annotations a bit, so we dig.
                    const clickable =
                        pre.classList.contains('clickable') ||
                        (pre.firstChild && pre.firstChild.classList &&
                            pre.firstChild.classList.contains(
                                'clickable')) ||
                        (pre.firstChild && pre.firstChild.firstChild &&
                            pre.firstChild.firstChild.classList &&
                            pre.firstChild.firstChild.classList
                                .contains('clickable'));

                    const text = pre.textContent;
                    pre.innerHTML = '';
                    CodeMirror.runMode(text, {
                        name: 'codeworld',
                        overrideKeywords: codeworldKeywords
                    }, pre);
                    pre.classList.add('cm-s-default');

                    if (linkable && clickable) {
                        pre.classList.add('clickable');
                        pre.onclick = () => {
                            if (env && env.loadSample) {
                                savePosition();
                                env.loadSample(text);
                            }
                        };
                    }
                })();
            }
        });
    }

    function activateCollapsible(root) {
        const elems = root.getElementsByClassName('collapsible');
        for (let i = 0; i < elems.length; ++i) {
            const elem = elems[i];
            elem.onclick = () => {
                if (elem.classList.contains('expanded')) {
                    elem.classList.remove('expanded');
                } else {
                    elem.classList.add('expanded');
                }
            };
        }
    }

    function linkFunBlocks(elem) {
        let blocks = elem.getElementsByTagName('xml');

        while (blocks !== null && blocks.length > 0) {
            const block = blocks[0];
            const text = block.outerHTML;

            const iframe = document.createElement('iframe');
            iframe.setAttribute('frameborder', '0');
            iframe.setAttribute('scrolling', 'no');

            iframe.addEventListener('load', e => {

                const currentTarget = e.currentTarget;
                const contentWindow = currentTarget.contentWindow;

                contentWindow.setParent(parent);
                contentWindow.setId(iframe);
                contentWindow.loadXml.call(iframe.contentWindow,
                    text);
            });

            iframe.src = 'blockframe.html';
            iframe.classList.add('clickable');

            const parent = block.parentNode;
            parent.insertBefore(iframe, block);
            parent.removeChild(block);

            blocks = elem.getElementsByTagName('xml');
        }
    }

    function addTableOfContents(body, outline) {
        const contents = document.createElement('div');
        contents.id = 'helpcontents';
        contents.classList.add('contents');
        setTheme(contents);

        const elems = body.getElementsByTagName('*');

        let currentLevel = 0;
        let currentElem = contents;
        let n = 0;
        for (let i = 0; i < elems.length; ++i) {
            const header = elems[i];
            const match = (/h([1-3])/i).exec(header.tagName);
            if (match === null) continue;
            const level = parseInt(match[1]);

            while (currentLevel < level) {
                const sub = document.createElement('ul');
                currentElem.appendChild(sub);
                ++currentLevel;
                currentElem = sub;
            }

            while (currentLevel > level) {
                currentElem = currentElem.parentNode;
                --currentLevel;
            }

            ++n;

            const anchor = document.createElement('a');
            anchor.setAttribute('name', n);
            anchor.innerHTML = header.innerHTML;
            header.innerHTML = '';
            header.appendChild(anchor);

            const li = document.createElement('li');
            const link = document.createElement('a');
            link.setAttribute('href', `#${n}`);
            link.textContent = header.textContent;
            li.appendChild(link);
            currentElem.appendChild(li);
        }

        while (outline.firstChild) {
            outline.removeChild(outline.firstChild);
        }
        outline.appendChild(contents);
    }

    function addPopout(help) {
        const popdiv = document.createElement('div');
        popdiv.id = 'popout';
        popdiv.style = 'text-align: right';
        const popout = document.createElement('a');
        popout.innerHTML =
            '<i class="mdi mdi-18px mdi-open-in-new"></i>&nbsp;Open the Help in a New Tab';
        popout.target = '_blank';
        popout.href = document.location.href;
        popout.onclick = e => {
            const tab = open(this.href);
            tab.addEventListener('load', () => {
                tab.env = parent;
                if (parent.sweetAlert) {
                    parent.sweetAlert.close();
                }
            });
            e.preventDefault();
        };
        popdiv.appendChild(popout);
        help.appendChild(popdiv);
    }

    function resolvePath(path, baseURL) {
        const url = new URL(path, baseURL);
        if (baseURL.origin === url.origin && baseURL.pathname === url.pathname) {
            return url.hash;
        } else {
            return url.href;
        }
    }

    function relativizeLinks(base, root, tag, attr) {
        const elems = root.getElementsByTagName(tag);
        for (const elem of elems) {
            if (elem.hasAttribute(attr)) {
                elem.setAttribute(attr,
                    resolvePath(elem.getAttribute(attr), base));
            }
        }
    }

    function removeCallStacks(node) {
        if (node.nodeType === Node.TEXT_NODE) {
            node.nodeValue = node.nodeValue.replace(/HasCallStack => /, '');
        } else {
            for (const child of node.childNodes) removeCallStacks(child);
        }
    }

    function setContent(elem) {
        const help = document.getElementById('help');
        setTheme(help);
        while (help.firstChild) {
            help.removeChild(help.firstChild);
        }
        if (parent && parent !== window) {
            addPopout(help);
        }
        help.appendChild(elem);

        document.scrollingElement.scrollLeft = position.scrollLeft;
        document.scrollingElement.scrollTop = position.scrollTop;
    }

    function loadPath(path) {
        if (!path && shelf) path = shelf.default || shelf.named[0][1];
        position.path = path;
        if (contents[path] && contents[path].elem) {
            setContent(contents[path].elem);
        } else {
            const request = new XMLHttpRequest();
            request.open('GET', path, true);
            request.onreadystatechange = () => {
                if (request.readyState !== 4) {
                    return;
                }

                const source = new URL(path, location.href);
                const content = document.createElement('div');
                const raw = request.responseText;

                if (path.endsWith('.md')) {
                    content.innerHTML = window.markdeep.format(raw,
                        false);
                    relativizeLinks(source, content, 'img', 'src');
                    if (shelf && shelf.blocks) {
                        linkFunBlocks(content);
                        linkCodeBlocks(content, false);
                        activateCollapsible(content);
                    } else {
                        linkCodeBlocks(content);
                        activateCollapsible(content);
                    }
                } else {
                    content.innerHTML = raw;
                    relativizeLinks(source, content, 'script', 'src');
                    relativizeLinks(source, content, 'link', 'href');
                    relativizeLinks(source, content, 'a', 'href');
                    relativizeLinks(source, content, 'img', 'src');
                    removeCallStacks(content);
                }

                const spacerDiv = document.createElement('div');
                spacerDiv.style = 'height: 90vh';
                content.appendChild(spacerDiv);

                if (!contents[path]) contents[path] = {};
                contents[path].elem = content;

                if (contents[path].outline) {
                    addTableOfContents(content, contents[path].outline);
                }
                setContent(content);
            };
            request.send(null);
        }
    }

    function loadSidebar() {
        let path = position.path;
        if (!path) path = shelf.default;

        let activeIndex = false;

        const acc = document.createElement('div');
        acc.id = 'helpacc';
        setTheme(acc);

        let paneNum = 0;
        for (const doc in shelf.named) {
            const hdr = document.createElement('h3');
            hdr.innerText = doc;
            acc.appendChild(hdr);

            const entry = document.createElement('div');
            entry.classList.add('accentry');
            entry.innerHTML = '<div>Loading...</div>';
            acc.appendChild(entry);

            contents[shelf.named[doc]] = {
                title: doc,
                header: hdr,
                outline: entry,
                index: paneNum,
                elem: null
            };

            if (shelf.named[doc] === path) activeIndex = paneNum;
            paneNum++;
        }

        document.body.insertBefore(acc, document.body.firstChild);
        $(acc).accordion({
            collapsible: true,
            active: activeIndex,
            heightStyle: 'content',
            beforeActivate: (event, ui) => {
                position.scrollLeft = 0;
                position.scrollTop = 0;
                const name = ui.newHeader.text();
                const path = name && shelf.named[name];
                if (path) loadPath(path);
            }
        });
    }

    function resolveShelfPath(path, baseURL) {
        // The first three cases are for backward-compatibility in case
        // caching on the shelf file is different from the js file.  The
        // plan is to migrate in three steps:
        //
        // 1. Change just this JavaScript file.  Let it propagate.
        // 2. Change the shelf files to use proper relative paths.
        // 3. Remove the compatibility cases here.
        //
        // There will be enough time between steps to refresh the caches
        // of most users.
        if (path.startsWith('help/')) {
            path = path.slice(5);
        } else if (path.startsWith('doc/') ||
            path.startsWith('doc-haskell/')) {
            path = `../${path}`;
        }

        return resolvePath(path, baseURL);
    }

    function resolveShelfPaths(shelf, baseURL) {
        shelf.default = resolveShelfPath(shelf.default, baseURL);
        for (const page in shelf.named) {
            shelf.named[page] = resolveShelfPath(shelf.named[page], baseURL);
        }
    }

    const markdeepStyle = document.createElement('style');
    document.head.appendChild(markdeepStyle);
    markdeepStyle.outerHTML = window.markdeep.stylesheet();

    if (params.get('shelf')) {
        const request = new XMLHttpRequest();
        request.open('GET', params.get('shelf'), true);
        request.onreadystatechange = () => {
            if (request.readyState !== 4) {
                return;
            }

            shelf = JSON.parse(request.responseText);
            const shelfURL = new URL(params.get('shelf'), location.href);
            resolveShelfPaths(shelf, shelfURL);
            loadSidebar();
            loadPath(position.path);
        };
        request.send(null);
    } else {
        shelf = null;
        loadPath(params.get('path'));
    }
})();
