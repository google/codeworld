/*
 * Copyright 2019 The CodeWorld Authors. All rights reserved.
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
window.env = parent;
let params = new URLSearchParams(window.location.search);
let position = {
    pageX: 0,
    pageY: 0,
    path: params.get('path')
};

function savePosition() {
    sessionStorage.setItem('position', JSON.stringify(position));
};

function loadPosition() {
    let savedPosition = sessionStorage.getItem('position');
    if (savedPosition && savedPosition != "undefined") {
        position = JSON.parse(savedPosition)
    }
};
window.onscroll = event => {
    position.pageX = event.pageX;
    position.pageY = event.pageY;
};
window.onload = () => {
    loadPosition();
};
(() => {
    let shelf = {};
    let contents = {};

    function linkCodeBlocks(elem, linkable = true) {
        codeworldKeywords = {};
        registerStandardHints(() => {
            let pres = elem.getElementsByTagName('pre');
            for (let i = 0; i < pres.length; ++i) {
                (() => {
                    let pre = pres[i];

                    // Markdeep buries the class annotations a bit, so we dig.
                    let clickable =
                        pre.classList.contains('clickable') ||
                        (pre.firstChild && pre.firstChild.classList &&
                            pre.firstChild.classList.contains(
                                'clickable')) ||
                        (pre.firstChild && pre.firstChild.firstChild &&
                            pre.firstChild.firstChild.classList &&
                            pre.firstChild.firstChild.classList
                            .contains('clickable'));

                    let text = pre.textContent;
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
                                env.loadSample(text);
                            }
                        }
                    }
                })();
            }
        });
    }

    function activateCollapsible(root) {
        let elems = root.getElementsByClassName('collapsible');
        for (let i = 0; i < elems.length; ++i) {
            let elem = elems[i];
            elem.onclick = () => {
                if (elem.classList.contains('expanded')) {
                    elem.classList.remove('expanded');
                } else {
                    elem.classList.add('expanded');
                }
            }
        }
    }

    function linkFunBlocks(elem) {
        let blocks = elem.getElementsByTagName('xml');
        let i = 0;

        while (blocks != null && blocks.length > 0) {
            let block = blocks[0];
            let text = block.outerHTML;

            let iframe = document.createElement('iframe');
            iframe.setAttribute('frameborder', '0');
            iframe.setAttribute('scrolling', 'no');

            iframe.addEventListener("load", () => {
                this.contentWindow.setParent(parent);
                this.contentWindow.setId(iframe);
                this.contentWindow.loadXml.call(iframe.contentWindow,
                    text);
            });

            iframe.src = 'blockframe.html';
            iframe.classList.add('clickable');

            let parent = block.parentNode;
            parent.insertBefore(iframe, block);
            parent.removeChild(block);

            blocks = elem.getElementsByTagName('xml');
            i++;
        }
    }

    function addTableOfContents(body, outline) {
        let contents = document.createElement('div');
        contents.id = 'helpcontents';
        contents.classList.add('contents');

        let elems = body.getElementsByTagName('*');

        let currentLevel = 0;
        let currentElem = contents;
        let n = 0;
        for (let i = 0; i < elems.length; ++i) {
            let header = elems[i];
            let match = (/h([1-3])/i).exec(header.tagName);
            if (match == null) continue;
            let level = parseInt(match[1]);

            while (currentLevel < level) {
                let sub = document.createElement('ul');
                currentElem.appendChild(sub);
                ++currentLevel;
                currentElem = sub;
            }

            while (currentLevel > level) {
                currentElem = currentElem.parentNode;
                --currentLevel;
            }

            ++n;

            let anchor = document.createElement('a');
            anchor.setAttribute('name', n);
            anchor.innerHTML = header.innerHTML;
            header.innerHTML = '';
            header.appendChild(anchor);

            let li = document.createElement('li');
            let link = document.createElement('a');
            link.setAttribute('href', '#' + n);
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
        let popdiv = document.createElement('div');
        popdiv.id = 'popout';
        popdiv.style = 'text-align: right';
        let popout = document.createElement('a');
        popout.innerHTML =
            '<i class="mdi mdi-18px mdi-open-in-new"></i>&nbsp;Open the Help in a New Tab';
        popout.target = '_blank';
        popout.href = document.location.href;
        popout.onclick = e => {
            let tab = open(this.href);
            tab.addEventListener("load", () => {
                tab.env = parent;
                if (parent.sweetAlert) {
                    parent.sweetAlert.close();
                }
                if (parent.sweetAlert2) {
                    parent.sweetAlert2.close();
                }
            });
            e.preventDefault();
        };
        popdiv.appendChild(popout);
        help.appendChild(popdiv);
    }

    function relativizeLinks(base, root, tag, attr) {
        let elems = root.getElementsByTagName(tag);
        for (let elem of elems) {
            if (elem.hasAttribute(attr)) {
                let url = new URL(elem.getAttribute(attr), base);
                if (base.origin == url.origin && base.pathname == url.pathname) {
                    url = url.hash;
                } else {
                    url = url.href;
                }
                elem.setAttribute(attr, url);
            }
        }
    }

    function removeCallStacks(node) {
        if (node.nodeType == Node.TEXT_NODE) {
            node.nodeValue = node.nodeValue.replace(/HasCallStack => /, '');
        } else {
            for (let child of node.childNodes) removeCallStacks(child);
        }
    }

    function setContent(elem) {
        let help = document.getElementById('help');
        while (help.firstChild) {
            help.removeChild(help.firstChild);
        }
        if (parent && parent !== window) {
            addPopout(help);
        }
        help.appendChild(elem)

        document.body.scrollTop = position.pageY;
        if (document.firstElementChild) document.firstElementChild.scrollTop =
            position.pageY;
    }

    function loadPath(path) {
        if (!path && shelf) path = shelf.default || shelf.named[0][1];
        position.path = path;
        if (contents[path] && contents[path].elem) {
            setContent(contents[path].elem);
        } else {
            let request = new XMLHttpRequest();
            request.open('GET', path, true);
            request.onreadystatechange = () => {
                if (request.readyState != 4) {
                    return;
                }

                let source = new URL(path, location.href);
                let content = document.createElement('div');
                let raw = request.responseText;

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

                let spacerDiv = document.createElement('div');
                spacerDiv.style = 'height: 90vh'
                content.appendChild(spacerDiv);

                if (!contents[path]) contents[path] = {};
                contents[path].elem = content;

                if (contents[path].outline) {
                    addTableOfContents(content, contents[path].outline);
                }
                setContent(content);
            }
            request.send(null);
        };
    }

    function loadSidebar() {
        let path = position.path;
        if (!path) path = shelf.default;

        let activeIndex = false;

        let acc = document.createElement('div')
        acc.id = 'helpacc';

        let paneNum = 0;
        for (let doc in shelf.named) {
            let hdr = document.createElement('h3');
            hdr.innerText = doc;
            acc.appendChild(hdr);

            let entry = document.createElement('div');
            entry.classList.add('accentry');
            entry.innerHTML = '<div>Loading...</div>';
            acc.appendChild(entry);

            contents[shelf.named[doc]] = {
                title: doc,
                header: hdr,
                outline: entry,
                index: paneNum,
                elem: null
            }

            if (shelf.named[doc] == path) activeIndex = paneNum;
            paneNum++;
        }

        document.body.insertBefore(acc, document.body.firstChild);
        $(acc).accordion({
            collapsible: true,
            active: activeIndex,
            heightStyle: 'content',
            beforeActivate: (event, ui) => {
                position.pageX = 0;
                position.pageY = 0;
                let name = ui.newHeader.text();
                let path = name && shelf.named[name];
                if (path) loadPath(path);
            }
        });
    }

    let markdeepStyle = document.createElement('style');
    document.head.appendChild(markdeepStyle);
    markdeepStyle.outerHTML = window.markdeep.stylesheet();

    if (params.get('shelf')) {
        let request = new XMLHttpRequest();
        request.open('GET', params.get('shelf'), true);
        request.onreadystatechange = () => {
            if (request.readyState != 4) {
                return;
            }

            shelf = JSON.parse(request.responseText);
            loadSidebar();
            loadPath(position.path);
        };
        request.send(null);
    } else {
        shelf = null;
        loadPath(params.get('path'));
    }
})();
