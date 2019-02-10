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
(function() {
    var params = new URLSearchParams(window.location.search);

    var shelf = {};
    var contents = {};

    function linkCodeBlocks(elem, linkable = true) {
        codeworldKeywords = {};
        registerStandardHints(function() {
            var pres = elem.getElementsByTagName('pre');
            for (var i = 0; i < pres.length; ++i) {
                (function() {
                    var pre = pres[i];

                    // Markdeep buries the class annotations a bit, so we dig.
                    var clickable =
                        pre.classList.contains('clickable') ||
                        (pre.firstChild && pre.firstChild.classList &&
                            pre.firstChild.classList.contains('clickable')) ||
                        (pre.firstChild && pre.firstChild.firstChild && pre.firstChild.firstChild.classList &&
                            pre.firstChild.firstChild.classList.contains('clickable'));

                    var text = pre.textContent;
                    pre.innerHTML = '';
                    CodeMirror.runMode(text, {
                        name: 'codeworld',
                        overrideKeywords: codeworldKeywords
                    }, pre);
                    pre.classList.add('cm-s-default');

                    if (linkable && clickable) {
                        pre.classList.add('clickable');
                        pre.onclick = function() {
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
        var elems = root.getElementsByClassName('collapsible');
        for (var i = 0; i < elems.length; ++i) {
            let elem = elems[i];
            elem.onclick = function() {
                if (elem.classList.contains('expanded')) {
                    elem.classList.remove('expanded');
                } else {
                    elem.classList.add('expanded');
                }
            }
        }
    }

    function linkFunBlocks(elem) {
        var blocks = elem.getElementsByTagName('xml');
        var i = 0;

        while (blocks != null && blocks.length > 0) {
            let block = blocks[0];
            let text = block.outerHTML;

            var iframe = document.createElement('iframe');
            iframe.setAttribute('frameborder', '0');
            iframe.setAttribute('scrolling', 'no');

            iframe.addEventListener("load", function() {
                this.contentWindow.setParent(parent);
                this.contentWindow.setId(iframe);
                this.contentWindow.loadXml.call(iframe.contentWindow, text);
            });

            iframe.src = 'blockframe.html';
            iframe.classList.add('clickable');

            var parent = block.parentNode;
            parent.insertBefore(iframe, block);
            parent.removeChild(block);

            blocks = elem.getElementsByTagName('xml');
            i++;
        }
    }

    function addTableOfContents(body, outline) {
        var contents = document.createElement('div');
        contents.id = 'helpcontents';
        contents.classList.add('contents');

        var elems = body.getElementsByTagName('*');

        var currentLevel = 0;
        var currentElem = contents;
        var n = 0;
        for (var i = 0; i < elems.length; ++i) {
            var header = elems[i];
            var match = (/h([1-3])/i).exec(header.tagName);
            if (match == null) continue;
            var level = parseInt(match[1]);

            while (currentLevel < level) {
                var sub = document.createElement('ul');
                currentElem.appendChild(sub);
                ++currentLevel;
                currentElem = sub;
            }

            while (currentLevel > level) {
                currentElem = currentElem.parentNode;
                --currentLevel;
            }

            ++n;

            var anchor = document.createElement('a');
            anchor.setAttribute('name', n);
            anchor.innerHTML = header.innerHTML;
            header.innerHTML = '';
            header.appendChild(anchor);

            var li = document.createElement('li');
            var link = document.createElement('a');
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
        var popdiv = document.createElement('div');
        popdiv.id = 'popout';
        popdiv.style = 'text-align: right';
        var popout = document.createElement('a');
        popout.innerHTML = '<i class="mdi mdi-18px mdi-open-in-new"></i>&nbsp;Open the Help in a New Tab';
        popout.target = '_blank';
        popout.href = document.location.href;
        popout.onclick = function(e) {
            var tab = open(this.href);
            tab.addEventListener("load", function() {
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
        var elems = root.getElementsByTagName(tag);
        for (let elem of elems) {
            if (elem.hasAttribute(attr)) {
                var url = new URL(elem.getAttribute(attr), base);
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
        var help = document.getElementById('help');
        while (help.firstChild) {
            help.removeChild(help.firstChild);
        }
        if (parent && parent !== window) {
            addPopout(help);
        }
        help.appendChild(elem)
        document.body.scrollTop = 0;
        if (document.firstElementChild) document.firstElementChild.scrollTop = 0;
    }

    function loadPath(path) {
        if (!path && shelf) path = shelf.default || shelf.named[0][1];

        if (contents[path] && contents[path].elem) {
            setContent(contents[path].elem);
        } else {
            var request = new XMLHttpRequest();
            request.open('GET', path, true);
            request.onreadystatechange = function() {
                if (request.readyState != 4) {
                    return;
                }

                var source = new URL(path, location.href);
                var content = document.createElement('div');
                var raw = request.responseText;

                if (path.endsWith('.md')) {
                    content.innerHTML = window.markdeep.format(raw, false);
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

                var spacerDiv = document.createElement('div');
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
        var path = params.get('path');
        if (!path) path = shelf.default;

        var activeIndex = false;

        var acc = document.createElement('div')
        acc.id = 'helpacc';

        var paneNum = 0;
        for (var doc in shelf.named) {
            var hdr = document.createElement('h3');
            hdr.innerText = doc;
            acc.appendChild(hdr);

            var entry = document.createElement('div');
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
            beforeActivate: function(event, ui) {
                var name = ui.newHeader.text();
                var path = name && shelf.named[name];
                if (path) loadPath(path);
            }
        });
    }

    var markdeepStyle = document.createElement('style');
    document.head.appendChild(markdeepStyle);
    markdeepStyle.outerHTML = window.markdeep.stylesheet();

    if (params.get('shelf')) {
        var request = new XMLHttpRequest();
        request.open('GET', params.get('shelf'), true);
        request.onreadystatechange = function() {
            if (request.readyState != 4) {
                return;
            }

            shelf = JSON.parse(request.responseText);
            loadSidebar();
            loadPath(params.get('path'));
        };
        request.send(null);
    } else {
        shelf = null;
        loadPath(params.get('path'));
    }
})();