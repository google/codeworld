/*
 * Copyright 2018 The CodeWorld Authors. All rights reserved.
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

    function linkCodeBlocks(elem, linkable=true) {
        codeworldKeywords = {};
        registerStandardHints(function() {
            var pres = elem.getElementsByTagName('pre');
            for (var i = 0; i < pres.length; ++i) {
                (function() {
                    var pre = pres[i];
                    var text = pre.textContent;
                    pre.innerHTML = '';
                    CodeMirror.runMode(text, { name: 'codeworld', overrideKeywords: codeworldKeywords }, pre);
                    pre.classList.add('cm-s-default');

                    if (linkable) {
                        if (text.indexOf("main ") != -1 || text.indexOf("program ") != -1) {
                            pre.classList.add('clickable');
                            pre.onclick = function() {
                                if (env && env.loadSample) {
                                    env.loadSample(text);
                                }
                            }
                        }
                    }
                })();
            }
        });
    }

    function linkFunBlocks(elem) {
        var pres = elem.getElementsByTagName('xml');
        var i = 0;

        while (pres != null && pres.length > 0) {
            let pre = pres[0];
            let text = pre.outerHTML;
            pre.outerHTML = '<iframe frameborder="0" scrolling="no" id="frame' + i + '"></iframe>';

            let myIframe = elem.getElementById('frame' + i);
            myIframe.addEventListener("load", function() {
                this.contentWindow.setParent(parent);
                this.contentWindow.setId(myIframe);
                this.contentWindow.loadXml.call(myIframe.contentWindow,text);
            });

            myIframe.src = 'blockframe.html';
            myIframe.classList.add('clickable');

            pres = elem.getElementsByTagName('xml');
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
        popdiv.style = 'text-align: right';
        var popout = document.createElement('a');
        popout.innerHTML = '<i class="mdi mdi-18px mdi-open-in-new"></i>&nbsp;Open the Help in a New Tab';
        popout.target = '_blank';
        popout.href = document.location.href;
        popout.onclick = function (e) {
            var tab = open(this.href);
            tab.addEventListener("load", function () {
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

    function loadPath(path) {
        var help = document.getElementById('help');
        while (help.firstChild) {
            help.removeChild(help.firstChild);
        }

        if (!path) path = shelf.default;

        if (contents[path] && contents[path].elem) {
            if (parent && parent !== window) {
                addPopout(help);
            }
            help.appendChild(contents[path].elem)
        } else {
            var request = new XMLHttpRequest();
            request.open('GET', path, true);
            request.onreadystatechange = function() {
                if (request.readyState != 4) {
                    return;
                }

                var content = document.createElement('div');
                var text = request.responseText;
                var converter = new Remarkable({ html: true });
                var html = converter.render(text);
                content.innerHTML = html;

                if (shelf.blocks) {
                    linkFunBlocks(content);
                    linkCodeBlocks(content, false);
                } else {
                    linkCodeBlocks(content);
                }

                if (!contents[path]) contents[path] = {};
                contents[path].elem = content;

                if (contents[path].outline) {
                    addTableOfContents(content, contents[path].outline);
                }

                if (parent && parent !== window) {
                    addPopout(help);
                }
                help.appendChild(content);
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
})();
