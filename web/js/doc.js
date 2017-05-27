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

window.env = parent;
(function() {
    function linkCodeBlocks(linkable=true) {
        codeworldKeywords = {};
        registerStandardHints( function(){
        var pres = document.getElementsByTagName('pre');
            for (var i = 0; i < pres.length; ++i) {
                (function() {
                    var pre = pres[i];
                    var text = pre.textContent;
                    pre.innerHTML = '';
                    CodeMirror.runMode(text, { name: 'codeworld', overrideKeywords: codeworldKeywords }, pre);
                    pre.classList.add('cm-s-default');

                    if (text.indexOf("main ") != -1 && linkable) {
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

    function linkFunBlocks() {
        var pres = document.getElementsByTagName('xml');
        var i = 0;

        while(pres != null && pres.length > 0){
          (function() {
          var pre = pres[0];
          
          var text = pre.outerHTML;
          
          pre.outerHTML = '<iframe frameborder="0" scrolling="no" id="frame' + i + '"></iframe>';

          var myIframe = document.getElementById('frame' + i);
          var thisDocument = document;
          myIframe.addEventListener("load", function() {
              this.contentWindow.setParent(parent);
              this.contentWindow.setId(myIframe);
              this.contentWindow.loadXml.call(myIframe.contentWindow,text);
          });

          myIframe.src = 'blockframe.html';
          myIframe.classList.add('clickable');


          pres = document.getElementsByTagName('xml');
          i++;
          })();
        }
    }

    function addTableOfContents() {
        var contents = document.createElement('div');
        contents.id = 'helpcontents';
        contents.classList.add('contents');

        var elems = document.getElementsByTagName('*');

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
        if (contents.childNodes.length > 0) {
            document.body.insertBefore(contents, document.body.firstChild);
        }
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
      }
      popdiv.appendChild(popout);
      help.appendChild(popdiv);
    }

    var path = window.location.search.slice(1);
    var request = new XMLHttpRequest();
    request.open('GET', path, true);
    request.onreadystatechange = function() {
        if (request.readyState == 4) {
          var help = document.getElementById('help');

          if (parent && parent !== window) {
            addPopout(help);
          }

          var content = document.createElement('div');
          var text = request.responseText;
          var converter = new Markdown.Converter();
          var html = converter.makeHtml(text);
          content.innerHTML = html;
          help.appendChild(content);

          if(path.includes('blocks')){
            linkFunBlocks();
            linkCodeBlocks(false);
          }
          else{
            linkCodeBlocks();
          }

          addTableOfContents();
        }
    };
    request.send(null);
})();
