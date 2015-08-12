/*
 * Copyright 2015 Google Inc. All rights reserved.
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

(function() {
  function linkCodeBlocks() {
    var pres = document.getElementsByTagName('pre');
    for (var i = 0; i < pres.length; ++i) {
      (function() {
        var pre = pres[i];
        var text = pre.textContent;
        pre.innerHTML = '';
        CodeMirror.runMode(text, 'codeworld', pre);
        pre.classList.add('cm-s-default');

        if (text.indexOf('main ') != -1) {
          pre.classList.add('clickable');
          pre.onclick = function() {
            if (parent && parent.setCode) {
              parent.setCode(text);
            }
          }
        }
      })();
    }
  }

  function addTableOfContents() {
    var contents = document.createElement('div');
    contents.classList.add('contents');

    var elems = document.getElementsByTagName('*');

    var currentLevel = 0;
    var currentElem = contents;
    var n = 0;
    for (var i = 0; i < elems.length; ++i) {
      var header = elems[i];
      var match = (/h([1-6])/i).exec(header.tagName);
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
      anchor.setAttribute('href', '#contents-' + n);
      anchor.innerHTML = header.innerHTML;
      header.innerHTML = '';
      header.appendChild(anchor);

      var li = document.createElement('li');
      var link = document.createElement('a');
      link.setAttribute('name', 'contents-' + n);
      link.setAttribute('href', '#' + n);
      link.textContent = header.textContent;
      li.appendChild(link);
      currentElem.appendChild(li);
    }
    if (contents.childNodes.length > 0) {
      document.body.insertBefore(contents, document.body.firstChild);
    }
  }
  var path = window.location.search.slice(1);
  var request = new XMLHttpRequest();
  request.open('GET', path, true);
  request.onreadystatechange = function() {
    if (request.readyState == 4) {
      var text = request.responseText;
      var converter = new Markdown.Converter();
      var html = converter.makeHtml(text);
      document.body.innerHTML = html;

      linkCodeBlocks();
      addTableOfContents();
    }
  };
  request.send(null);
})();
