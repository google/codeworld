/*
 * Copyright 2016 The CodeWorld Authors. All rights reserved.
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

/*
 * Polyfill for String.startsWith, which isn't available in all browsers.
 */
if (!String.prototype.startsWith) {
    String.prototype.startsWith = function(searchString, position){
      position = position || 0;
      return this.substr(position, searchString.length) === searchString;
  };
}

/*
 * Utility function for sending an HTTP request to fetch a resource.
 *
 * Args:
 *   - method: The HTTP method to use, such as 'GET'
 *   - url: The URL to fetch, whether absolute or relative.
 *   - body: The request body to send.  Use null for no body.
 *   - callback: A callback function to send when complete.  (optional)
 *
 * If provided, the callback will be given the XmlHttpRequest object, so
 * it can inspect the response code and headers as well as the contents.
 */
function sendHttp(method, url, body, callback) {
    var request = new XMLHttpRequest();

    if (callback) {
        request.onreadystatechange = function() {
            if (request.readyState == 4) callback(request);
        };
    }

    request.open(method, url, true);
    request.send(body);
}

function loadXmlHash(hash)
{
   sendHttp('GET', 'loadXML?hash=' + hash + '&mode=blocklyXML', null, function(request) {
     if (request.status == 200) {
          var workspace = Blockly.mainWorkspace;
          Blockly.mainWorkspace.clear();
          var xmldom = Blockly.Xml.textToDom(request.responseText);
          Blockly.Xml.domToWorkspace(workspace, xmldom);
     }
    });
}

codeworldKeywords = {};
function init()
{
    var hash = location.hash.slice(1);
    if (hash.length > 0) {
        if (hash.slice(-2) == '==') {
            hash = hash.slice(0, -2);
        }
        loadXmlHash(hash);

    } 
    else {
      // Do nothing
    }
    
    // Add hint highlighting
    function createHint(line, wordStart, wordEnd) {
        var word = line.slice(wordStart, wordEnd);

        function renderer(elem, data, cur) {
            if (wordStart > 0) {
                elem.appendChild(document.createTextNode(line.slice(0, wordStart)));
            }

            var wordElem = document.createElement("span");
            wordElem.className = 'hint-word';
            wordElem.appendChild(document.createTextNode(word));
            elem.appendChild(wordElem);
            if (wordEnd < line.length) {
                elem.appendChild(document.createTextNode(line.slice(wordEnd)));
            }
        }
        return {
            text: word,
            render: renderer,
            source: line
        };
    }
    var hints = [
        createHint("main :: Program", 0, 4),
        createHint("--  single line comment", 0, 2),
        createHint("{-  start a multi-line comment", 0, 2),
        createHint("-}  end a multi-line comment", 0, 2),
        createHint("::  write a type annotation", 0, 2),
        createHint("->  declare a function type or case branch", 0, 2),
        createHint("<-  list comprehension index", 0, 2),
        createHint("..  list range", 0, 2),
        createHint("case  decide between many options", 0, 4),
        createHint("of  finish a case statement", 0, 2),
        createHint("if  decide between two choices", 0, 2),
        createHint("then  1st choice of an if statement", 0, 4),
        createHint("else  2nd choice of an if statement", 0, 4),
        createHint("data  define a new data type", 0, 4),
        createHint("let  define local variables", 0, 3),
        createHint("in  finish a let statement", 0, 2),
        createHint("where  define local variables", 0, 5),
        createHint("type  define a type synonym", 0, 4),
        createHint("(:) :: a -> [a] -> [a]", 1, 2)
    ];

    CodeMirror.registerHelper('hint', 'codeworld', function(cm) {
        var cur = cm.getCursor();
        var token = cm.getTokenAt(cur);
        var to = CodeMirror.Pos(cur.line, token.end);
        if (token.string && /\w/.test(token.string[token.string.length - 1])) {
            var term = token.string,
                from = CodeMirror.Pos(cur.line, token.start);
        } else {
            var term = "",
                from = to;
        }
        var found = [];
        for (var i = 0; i < hints.length; i++) {
            var hint = hints[i];
            if (hint.text.slice(0, term.length) == term)
                found.push(hint);
        }

        if (found.length) return {
            list: found,
            from: from,
            to: to
        };
    });

    sendHttp('GET', 'codeworld-base.txt', null, function(request) {
        var lines = [];
        if (request.status != 200) {
            console.log('Failed to load autocomplete word list.');
        } else {
            lines = request.responseText.split('\n');
        }

        var startLine = lines.indexOf('module Prelude') + 1;
        var endLine = startLine;
        while (endLine < lines.length) {
            if (lines[endLine].startsWith("module ")) {
                break;
            }
            endLine++;
        }
        lines = lines.slice(startLine, endLine);

        // Special case for main, since it's morally a built-in name.
        codeworldKeywords['main'] = 'builtin';

        lines = lines.sort().filter(function(item, pos, array) {
            return !pos || item != array[pos - 1];
        });

        var hintBlacklist = [
            // Symbols that only exist to implement RebindableSyntax or map to
            // built-in Haskell types.
            "Bool",
            "IO",
            "fromDouble",
            "fromInt",
            "fromInteger",
            "fromRational",
            "fromString",
            "ifThenElse",
            "line",
            "negate",
            "pictureOf",
            "thickLine",
            "toDouble",
            "toInt",
        ];

        lines.forEach(function(line) {
            if (line.startsWith("type Program")) {
                // We must intervene to hide the IO type.
                line = "data Program";
            } else if (line.startsWith("type Truth")) {
                line = "data Truth";
            } else if (line.startsWith("True ::")) {
                line = "True :: Truth";
            } else if (line.startsWith("False ::")) {
                line = "False :: Truth";
            } else if (line.startsWith("newtype ")) {
                // Hide the distinction between newtype and data.
                line = "data " + line.substr(8);
            } else if (line.startsWith("class ")) {
                return;
            } else if (line.startsWith("instance ")) {
                return;
            } else if (line.startsWith("-- ")) {
                return;
            }

            // Filter out strictness annotations.
            line = line.replace(/(\s)!([A-Za-z\(\[])/g, '$1$2');

            var wordStart = 0;
            if (line.startsWith("type ") || line.startsWith("data")) {
                wordStart += 5;

                // Hide kind annotations.
                var kindIndex = line.indexOf(" ::");
                if (kindIndex != -1) {
                    line = line.substr(0, kindIndex);
                }
            }

            var wordEnd = line.indexOf(" ", wordStart);
            if (wordEnd == -1) {
                wordEnd = line.length;
            }
            if (wordStart == wordEnd) {
                return;
            }

            if (line[wordStart] == "(" && line[wordEnd - 1] == ")") {
                wordStart++;
                wordEnd--;
            }

            var word = line.substr(wordStart, wordEnd - wordStart);

            if (hintBlacklist.indexOf(word) >= 0) {
                codeworldKeywords[word] = 'deprecated';
            } else if (/^[A-Z:]/.test(word)) {
                codeworldKeywords[word] = 'builtin-2';
                hints.push(createHint(line, wordStart, wordEnd));
            } else {
                codeworldKeywords[word] = 'builtin';
                hints.push(createHint(line, wordStart, wordEnd));
            }

        });

        hints.sort(function(a, b) {
            return a.source < b.source ? -1 : 1
        });
        CodeMirror.registerHelper('hintWords', 'codeworld', hints);
    });




}

function addToMessage(msg) {
    var message = document.getElementById('message');
    message.innerHTML += msg
}

function updateEditor(code) {
    var editor = document.getElementById('genCode');
    CodeMirror.runMode(code
      ,{
        name: 'codeworld',
        overrideKeywords: codeworldKeywords
      }
      ,editor);
}

function run(xmlHash, codeHash, msg, error) {

    window.showingResult = xmlHash || msg;

    var hash = codeHash 

    if (window.showingResult) {
        window.showingDoc = false;
    }

    if (hash) {
        window.location.hash = '#' + xmlHash;
    } else {
        window.location.hash = '';
    }

    var runner = document.getElementById('runner');
    if (hash && !error) {
        var loc = 'run.html?hash=' + hash + '&mode=' + window.buildMode;
        runner.contentWindow.location.replace(loc);
        document.getElementById('runner').style.display = '';
        document.getElementById('runner').contentWindow.focus();
        window.programRunning = true;
    } else {
        runner.contentWindow.location.replace('about:blank');
        document.getElementById('runner').style.display = 'none';
        window.programRunning = false;
    }

    var message = document.getElementById('message');
    message.innerHTML = '';
    addToMessage(msg);

    if (error) {
        message.classList.add('error');
    } else {
        message.classList.remove('error');
    }

}

function removeErrors()
{
    $('.blocklyDraggable').removeClass('blocklyErrorSelected');
    var blocks = Blockly.getMainWorkspace().getAllBlocks();

    blocks.forEach(function(block){
      block.removeErrorSelect();
    });
}

function compile(src) {
    run('', '', 'Building...', false);

    var workspace = Blockly.getMainWorkspace();
    var xml = Blockly.Xml.workspaceToDom(workspace);
    var xml_text = Blockly.Xml.domToText(xml);

    var data = new FormData();
    data.append('source', xml_text);
    data.append('mode', 'blocklyXML');

    sendHttp('POST', 'saveXMLhash', data, function(request) {

        // XML Hash
        var xmlHash = request.responseText;

        var data = new FormData();
        data.append('source', src);
        data.append('mode', window.buildMode);

        sendHttp('POST', 'compile', data, function(request) {
            var success = request.status == 200;

            // Code hash
            var codeHash = request.responseText;

            var data = new FormData();
            data.append('hash', codeHash);
            data.append('mode', window.buildMode);

            sendHttp('POST', 'runMsg', data, function(request) {
                var msg = '';
                if (request.status == 200) {
                    msg = request.responseText;
                } else if (request.status == 404) {
                    msg = "Sorry!  Your program couldn't be run right now.  Please try again.";
                }

                if (success) {
                    run(xmlHash, codeHash, 'Running...\n\n' + msg, false);
                } else {
                    run(xmlHash, codeHash, msg, true);
                }
            });
        });
    });
}


