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

function registerStandardHints(successFunc)
{
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

    // Add hint highlighting
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
    successFunc();
  });
}


function addToMessage(msg) {
    // usingHaskellPrelude must be defined
    if (window.buildMode == 'codeworld' && !usingHaskellPrelude()) {
        msg = msg
            .replace(/\u2018/g, '')
            .replace(/\u2019/g, '')
            .replace(/IO action main/g, 'variable main')
            .replace(/module Main/g, 'the program')
            .replace(/\[GHC\.Types\.Char\] -> /g, '')
            .replace(/base(-[0-9.]*)?\:(.|\n)*?->( |\n)*/g, '')
            .replace(/integer-gmp(-[0-9\.]*)?:(.|\n)*?->( |\n)*/g, '')
            .replace(/GHC\.[A-Za-z.]*(\s|\n)*->( |\n)*/g, '')
            .replace(/Main\./g, '')
            .replace(/main :: t/g, 'main :: Program')
            .replace(/Prelude\./g, '')
            .replace(/\bBool\b/g, 'Truth')
            .replace(/IO \(\)/g, 'Program')
            .replace(/IO [a-z][a-zA-Z0-9_]*/g, 'Program')
            .replace(/[ ]*Perhaps you intended to use TemplateHaskell\n/, '')
            .replace(/imported from [^)\n]*/g, 'defined in the standard library')
            .replace(/\(and originally defined in [^)]*\)/g, '')
            .replace(/the first argument/g, 'the parameter(s)')
            .replace(/[ ]*The function [a-zA-Z0-9_]* is applied to [a-z0-9]* arguments,\n/, '')
            .replace(/[ ]*but its type .* has only .*\n/, '')
            .replace(/A data constructor of that name is in scope; did you mean DataKinds\?/,
                'That name refers to a value, not a type.')
            .replace(/type constructor or class/g, 'type constructor')
            .replace(/Illegal tuple section: use TupleSections/,
                'This tuple is missing a value, or has an extra comma.')
            .replace(/in string\/character literal/, 'in text literal')
            .replace(/lexical error at character '\\822[01]'/,
                     'Smart quotes are not allowed.');
    }

    msg = msg
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;')
        .replace(/(user\/)?([PQ]..\/)?[PQ][A-Za-z0-9_=\-]*\.hs:(\d+):((\d+)(-\d+)?)/g,
            '<a href="#" onclick="goto($3, $5);">Line $3, Column $4</a>')
        .replace(/(user\/)?([PQ]..\/)?[PQ][A-Za-z0-9_=\-]*\.hs:\((\d+),(\d+)\)-\((\d+),(\d+)\)/g,
            '<a href="#" onclick="goto($3, $4);">Line $3-$5, Column $4-$6</a>');

    var message = document.getElementById('message');
    message.innerHTML += msg
}

function signin() {
    if (window.auth2) auth2.signIn();
}

function signout() {
    if (window.auth2) auth2.signOut();
}

function signedIn() {
    return window.auth2 && auth2.isSignedIn.get();
}

//signinCallback must be defined
function handleGAPILoad() {
    gapi.load('auth2', function() {
        withClientId(function(clientId) {
            window.auth2 = gapi.auth2.init({
                client_id: clientId,
                scope: 'profile',
                fetch_basic_profile: false
            });

            auth2.isSignedIn.listen(signinCallback);
            auth2.currentUser.listen(signinCallback);

            if (auth2.isSignedIn.get() == true) auth2.signIn();
        });
    });

    discoverProjects();
    updateUI();
}

function withClientId(f) {
    if (window.clientId) return f(window.clientId);

    sendHttp('GET', 'clientId.txt', null, function(request) {
        if (request.status != 200 || request.responseText == '') {
            sweetAlert('Oops!', 'Missing API client key.  You will not be able to sign in.', 'warning');
            return null;
        }

        window.clientId = request.responseText.trim();
        return f(window.clientId);
    });
}

function discoverProjects_(buildMode) {
    if (!signedIn()) {
        allProjectNames = window.openProjectName ? [window.openProjectName] : [];
        updateUI();
        return;
    }

    var data = new FormData();
    data.append('id_token', auth2.currentUser.get().getAuthResponse().id_token);
    data.append('mode', buildMode);

    sendHttp('POST', 'listProjects', data, function(request) {
        if (request.status != 200) {
            return;
        }

        allProjectNames = JSON.parse(request.responseText);
        updateUI();
    });
}

function warnIfUnsaved(action) {
    if (isEditorClean()) {
        action();
    } else {
        var msg = 'There are unsaved changes to your project. ' + 'Continue and throw away your changes?';
        sweetAlert({
            title: 'Warning',
            text: msg,
            type: 'warning',
            showCancelButton: true,
            confirmButtonColor: '#DD6B55',
            confirmButtonText: 'Yes, discard my changes!'
        }, action);
    }
}

function saveProjectAs() {
    if (!signedIn()) {
        sweetAlert('Oops!', 'You must sign in to save files.', 'error');
        updateUI();
        return;
    }

    // window.codeworldEditor.focus();
    var text = 'Save Project As: <input type="text" style="width: 10em"/>';

    var defaultName;
    if (window.openProjectName) {
        defaultName = window.openProjectName;
    } else {
        defaultName = '';
    }

    sweetAlert({
        html: true,
        title: '<i class="mdi mdi-72px mdi-cloud-upload"></i>&nbsp; Save As',
        text: 'Enter a name for your project:',
        type: 'input',
        inputValue: defaultName,
        confirmButtonText: 'Save',
        showCancelButton: true,
        closeOnConfirm: false
    }, saveProjectBase);
}

function saveProject() {
    if (!signedIn()) {
        sweetAlert('Oops!', 'You must sign in to save files.', 'error');
        updateUI();
        return;
    }

    if (window.openProjectName) {
        saveProjectBase(openProjectName);
    } else {
        saveProjectAs();
    }
}

function saveProjectBase_(projectName, mode, successFunc) {
    if (projectName == null || projectName == '') return;

    if (!signedIn) {
        sweetAlert('Oops!', 'You must sign in to save files.', 'error');
        updateUI();
        return;
    }

    function go() {
        sweetAlert.close();
        var project = getCurrentProject();
        project['name'] = projectName;

        var data = new FormData();
        data.append('id_token', auth2.currentUser.get().getAuthResponse().id_token);
        data.append('project', JSON.stringify(project));
        data.append('mode', mode);

        sendHttp('POST', 'saveProject', data, function(request) {
            if (request.status != 200) {
                sweetAlert('Oops!', 'Could not save your project!!!  Please try again.', 'error');
                return;
            }

            successFunc();

            updateUI();

            if (allProjectNames.indexOf(projectName) == -1) {
                discoverProjects();
            }
        });
    }

    if (allProjectNames.indexOf(projectName) == -1 || projectName == openProjectName) {
        go();
    } else {
        var msg = 'Are you sure you want to save over another project?\n\n' +
            'The previous contents of ' + projectName + ' will be permanently destroyed!';
        sweetAlert({
            title: 'Warning',
            text: msg,
            type: 'warning',
            showCancelButton: true,
            confirmButtonColor: '#DD6B55',
            confirmButtonText: 'Yes, overwrite it!'
        }, go);
    }
}

function deleteProject_(buildMode, successFunc) {
    if (!window.openProjectName) return;

    if (!signedIn) {
        sweetAlert('Oops', 'You must sign in to delete a project.', 'error');
        updateUI();
        return;
    }

    function go() {
        var data = new FormData();
        data.append('id_token', auth2.currentUser.get().getAuthResponse().id_token);
        data.append('name', window.openProjectName);
        data.append('mode', buildMode);

        sendHttp('POST', 'deleteProject', data, function(request) {
            if (request.status == 200) {
                successFunc();
            }

            discoverProjects();
            updateUI();
        });
    }

    var msg = 'Deleting a project will throw away all work, and cannot be undone. ' + 'Are you sure?';
    sweetAlert({
        title: 'Warning',
        text: msg,
        type: 'warning',
        showCancelButton: true,
        confirmButtonColor: '#DD6B55',
        confirmButtonText: 'Yes, delete it!'
    }, go);
}

function loadProject_(name, buildMode, successFunc) {
    
  warnIfUnsaved(function(){
    if (!signedIn()) {
        sweetAlert('Oops!', 'You must sign in to open projects.', 'error');
        updateUI();
        return;
    }

    var data = new FormData();
    data.append('id_token', auth2.currentUser.get().getAuthResponse().id_token);
    data.append('name', name);
    data.append('mode', buildMode);

    sendHttp('POST', 'loadProject', data, function(request) {
        if (request.status == 200) {
            var project = JSON.parse(request.responseText);

            successFunc(project);
        }
    });
  });
}


