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

/*
 * Initializes the programming environment.  This is called after the
 * entire document body and other JavaScript has loaded.
 */
function init() {
    showingBrowse = true;
    showingDoc = false;
    showingResult = false;
    allProjectNames = [];

    if (window.location.pathname == '/haskell') {
        window.buildMode = 'haskell'
    } else {
        window.buildMode = 'codeworld';
    }

    var editor = document.getElementById('editor');

    codeworldKeywords = {};

    window.codeworldEditor = CodeMirror.fromTextArea(editor, {
        mode: {
            name: 'codeworld',
            overrideKeywords: codeworldKeywords
        },
        lineNumbers: true,
        autofocus: true,
        matchBrackets: true,
        styleActiveLine: true,
        showTrailingSpace: true,
        indentWithTabs: false,
        autoClearEmptyLines: true,
        rulers: [{
            column: 80,
            color: "#bbb",
            lineStyle: "dashed"
        }],
        extraKeys: {
            "Ctrl-Space": "autocomplete",
            "Shift-Space": "autocomplete",
            "Tab": "indentMore",
            "Shift-Tab": "indentLess",
            "Ctrl-Enter": compile
        }
    });

    CodeMirror.commands.save = function(cm) {
        saveProject();
    }
    document.onkeydown = function(e) {
        if (e.ctrlKey && e.keyCode === 83) {
            saveProject();
            return false;
        }
    };

    window.codeworldEditor.on('changes', window.updateUI);

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

    var hash = location.hash.slice(1);
    if (hash.length > 0) {
        if (hash.slice(-2) == '==') {
            hash = hash.slice(0, -2);
        }
        sendHttp('GET', 'loadSource?hash=' + hash, null, function(request) {
            if (request.status == 200) {
                setCode(request.responseText, null, null, true);
            }
        });
    } else {
        setCode('');
    }

    updateUI();

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

        setMode(true);

        hints.sort(function(a, b) {
            return a.source < b.source ? -1 : 1
        });
        CodeMirror.registerHelper('hintWords', 'codeworld', hints);
    });

    window.onbeforeunload = function(event) {
        if (!isEditorClean()) {
            var msg = 'There are unsaved changes to your project. ' + 'If you continue, they will be lost!';
            if (event) event.returnValue = msg;
            return msg;
        }
    }
}

function setMode(force) {
    if (window.buildMode == 'haskell') {
        if (force || window.codeworldEditor.getMode().name == 'codeworld') {
            window.codeworldEditor.setOption('mode', 'haskell');
        }
    } else {
        if (force || window.codeworldEditor.getMode().name != 'codeworld') {
            window.codeworldEditor.setOption(
                'mode', {
                    name: 'codeworld',
                    overrideKeywords: codeworldKeywords
                });
        }
    }
}

/*
 * Updates all UI components to reflect the current state.  The general pattern
 * is to modify the state stored in variables and such, and then call updateUI
 * to get the visual presentation to match.
 */
function updateUI() {
    var isSignedIn = signedIn();
    if (isSignedIn) {
        document.getElementById('signin').style.display = 'none';
        document.getElementById('signout').style.display = '';
        document.getElementById('saveAsButton').style.display = '';

        if (window.openProjectName) {
            document.getElementById('saveButton').style.display = '';
            document.getElementById('deleteButton').style.display = '';
        } else {
            document.getElementById('saveButton').style.display = 'none';
            document.getElementById('deleteButton').style.display = 'none';
        }
    } else {
        document.getElementById('signin').style.display = '';
        document.getElementById('signout').style.display = 'none';
        document.getElementById('saveButton').style.display = 'none';
        document.getElementById('saveAsButton').style.display = 'none';
        document.getElementById('deleteButton').style.display = 'none';
    }

    if (window.showingBrowse) {
        document.getElementById('nav').style.display = '';
    } else {
        document.getElementById('nav').style.display = 'none';
    }

    if (window.showingDoc) {
        document.getElementById('doc').style.display = '';
    } else {
        document.getElementById('doc').style.display = 'none';
    }

    if (window.showingResult) {
        document.getElementById('result').style.display = '';

        if (window.programRunning) {
            document.getElementById('shareButton').style.display = '';
        } else {
            document.getElementById('shareButton').style.display = 'none';
        }
    } else {
        document.getElementById('result').style.display = 'none';
        document.getElementById('shareButton').style.display = 'none';
    }

    var projects = document.getElementById('nav_mine');
    var newProject = document.getElementById('newButton');

    while (projects.lastChild && projects.lastChild != newProject) {
        projects.removeChild(projects.lastChild);
    }

    allProjectNames.sort(function(a, b) {
        return a.localeCompare(b);
    });

    allProjectNames.forEach(function(projectName) {
        var active = projectName == openProjectName;
        if (!isSignedIn && !active) {
            return;
        }

        var title = projectName;
        if (active && !isEditorClean()) {
            title = "* " + title;
        }

        var encodedName = title.replace('&', '&amp;')
            .replace('<', '&lt;')
            .replace('>', '&gt;');

        var template = document.getElementById('projectTemplate').innerHTML;
        template = template.replace('{{label}}', encodedName);
        template = template.replace(/{{ifactive ([^}]*)}}/, active ? "$1" : "");

        var span = document.createElement('span');
        span.innerHTML = template;
        var elem = span.getElementsByTagName('a')[0];
        elem.onclick = function() {
            loadProject(projectName);
        };

        projects.appendChild(span.removeChild(elem));
    });

    var title;
    if (window.openProjectName) {
        title = window.openProjectName;
    } else {
        title = "(new)";
    }

    if (!isEditorClean()) {
        title = "* " + title;
    }

    document.title = title + " - CodeWorld"
}

function toggleBrowser() {
    window.showingBrowse = !window.showingBrowse;
    updateUI();
}

function toggleDoc() {
    window.showingDoc = !window.showingDoc;
    updateUI();

    if (window.showingDoc) {
        stop();
        var loc = document.getElementById('doc').contentWindow.location;
        loc.search = 'help/' + window.buildMode + '.md';
    }
}

function discoverProjects() {
    if (!signedIn()) {
        allProjectNames = window.openProjectName ? [window.openProjectName] : [];
        updateUI();
        return;
    }

    var data = new FormData();
    data.append('id_token', auth2.currentUser.get().getAuthResponse().id_token);
    data.append('mode', window.buildMode);

    sendHttp('POST', 'listProjects', data, function(request) {
        if (request.status != 200) {
            return;
        }

        allProjectNames = JSON.parse(request.responseText);
        updateUI();
    });
}

function isEditorClean() {
    var doc = window.codeworldEditor.getDoc();

    if (window.savedGeneration == null) return doc.getValue() == '';
    else return doc.isClean(window.savedGeneration);
}

function setCode(code, history, name, autostart) {
    openProjectName = name;

    var doc = codeworldEditor.getDoc();
    doc.setValue(code);
    savedGeneration = doc.changeGeneration(true);

    if (history) {
        doc.setHistory(history);
    } else {
        doc.clearHistory();
    }

    codeworldEditor.focus();

    if (autostart) {
        compile();
    } else {
        stop();
    }
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

function loadSample(code) {
    warnIfUnsaved(function() {
        setCode(code);
    });
}

function newProject() {
    warnIfUnsaved(function() {
        setCode('');
    });
}

function loadProject(name) {
    if (!signedIn()) {
        sweetAlert('Oops!', 'You must sign in to open projects.', 'error');
        updateUI();
        return;
    }

    warnIfUnsaved(function() {
        var data = new FormData();
        data.append('id_token', auth2.currentUser.get().getAuthResponse().id_token);
        data.append('name', name);
        data.append('mode', window.buildMode);

        sendHttp('POST', 'loadProject', data, function(request) {
            if (request.status == 200) {
                var project = JSON.parse(request.responseText);
                setCode(project.source, project.history, name);
            }
        });
    });
}

function share() {
    var runner = document.getElementById('runner');
    if (runner.contentWindow.location.href == 'about:blank') {
        sweetAlert('Oops!', 'You must run your program before sharing it.', 'error');
    } else {
        var url = window.location.href;

        // Strip trailing equal-signs, since some social sites mangle them.
        url = url.replace(/=*$/, '');

        sweetAlert({
            html: true,
            title: '<i class="fa fa-2x fa-share"></i>&nbsp; Share',
            text: 'Copy and share this link with others!',
            type: 'input',
            inputValue: url,
            confirmButtonText: 'Done',
            animation: 'slide-from-bottom'
        });
    }
}

function stop() {
    run('', '', false);
}

function addToMessage(msg) {
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

function run(hash, msg, error) {
    window.showingResult = hash || msg;
    if (window.showingResult) {
        window.showingDoc = false;
    }

    if (hash) {
        window.location.hash = '#' + hash;
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

    updateUI();
}

function goto(line, col) {
    codeworldEditor.getDoc().setCursor(line - 1, col - 1);
    codeworldEditor.scrollIntoView(null, 100);
    codeworldEditor.focus();
}

function usingHaskellPrelude() {
    var src = window.codeworldEditor.getValue();
    return /HaskellPrelude/.test(src);
}

function compile() {
    run('', 'Building...', false);

    var src = window.codeworldEditor.getValue();
    var data = new FormData();
    data.append('source', src);
    data.append('mode', window.buildMode);

    sendHttp('POST', 'compile', data, function(request) {
        var hash = request.responseText;
        var success = request.status == 200;

        var data = new FormData();
        data.append('hash', hash);
        data.append('mode', window.buildMode);

        sendHttp('POST', 'runMsg', data, function(request) {
            var msg = '';
            if (request.status == 200) {
                msg = request.responseText;
            } else if (request.status == 404) {
                msg = "Sorry!  Your program couldn't be run right now.  Please try again.";
            }

            if (success) {
                run(hash, 'Running...\n\n' + msg, false);
            } else {
                run(hash, msg, true);
            }
        });
    });
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

function signinCallback(result) {
    discoverProjects();
    updateUI();
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

function saveProjectAs() {
    if (!signedIn()) {
        sweetAlert('Oops!', 'You must sign in to save files.', 'error');
        updateUI();
        return;
    }

    window.codeworldEditor.focus();
    var text = 'Save Project As: <input type="text" style="width: 10em"/>';

    var defaultName;
    if (window.openProjectName) {
        defaultName = window.openProjectName;
    } else {
        defaultName = '';
    }

    sweetAlert({
        html: true,
        title: '<i class="fa fa-2x fa-cloud-upload"></i>&nbsp; Save As',
        text: 'Enter a name for your project:',
        type: 'input',
        inputValue: defaultName,
        confirmButtonText: 'Save',
        showCancelButton: true,
        closeOnConfirm: false
    }, saveProjectBase);
}

function saveProjectBase(projectName) {
    if (projectName == null || projectName == '') return;

    if (!signedIn) {
        sweetAlert('Oops!', 'You must sign in to save files.', 'error');
        updateUI();
        return;
    }

    function go() {
        sweetAlert.close();
        var doc = window.codeworldEditor.getDoc();
        var project = {
            'name': projectName,
            'source': doc.getValue(),
            'history': doc.getHistory()
        };

        var data = new FormData();
        data.append('id_token', auth2.currentUser.get().getAuthResponse().id_token);
        data.append('project', JSON.stringify(project));
        data.append('mode', window.buildMode);

        sendHttp('POST', 'saveProject', data, function(request) {
            if (request.status != 200) {
                sweetAlert('Oops!', 'Could not save your project!!!  Please try again.', 'error');
                return;
            }

            window.openProjectName = projectName;
            window.savedGeneration = doc.changeGeneration(true);
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

function deleteProject() {
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
        data.append('mode', window.buildMode);

        sendHttp('POST', 'deleteProject', data, function(request) {
            if (request.status == 200) {
                savedGeneration = codeworldEditor.getDoc().changeGeneration(true);
                setCode('');
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

    sendHttp('GET', 'rts.js');
    sendHttp('GET', 'lib.base.js');
    sendHttp('GET', 'out.base.js');
}
