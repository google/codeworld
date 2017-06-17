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
    function createHint(line, wordStart, wordEnd, cname) {
        var word = line.slice(wordStart, wordEnd);
        if (!cname) cname = 'hint-word';

        function renderer(elem, data, cur) {
            if (wordStart > 0) {
                elem.appendChild(document.createTextNode(line.slice(0, wordStart)));
            }
            var wordElem = document.createElement("span");
            wordElem.className = cname;
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
        createHint("--  single line comment", 0, 2, 'hint-keyword'),
        createHint("{-  start a multi-line comment", 0, 2, 'hint-keyword'),
        createHint("-}  end a multi-line comment", 0, 2, 'hint-keyword'),
        createHint("::  write a type annotation", 0, 2, 'hint-keyword'),
        createHint("->  declare a function type or case branch", 0, 2, 'hint-keyword'),
        createHint("<-  list comprehension index", 0, 2, 'hint-keyword'),
        createHint("..  list range", 0, 2, 'hint-keyword'),
        createHint("case  decide between many options", 0, 4, 'hint-keyword'),
        createHint("of  finish a case statement", 0, 2, 'hint-keyword'),
        createHint("if  decide between two choices", 0, 2, 'hint-keyword'),
        createHint("then  1st choice of an if statement", 0, 4, 'hint-keyword'),
        createHint("else  2nd choice of an if statement", 0, 4, 'hint-keyword'),
        createHint("data  define a new data type", 0, 4, 'hint-keyword'),
        createHint("let  define local variables", 0, 3, 'hint-keyword'),
        createHint("in  finish a let statement", 0, 2, 'hint-keyword'),
        createHint("where  define local variables", 0, 5, 'hint-keyword'),
        createHint("type  define a type synonym", 0, 4, 'hint-keyword'),
        createHint("(:) :: a -> [a] -> [a]", 1, 2)
    ];

    CodeMirror.registerHelper('hint', 'codeworld', function(cm) {
        var cur = cm.getCursor();
        var token = cm.getTokenAt(cur);
        var to = CodeMirror.Pos(cur.line, token.end);

	//To check for the case of insertion in between two parameters
        r = new RegExp("^\\s+$");
	// If string is completely made of spaces
        if (r.test(token.string)) {
            token.string = token.string.substr(0, cur.ch - token.start);
            token.end = cur.ch;
            to = CodeMirror.Pos(cur.line, token.end);
        }

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
        "fail",
        "fromCWText",
        "fromDouble",
        "fromInt",
        "fromInteger",
        "fromRational",
        "fromString",
        "ifThenElse",
        "line",
        "negate",
        "pictureOf",
        "randomsFrom",
        "thickLine",
        "toCWText",
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
        } else if (line.startsWith("infix ")) {
            return;
        } else if (line.startsWith("infixl ")) {
            return;
        } else if (line.startsWith("infixr ")) {
            return;
        }

        // Filter out strictness annotations.
        line = line.replace(/(\s)!([A-Za-z\(\[])/g, '$1$2');

        // Filter out CallStack constraints.
        line = line.replace(/:: HasCallStack =>/g, '::');

        var wordStart = 0;
        if (line.startsWith("type ") || line.startsWith("data ")) {
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
    if (window.buildMode == 'codeworld') {
        msg = msg
            .replace(/\u2018/g, '')
            .replace(/\u2019/g, '')
            .replace(/ \[-W[a-z-]*\]/g, '')
            .replace(/IO action main/g, 'variable main')
            .replace(/module Main/g, 'your program')
            .replace(/main\:Main/g, 'your program')
            .replace(/Couldn't match expected type Text\s*with actual type GHC.Types.Char/g,
                     'Text requires double quotes, rather than single.')
            .replace(/base-[0-9.]*:GHC\.Stack\.Types\.HasCallStack => /g, '')
            .replace(/When checking that:\s*[^\n]*\s*is more polymorphic than:\s*[^\n]*(\n\s*)?/g, '')
            .replace(/\[GHC\.Types\.Char\] -> /g, '')
            .replace(/base(-[0-9.]*)?\:(.|\n)*?->( |\n)*/g, '')
            .replace(/integer-gmp(-[0-9\.]*)?:(.|\n)*?->( |\n)*/g, '')
            .replace(/GHC\.[A-Za-z.]*(\s|\n)*->( |\n)*/g, '')
            .replace(/at src\/[A-Za-z0-9/.:]* /g, '')
            .replace(/GHC\.Types\.Char/g, '')
            .replace(/codeworld-base[-.:_A-Za-z0-9]*/g, 'the standard library')
            .replace(/Main\./g, '')
            .replace(/main :: t/g, 'main :: Program')
            .replace(/Prelude\./g, '')
            .replace(/\bBool\b/g, 'Truth')
            .replace(/IO \(\)/g, 'Program')
            .replace(/IO [a-z][a-zA-Z0-9_]*/g, 'Program')
            .replace(/[ ]*Perhaps you intended to use TemplateHaskell\n/g, '')
            .replace(/imported from [^)\n]*/g, 'defined in the standard library')
            .replace(/\(and originally defined in [^)]*\)/g, '')
            .replace(/the first argument/g, 'the parameter(s)')
            .replace(/[ ]*The function [a-zA-Z0-9_]* is applied to [a-z0-9]* arguments,\n/g, '')
            .replace(/[ ]*but its type .* has only .*\n/g, '')
            .replace(/A data constructor of that name is in scope; did you mean DataKinds\?/g,
                'That name refers to a value, not a type.')
            .replace(/type constructor or class/g, 'type')
            .replace(/Illegal tuple section: use TupleSections/g,
                'This tuple is missing a value, or has an extra comma.')
            .replace(/in string\/character literal/g, 'in text literal')
            .replace(/lexical error at character '\\822[01]'/g,
                     'Smart quotes are not allowed.')
            .replace(/Use -v to see a list of the files searched for\./g, '')
            .replace(/CallStack \(from HasCallStack\)\:/g, 'When evaluating:')
            .replace(/\n\s+\n/g, '\n');
    }

    msg = msg
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;')
        .replace(/program\.hs:(\d+):((\d+)(-\d+)?)/g,
            '<a href="#" onclick="goto($1, $3);">Line $1, Column $2</a>')
        .replace(/program\.hs:\((\d+),(\d+)\)-\((\d+),(\d+)\)/g,
            '<a href="#" onclick="goto($1, $2);">Line $1-$3, Column $2-$4</a>');

    var message = document.getElementById('message');
    message.innerHTML += msg
}

function signin() {
    if (window.auth2) auth2.signIn({prompt: 'login'});
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
    
    discoverProjects("", 0);
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

function discoverProjects_(path, buildMode, index) {
    if (!signedIn()) {
        allProjectNames = window.openProjectName ? [[window.openProjectName]] : [[]];
        allFolderNames = [[]];
        nestedDirs = [""];
        updateUI();
        return;
    }

    var data = new FormData();
    data.append('id_token', auth2.currentUser.get().getAuthResponse().id_token);
    data.append('mode', buildMode);
    data.append('path', path);

    sendHttp('POST', 'listFolder', data, function(request) {
        if (request.status != 200) {
            return;
        }
        var allContents = JSON.parse(request.responseText);
        allProjectNames[index] = allContents['files'];
        allFolderNames[index] = allContents['dirs'];
        updateUI();
    });
}

function warnIfUnsaved(action, showAnother) {
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
            confirmButtonText: 'Yes, discard my changes!',
            closeOnConfirm: !showAnother
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

    function go(projectName) {
        saveProjectBase(nestedDirs.slice(1).join('/'), projectName);
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
    }, go);
}

function saveProject() {
    if (!signedIn()) {
        sweetAlert('Oops!', 'You must sign in to save files.', 'error');
        updateUI();
        return;
    }

    if (window.openProjectName) {
        saveProjectBase(nestedDirs.slice(1).join('/'), openProjectName);
    } else {
        saveProjectAs();
    }
}

function saveProjectBase_(path, projectName, mode, successFunc) {
    if (projectName == null || projectName == '') return;

    if (!signedIn()) {
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
        data.append('path', path);

        sendHttp('POST', 'saveProject', data, function(request) {
            if (request.status != 200) {
                sweetAlert('Oops!', 'Could not save your project!!!  Please try again.', 'error');
                return;
            }

            successFunc();

            updateUI();

            if (allProjectNames[allProjectNames.length - 1].indexOf(projectName) == -1) {
                discoverProjects(path, allProjectNames.length - 1);
            }
        });
    }

    if (allProjectNames[allProjectNames.length - 1].indexOf(projectName) == -1 || projectName == openProjectName) {
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

function deleteProject_(path, buildMode, successFunc) {
    if (!window.openProjectName) return;

    if (!signedIn()) {
        sweetAlert('Oops', 'You must sign in to delete a project.', 'error');
        updateUI();
        return;
    }

    function go() {
        var data = new FormData();
        data.append('id_token', auth2.currentUser.get().getAuthResponse().id_token);
        data.append('name', window.openProjectName);
        data.append('mode', buildMode);
        data.append('path', path);

        sendHttp('POST', 'deleteProject', data, function(request) {
            if (request.status == 200) {
                successFunc();
                discoverProjects(path, allProjectNames.length - 1);
            }
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

function deleteFolder_(path, buildMode, successFunc) {
    if(path == "" || window.openProjectName != null) {
        return;
    }
    if(!signedIn()) {
        sweetAlert('Oops', 'You must sign in to delete a folder.', 'error');
        updateUI();
        return;
    }

    function go() {
        var data = new FormData();
        data.append('id_token', auth2.currentUser.get().getAuthResponse().id_token);
        data.append('mode', buildMode);
        data.append('path', path);

        sendHttp('POST', 'deleteFolder', data, function(request) {
            if (request.status == 200) {
                successFunc();
                nestedDirs.pop();
                allProjectNames.pop();
                allFolderNames.pop();
                discoverProjects(nestedDirs.slice(1).join('/'), allProjectNames.length - 1);
            }
        });
    }

    var msg = 'Deleting a folder will throw away all of its content, cannot be undone. ' + 'Are you sure?';
    sweetAlert({
        title: 'Warning',
        text: msg,
        type: 'warning',
        showCancelButton: true,
        confirmButtonColor: '#DD6B55',
        confirmButtonText: 'Yes, delete it!'
    }, go);
}

function createFolder(path, buildMode) {
    warnIfUnsaved(function() {
        if(!signedIn()) {
            sweetAlert('Oops!', 'You must sign in to create a folder.', 'error');
            updateUI();
            return;
        }

        function go(folderName) {
            if(folderName == null || folderName == '') {
                return;
            }

            sweetAlert.close();
            var data = new FormData();
            data.append('id_token', auth2.currentUser.get().getAuthResponse().id_token);
            data.append('mode', buildMode);
            if (path == "")
                data.append('path', folderName);
            else
                data.append('path', path + '/' + folderName);

            sendHttp('POST', 'createFolder', data, function(request) {
                if (request.status != 200) {
                    sweetAlert('Oops', 'Could not create your directory! Please try again.', 'error');
                    return;
                }

                allFolderNames[allFolderNames.length - 1].push(folderName);
                setCode('');
                updateUI();
            });
        }

        sweetAlert({
            html: true,
            title: '<i class="mdi mdi72px mdi-folder-plus"></i>&nbsp; Create Folder',
            text: 'Enter a name for your folder:',
            type: 'input',
            inputValue: '',
            confirmButtonText: 'Create',
            showCancelButton: true,
            closeOnConfirm: false
        }, go);
    }, true);
}

function loadProject_(index, name, buildMode, successFunc) {
    
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
    data.append('path', nestedDirs.slice(1, index + 1).join('/'));

    sendHttp('POST', 'loadProject', data, function(request) {
        if (request.status == 200) {
            var project = JSON.parse(request.responseText);

            successFunc(project);
            window.nestedDirs = nestedDirs.slice(0, index + 1);
            window.allProjectNames = allProjectNames.slice(0, index + 1);
            window.allFolderNames = allFolderNames.slice(0, index + 1);
            updateUI();
        }
    });
  }, false);
}

function share() {
  var offerSource = true;

  function go() {
    var url;
    var msg;
    var showConfirm;
    var confirmText;

    if (!window.deployHash) {
      url = window.location.href;
      msg = 'Copy this link to share your program and source code with others!';
      showConfirm = false;
    } else if (offerSource) {
      url = window.location.href;
      msg = 'Copy this link to share your program and source code with others!';
      showConfirm = true;
      confirmText = 'Remove Source Code';
    } else {
      var a = document.createElement('a');
      a.href = window.location.href;
      a.hash = '';
      a.pathname = '/run.html'
      a.search = '?mode=' + window.buildMode + '&dhash=' + window.deployHash;

      url = a.href;
      msg = 'Copy this link to share your program (not source code) with others!';
      showConfirm = true;
      confirmText = 'Share Source Code';
    }

    sweetAlert({
        html: true,
        title: '<i class="mdi mdi-72px mdi-share"></i>&nbsp; Share',
        text: msg,
        type: 'input',
        inputValue: url,
        showConfirmButton: showConfirm,
        confirmButtonText: confirmText,
        closeOnConfirm: false,
        showCancelButton: true,
        cancelButtonText: 'Done',
        animation: 'slide-from-bottom'
    }, function() {
      offerSource = !offerSource;
      go();
    });
  }

  go();
}
