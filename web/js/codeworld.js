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
 * Initializes the programming environment.  This is called after the
 * entire document body and other JavaScript has loaded.
 */
function init() {
    allProjectNames = [];
    openProjectName = null;

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

    registerStandardHints(function(){setMode(true);});

    updateUI();

    window.onbeforeunload = function(event) {
        if (!isEditorClean()) {
            var msg = 'There are unsaved changes to your project. ' + 'If you continue, they will be lost!';
            if (event) event.returnValue = msg;
            return msg;
        }
    }

    var hash = location.hash.slice(1);
    if (hash.length > 0) {
        if (hash.slice(-2) == '==') {
            hash = hash.slice(0, -2);
        }
        sendHttp('GET', 'loadSource?hash=' + hash + '&mode=' + window.buildMode, null, function(request) {
            if (request.status == 200) {
                setCode(request.responseText, null, null, true);
            }
        });
    } else {
        setCode('');
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

function getCurrentProject() {
  var doc = window.codeworldEditor.getDoc();
  return {
      'name': window.openProjectName || 'Untitled',
      'source': doc.getValue(),
      'history': doc.getHistory()
  };
}

/*
 * Updates all UI components to reflect the current state.  The general pattern
 * is to modify the state stored in variables and such, and then call updateUI
 * to get the visual presentation to match.
 */
function updateUI() {
    var isSignedIn = signedIn();
    if (isSignedIn) {
        if (document.getElementById('signout').style.display == 'none') {
            document.getElementById('signin').style.display = 'none';
            document.getElementById('signout').style.display = '';
            document.getElementById('navButton').style.display = '';
            window.mainLayout.show('west');
            window.mainLayout.open('west');
        }

        if (window.openProjectName) {
            document.getElementById('saveButton').style.display = '';
            document.getElementById('deleteButton').style.display = '';
        } else {
            document.getElementById('saveButton').style.display = 'none';
            document.getElementById('deleteButton').style.display = 'none';
        }
    } else {
        if (document.getElementById('signout').style.display == '') {
            document.getElementById('signin').style.display = '';
            document.getElementById('signout').style.display = 'none';
            document.getElementById('saveButton').style.display = 'none';
            window.mainLayout.hide('west');
        }
        document.getElementById('navButton').style.display = 'none';
        document.getElementById('deleteButton').style.display = 'none';
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

function help(doc) {
    if (!doc) doc = window.buildMode;
    var url = 'doc.html?help/' + doc + '.md';
    sweetAlert({
        title: '',
        text: '<iframe id="doc" style="width: 100%; height: 100%" class="dropbox" src="' + url + '"></iframe>',
        html: true,
        customClass: 'helpdoc',
        allowEscapeKey: true,
        allowOutsideClick: true,
        showConfirmButton: false,
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

function loadSample(code) {
    if (isEditorClean()) sweetAlert.close();
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
  function successFunc(project){
    setCode(project.source, project.history, name);
  }
  loadProject_(name, window.buildMode, successFunc);
}

function stop() {
    run('', '', false);
}

function run(hash, msg, error) {
    if (hash) {
        window.location.hash = '#' + hash;
        document.getElementById('shareButton').style.display = '';
    } else {
        window.location.hash = '';
        document.getElementById('shareButton').style.display = 'none';
    }

    var runner = document.getElementById('runner');
    if (hash || msg) {
        if (hash && !error) {
            var loc = 'run.html?hash=' + hash + '&mode=' + window.buildMode;
            runner.contentWindow.location.replace(loc);
            document.getElementById('runner').style.display = '';
            document.getElementById('runner').contentWindow.focus();
        } else {
            runner.contentWindow.location.replace('about:blank');
            document.getElementById('runner').style.display = 'none';
        }
        window.mainLayout.show('east');
        window.mainLayout.open('east');
    } else {
        window.mainLayout.hide('east');
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

function signinCallback(result) {
    discoverProjects();
    updateUI();
}

function discoverProjects(){
  discoverProjects_(window.buildMode);
}

function saveProjectBase(projectName) {

    function successFunc() {
            window.openProjectName = projectName;
            var doc = window.codeworldEditor.getDoc();
            window.savedGeneration = doc.changeGeneration(true);
            updateUI();

            if (allProjectNames.indexOf(projectName) == -1) {
                discoverProjects();
            }
    }

    saveProjectBase_(projectName, window.buildMode, successFunc);
}

function deleteProject()
{
  function successFunc(){
    savedGeneration = codeworldEditor.getDoc().changeGeneration(true);
    setCode('');
  }
  deleteProject_(window.buildMode, successFunc);
}
