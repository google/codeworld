/*
 * Copyright 2018 The CodeWorld Authors. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://w...content-available-to-author-only...e.org/licenses/LICENSE-2.0
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
    allProjectNames = [[]];
    allFolderNames = [[]];
    openProjectName = null;
    nestedDirs = [""];

    if (window.location.pathname == '/haskell') {
        window.buildMode = 'haskell'
    } else {
        window.buildMode = 'codeworld';
    }

    var hash = location.hash.slice(1);
    if (hash.length > 0) {
        if (hash.slice(-2) == '==') {
            hash = hash.slice(0, -2);
        }
        if(hash[0] == 'F') {
            function go(folderName) {
                var id_token = auth2.currentUser.get().getAuthResponse().id_token;
                var data = new FormData();
                data.append('id_token', id_token);
                data.append('mode', window.buildMode);
                data.append('shash', hash);
                data.append('name', folderName);

                sendHttp('POST', 'shareContent', data, function(request) {
                    window.location.hash = '';
                    if (request.status == 200) {
                        sweetAlert('Success!', 'The shared folder is moved into your root directory.', 'success');
                    } else {
                        sweetAlert('Oops!', 'Could not load the shared directory. Please try again.', 'error');
                    }
                    initCodeworld();
                    registerStandardHints(function(){setMode(true);});
                    discoverProjects("", 0);
                    updateUI();
                });
            }

            sweetAlert({
                html: true,
                title: '<i class="mdi mdi-72px mdi-cloud-upload"></i>&nbsp; Save As',
                text: 'Enter a name for the shared folder:',
                type: 'input',
                confirmButtonText: 'Save',
                showCancelButton: false,
                closeOnConfirm: false
            }, go);
        } else {
            initCodeworld();
            registerStandardHints(function(){setMode(true);});
            updateUI();
        }
    } else {
        initCodeworld();
        registerStandardHints(function(){setMode(true);});
        updateUI();
    }
 
    if (hash.length > 0) {
        if (hash.slice(-2) == '==') {
            hash = hash.slice(0, -2);
        }
        if (hash[0] == 'P') {
            sendHttp('GET', 'loadSource?hash=' + hash + '&mode=' + window.buildMode, null, function(request) {
                if (request.status == 200) {
                    setCode(request.responseText, null, null, true);
                }
            });
        } else if (hash[0] != 'F') {
            setCode('');
            if (!signedIn()) help();
        }
    } else {
        setCode('');
        if (!signedIn()) help();
    }
}

function initCodeworld() {
    var editor = document.getElementById('editor');

    codeworldKeywords = {};

    window.codeworldEditor = CodeMirror.fromTextArea(editor, {
        mode: {
            name: 'codeworld',
            overrideKeywords: codeworldKeywords
        },
        lineNumbers: true,
        autofocus: true,
        matchBrackets: window.buildMode !== 'codeworld',
        highlightParams: window.buildMode === 'codeworld',
        styleActiveLine: !WURFL || !WURFL.is_mobile,
        showTrailingSpace: true,
        indentWithTabs: false,
        autoClearEmptyLines: true,
        highlightSelectionMatches: {
            showToken: /\w/,
            annotateScrollbar: true
        },
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
            "Ctrl-Enter": compile,
            "Ctrl-Up": changeFontSize(1),
            "Ctrl-Down": changeFontSize(-1)
        }
    });
    window.codeworldEditor.refresh();

    CodeMirror.commands.save = function(cm) {
        saveProject();
    }
    document.onkeydown = function(e) {
        if (e.ctrlKey && e.keyCode === 83) { // Ctrl+S
            saveProject();
            return false;
        }
        if (e.ctrlKey && e.keyCode === 73) { // Ctrl+I
            formatSource();
            return false;
        }
    };

    window.codeworldEditor.on('changes', window.updateUI);

    window.onbeforeunload = function(event) {
        if (!isEditorClean()) {
            var msg = 'There are unsaved changes to your project. ' + 'If you continue, they will be lost!';
            if (event) event.returnValue = msg;
            return msg;
        }
    }
}

class CanvasRecorder {
    constructor(canvas, framerate) {
        var cStream = canvas.captureStream(framerate);

        this.chunks = [];
        this.recorder = new MediaRecorder(cStream);
        this.recorder.ondataavailable = this.addChunk(this.chunks);
        this.recorder.onstop = this.exportStream(this.chunks);
    }

    addChunk(chunks) {
        return function(e) {
            chunks.push(e.data);
        }
    }

    exportStream(chunks) {
        return function() {
            var blob = new Blob(chunks);

            // Reset data
            chunks = [];

            // Set file name
            var d = new Date();
            var videoFileName = 'codeworld_recording_'
                                + d.toDateString().split(' ').join('_') + '_'
                                + d.getHours() +':'+ d.getMinutes() +':'+ d.getSeconds()
                                +'.webm';

            // Create a new video link
            var a = document.createElement("a");
            document.body.appendChild(a);
            a.style = "display: none";

            // Save the video
            var url = window.URL.createObjectURL(blob);
            a.href = url;
            a.download = videoFileName;
            a.click();
            window.URL.revokeObjectURL(url);

            // Remove the video link
            a.remove();
        }
    }
}

var canvasRecorder;

function captureStart() {
    var iframe = document.querySelector('#runner');
    var innerDoc = iframe.contentDocument || iframe.contentWindow.document;

    var canvas = innerDoc.querySelector('#screen');

    canvasRecorder = new CanvasRecorder(canvas, 30);

    document.querySelector('#recordIcon').style.display = '';
    document.querySelector('#startRecButton').style.display = 'none';
    document.querySelector('#stopRecButton').style.display = '';

    canvasRecorder.recorder.start();
}

function stopRecording() {
    canvasRecorder.recorder.stop();

    document.querySelector('#recordIcon').style.display = 'none';
    document.querySelector('#startRecButton').style.display = '';
    document.querySelector('#stopRecButton').style.display = 'none';
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

function folderHandler(folderName, index, state) {
    warnIfUnsaved(function() {
        window.nestedDirs = nestedDirs.slice(0, index + 1);
        window.allProjectNames = allProjectNames.slice(0, index + 1);
        window.allFolderNames = allFolderNames.slice(0, index + 1);
        if (!state) {
            nestedDirs.push(folderName);
            allProjectNames.push([]);
            allFolderNames.push([]);
            discoverProjects(nestedDirs.slice(1).join('/'), index + 1);
        }
        if (!window.move) {
            setCode('');
            updateUI();
        } else {
            updateNavBar();
        }
    }, false);
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
            if (window.nestedDirs != "") {
                document.getElementById('deleteButton').style.display = '';
            } else {
                document.getElementById('deleteButton').style.display = 'none';
            }
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

    var debugAvailable = document.getElementById('runner').contentWindow.debugAvailable;
    var debugActive = document.getElementById('runner').contentWindow.debugActive;
    if (debugAvailable) {
        document.getElementById('inspectButton').style.display = '';

        if (debugActive) {
            document.getElementById('inspectButton').style.color = 'black';
        } else {
            document.getElementById('inspectButton').style.color = '';
        }
    } else {
        document.getElementById('inspectButton').style.display = 'none';
    }

    if (window.move) {
      document.getElementById('newButton').style.display = 'none';
      document.getElementById('saveButton').style.display = 'none';
      document.getElementById('saveAsButton').style.display = 'none';
      document.getElementById('deleteButton').style.display = 'none';
      document.getElementById('downloadButton').style.display = 'none';
      document.getElementById('moveButton').style.display = 'none';
      document.getElementById('moveHereButton').style.display = '';
      document.getElementById('cancelMoveButton').style.display = '';
      document.getElementById('runButtons').style.display = 'none';
      document.getElementById('shareFolderButton').style.display = 'none';
    } else {
      document.getElementById('newButton').style.display = '';
      document.getElementById('saveAsButton').style.display = '';
      document.getElementById('downloadButton').style.display = '';
      document.getElementById('runButtons').style.display = '';
      document.getElementById('moveHereButton').style.display = 'none';
      document.getElementById('cancelMoveButton').style.display = 'none';

      if (nestedDirs.length != 1 && (openProjectName == null || openProjectName == '')) {
          document.getElementById('shareFolderButton').style.display = '';
      } else {
          document.getElementById('shareFolderButton').style.display = 'none';
      }

      if((openProjectName != null && openProjectName != '') || nestedDirs.length != 1) {
        document.getElementById('moveButton').style.display = '';
      } else {
        document.getElementById('moveButton').style.display = 'none';
      }
    }

    updateNavBar();

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

function updateNavBar() {
    allProjectNames.forEach(function(projectNames) {
        projectNames.sort(function(a, b) {
            return a.localeCompare(b);
        });
    });

    allFolderNames.forEach(function(folderNames) {
        folderNames.sort(function(a, b) {
            return a.localeCompare(b);
        });
    });

    var makeDirNode = function(name, isOpen, level) {
        var encodedName = name
            .replace('&', '&amp;')
            .replace('<', '&lt;')
            .replace('>', '&gt;');
        var templateName = isOpen ? 'openFolderTemplate' : 'folderTemplate';
        var template = document.getElementById(templateName).innerHTML;
        template = template.replace('{{label}}', encodedName);
        var span = document.createElement('span');
        span.innerHTML = template;
        var elem = span.getElementsByTagName('a')[0];
        elem.style.marginLeft = (3 + 16 * level) + 'px';
        elem.onclick = function() {
            folderHandler(name, level, isOpen);
        };
        span.style.display = 'flex';
        span.style.flexDirection = 'column';

        return span;
    };

    var makeProjectNode = function(name, level, active) {
        var title = name;
        if(active && !isEditorClean()) {
            title = "* " + title;
        }
        var encodedName = title
            .replace('&', '&amp;')
            .replace('<', '&lt;')
            .replace('>', '&gt;');
        var template = document.getElementById('projectTemplate').innerHTML;
        template = template.replace('{{label}}', encodedName);
        template = template.replace(/{{ifactive ([^}]*)}}/, active ? "$1" : "");
        var span = document.createElement('span');
        span.innerHTML = template;
        var elem = span.getElementsByTagName('a')[0];
        elem.style.marginLeft = (3 + 16 * level) + 'px';
        elem.onclick = function() {
            loadProject(name, level);
        };
        span.style.display = 'flex';
        span.style.flexDirection = 'column';
        return span;
    };

    var projects = document.getElementById('nav_mine');

    while (projects.lastChild) {
        projects.removeChild(projects.lastChild);
    }

    for (let i = 0; i < nestedDirs.length; i++) {
        var nextProjects;
        allFolderNames[i].forEach(function(folderName) {
            var active = i + 1 < nestedDirs.length && nestedDirs[i + 1] == folderName;
            if (!signedIn() && !active) {
                return;
            }
            var span = makeDirNode(folderName, active, i);
            projects.appendChild(span);
            if (active) {
                nextProjects = span.appendChild(document.createElement('div'));
            }
        });
        allProjectNames[i].forEach(function(projectName) {
            var active = i + 1 == nestedDirs.length && window.openProjectName == projectName;
            if (!signedIn() && !active) {
                return;
            }
            var span = makeProjectNode(projectName, i, active);
            projects.appendChild(span);
        });
        projects = nextProjects;
    }
}

function moveProject() {
    warnIfUnsaved(function() {
        if (!signedIn()) {
            sweetAlert('Oops!', 'You must sign in to move this project or folder.', 'error');
            updateUI();
            return;
        }

        if ((openProjectName == null || openProjectName == '') && nestedDirs.length == 1) {
            sweetAlert('Oops!', 'You must select a project or folder to move.', 'error');
            updateUI();
            return;
        }

        var tempOpen = openProjectName;
        var tempPath = nestedDirs.slice(1).join('/');
        setCode('');
        if (tempOpen == null || tempOpen == '') {
            nestedDirs.splice(-1);
            allProjectNames.splice(-1);
            allFolderNames.splice(-1);
        }

        window.move = Object();
        window.move.path = tempPath;
        if (tempOpen != null && tempOpen != '') {
            window.move.file = tempOpen;
        }

        updateNavBar();
        discoverProjects("", 0);
    }, false);
}

function moveHere() {
    function successFunc() {
        nestedDirs = [""];
        discoverProjects("", 0);
        cancelMove();
        updateUI();
    }
    moveHere_(nestedDirs.slice(1).join('/'), window.buildMode, successFunc);
}

function changeFontSize(incr) {
    return function() {
        var elem = window.codeworldEditor.getWrapperElement();
        var fontParts = window.getComputedStyle(elem)['font-size'].match(/^([0-9]+)(.*)$/);
        var fontSize = 12;
        var fontUnit = 'px';
        if (fontParts.length >= 3) {
          fontSize = parseInt(fontParts[1]);
          fontUnit = fontParts[2];
        }
        fontSize += incr;
        if(fontSize < 8) fontSize = 8;
        elem.style.fontSize = fontSize + fontUnit;
        window.codeworldEditor.refresh();
    }
}

function help() {
    var url = 'doc.html?shelf=help/' + window.buildMode + '.shelf';
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

function editorHelp(doc) {
    var helpText = "<h3>Editor Shortcuts</h3>" +
        "<div id='keyboard-shortcuts'><table><tbody>" +
        "<tr><td>Ctrl + Space / Shift + Space </td><td> Autocomplete</td></tr>" +
        "<tr><td>Ctrl + Up </td><td> Zoom In </td></tr>" +
        "<tr><td>Ctrl + Down </td><td>  Zoom Out </td></tr>" +
        "<tr><td>Ctrl + A </td><td>  Select All </td></tr>" +
        "<tr><td>Ctrl + Home </td><td>  Go to Start</td></tr>" +
        "<tr><td>Ctrl + End </td><td>  Go to End </td></tr>" +
        "<tr><td>Alt + Left </td><td>  Go to start of line</td></tr>" +
        "<tr><td>Alt + Right </td><td>  Go to end of line</td></tr>" +
        "<tr><td>Ctrl + D </td><td>  Delete Line </td></tr>" +
        "<tr><td>Ctrl + Left </td><td>  Go one word Left</td></tr>" +
        "<tr><td>Ctrl + Right </td><td>  Go one word Right </td></tr>" +
        "<tr><td>Ctrl + Backspace </td><td>  Delete previous word</td></tr>" +
        "<tr><td>Ctrl + Delete </td><td>  Delete next word</td></tr>" +
        "<tr><td>Ctrl + F </td><td>  Search </td></tr>" +
        "<tr><td>Ctrl + G </td><td>  Find next occurence </td></tr>" +
        "<tr><td>Ctrl + Shift + G </td><td>  Find previous occurence </td></tr>" +
        "<tr><td>Ctrl + Shift + F </td><td>  Replace </td></tr>" +
        "<tr><td>Ctrl + Shift + R </td><td>  Replace All </td></tr>" +
        "<tr><td>Ctrl + S </td><td> Save </td></tr>" +
        "<tr><td>Ctrl + Z </td><td> Undo </td></tr>" +
        "<tr><td>Ctrl + Shift + Z / Ctrl + Y </td><td> Redo </td></tr>" +
        "<tr><td>Ctrl + U </td><td> Undo Selection </td></tr>" +
        "<tr><td>Ctrl + Shift +  U / Alt + U </td><td> Redo Selection </td></tr>" +
        "<tr><td>Tab / Ctrl + ] </td><td> Indent </td></tr>" +
        "<tr><td>Shift + Tab / Ctrl + [ </td><td> Un-indent </td></tr>" +
        "<tr><td>Ctrl + I </td><td> Reformat (Haskell Mode Only) </td></tr>" +
        "</tbody></table></div>";
    sweetAlert({
        title: '',
        text: helpText,
        html: true,
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
    }, false);
}

function newProject() {
    warnIfUnsaved(function() {
        setCode('');
    }, false);
}

function newFolder() {
    function successFunc() {
        if (!window.move)
            setCode('');
    }
    createFolder(nestedDirs.slice(1).join('/'), window.buildMode, successFunc);
}

function loadProject(name, index) {
    if (window.move) {
        return;
    }
    function successFunc(project){
        setCode(project.source, project.history, name);
    }
    loadProject_(index, name, window.buildMode, successFunc);
}

function formatSource() {
    if (window.buildMode == 'codeworld') {
      // Unfortunately, there isn't an acceptable style for CodeWorld yet.
      return;
    }

    var src = window.codeworldEditor.getValue();
    var data = new FormData();
    data.append('source', src);
    data.append('mode', window.buildMode);

    sendHttp('POST', 'indent', data, function(request) {
        if (request.status == 200) {
          codeworldEditor.getDoc().setValue(request.responseText);
        }
    });
}

function stop() {
    run('', '', '', false);
}

function run(hash, dhash, msg, error) {
    var runner = document.getElementById('runner');

    // Stop canvas recording if the recorder is active
    if (canvasRecorder && canvasRecorder.recorder.state === "recording") {
        stopRecording();
    }

    if (hash) {
        window.location.hash = '#' + hash;
        document.getElementById('shareButton').style.display = '';
    } else {
        window.location.hash = '';
        document.getElementById('shareButton').style.display = 'none';
    }

    if (dhash) {
        var loc = 'run.html?dhash=' + dhash + '&mode=' + window.buildMode;
        runner.contentWindow.location.replace(loc);
        if (!!navigator.mediaDevices && !!navigator.mediaDevices.getUserMedia) {
            document.getElementById('startRecButton').style.display = '';
        }
    } else {
        runner.contentWindow.location.replace('about:blank');
        document.getElementById('runner').style.display = 'none';
        document.getElementById('startRecButton').style.display = 'none';
    }

    if (hash || msg) {
        cancelMove();
        window.mainLayout.show('east');
        window.mainLayout.open('east');
        document.getElementById('shareFolderButton').style.display = 'none';
    } else {
        document.getElementById('shareFolderButton').style.display = '';
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

    window.deployHash = dhash;

    updateUI();

    document.getElementById('runner').addEventListener('load', function () {
        updateUI();
    });
}

function goto(line, col) {
    codeworldEditor.getDoc().setCursor(line - 1, col - 1);
    codeworldEditor.scrollIntoView(null, 100);
    codeworldEditor.focus();
}

function compile() {
    run('', '', 'Compiling...', false);

    if (document.getElementById("runner").contentWindow.debugActive)
        document.getElementById("runner").contentWindow.stopDebugMode();

    destroyTreeDialog();

    var src = window.codeworldEditor.getValue();
    var data = new FormData();
    data.append('source', src);
    data.append('mode', window.buildMode);

    sendHttp('POST', 'compile', data, function(request) {
        var success = request.status == 200;

        var hash;
        var dhash;
        if (request.responseText.length == 23) {
          hash = request.responseText;
          dhash = null;
        } else {
          var obj = JSON.parse(request.responseText);
          hash = obj.hash;
          dhash = obj.dhash;
        }

        var data = new FormData();
        data.append('hash', hash);
        data.append('mode', window.buildMode);

        sendHttp('POST', 'runMsg', data, function(request) {
            var msg = '';
            if (request.status == 200) {
                msg = request.responseText;
            } else if (request.status >= 400) {
                msg = "Sorry!  Your program couldn't be run right now.  Please try again.";
            }

            if (success) {
                run(hash, dhash, 'Running...\n\n' + msg, false);
            } else {
                run(hash, '', msg, true);
            }
        });
    });
}

function signinCallback(result) {
    discoverProjects("", 0);
    cancelMove();
    updateUI();
    if (signedIn()) {
        sweetAlert.close();
    }
}

function discoverProjects(path, index){
    discoverProjects_(path, window.buildMode, index);
}

function saveProjectBase(path, projectName) {
    function successFunc() {
        window.openProjectName = projectName;
        var doc = window.codeworldEditor.getDoc();
        window.savedGeneration = doc.changeGeneration(true);
    }

    saveProjectBase_(path, projectName, window.buildMode, successFunc);
}

function deleteFolder() {
    var path = nestedDirs.slice(1).join('/')
    if (path == "" || window.openProjectName != null) {
        return;
    }
    function successFunc() {
        savedGeneration = codeworldEditor.getDoc().changeGeneration(true);
        setCode('');
    }
    deleteFolder_(path, window.buildMode, successFunc);
}

function deleteProject() {
    if (!window.openProjectName) {
        deleteFolder();
        return;
    }
    function successFunc(){
        savedGeneration = codeworldEditor.getDoc().changeGeneration(true);
        setCode('');
    }
    var path = nestedDirs.slice(1).join('/');
    deleteProject_(path, window.buildMode, successFunc);
}

function shareFolder() {
    shareFolder_('codeworld');
}

function downloadProject() {
    var blob = new Blob(
        [window.codeworldEditor.getDoc().getValue()],
        { type: 'text/plain', endings: 'native' });
    var filename = "untitled.hs";
    if (window.openProjectName) filename = window.openProjectName + '.hs';

    if (window.navigator.msSaveBlob) {
        window.navigator.msSaveBlob(blob, filename);
    } else {
        var elem = window.document.createElement('a');
        elem.href = window.URL.createObjectURL(blob);
        elem.download = filename;
        document.body.appendChild(elem);
        elem.click();
        document.body.removeChild(elem);
    }
}
