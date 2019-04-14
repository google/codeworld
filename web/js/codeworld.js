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
'use strict';

const autohelpEnabled = location.hash.length <= 2;

/*
 * Initializes the programming environment.  This is called after the
 * entire document body and other JavaScript has loaded.
 */
async function init() {
    await Alert.init();
    await Auth.init();

    // Keep the base bundle preloaded by retrying regularly.
    function preloadBaseBundle() {
        const request = new XMLHttpRequest();
        request.open('GET', '/runBaseJS', true);
        request.setRequestHeader('Cache-control', 'max-stale');
        request.send();
    }
    preloadBaseBundle();
    window.setInterval(preloadBaseBundle, 1000 * 60 * 60);

    window.allProjectNames = [
        []
    ];
    window.allFolderNames = [
        []
    ];
    window.openProjectName = null;
    window.nestedDirs = [''];

    window.savedGeneration = null;
    window.runningGeneration = null;

    if (window.location.pathname === '/haskell') {
        window.buildMode = 'haskell';
    } else {
        window.buildMode = 'codeworld';
    }
    document.documentElement.classList.add(window.buildMode);

    window.cancelCompile = () => {};

    let hash = location.hash.slice(1);
    if (hash.length > 0) {
        if (hash.slice(-2) === '==') {
            hash = hash.slice(0, -2);
        }
        if (hash[0] === 'F') {
            sweetAlert({
                title: Alert.title('Save As', 'mdi-cloud-upload'),
                html: 'Enter a name for the shared folder:',
                input: 'text',
                confirmButtonText: 'Save',
                showCancelButton: false,
                closeOnConfirm: false
            }).then(result => {
                if (!result) {
                    return;
                }

                const data = new FormData();
                data.append('mode', window.buildMode);
                data.append('shash', hash);
                data.append('name', result.value);

                sendHttp('POST', 'shareContent', data, request => {
                    window.location.hash = '';
                    if (request.status === 200) {
                        sweetAlert('Success!',
                            'The shared folder has been copied to your root directory.',
                            'success');
                    } else {
                        sweetAlert('Oops!',
                            'Could not load the shared directory. Please try again.',
                            'error');
                    }
                    initCodeworld();
                    registerStandardHints(() => {
                        setMode(true);
                        parseSymbolsFromCurrentCode();
                    });
                    discoverProjects('', 0);
                    updateUI();
                });
            });
        } else {
            initCodeworld();
            registerStandardHints(() => {
                setMode(true);
                parseSymbolsFromCurrentCode();
            });
            updateUI();
        }
    } else {
        initCodeworld();
        registerStandardHints(() => {
            setMode(true);
            parseSymbolsFromCurrentCode();
        });
        updateUI();
    }

    if (hash.length > 0) {
        if (hash.slice(-2) === '==') {
            hash = hash.slice(0, -2);
        }
        if (hash[0] === 'P') {
            sendHttp('GET', `loadSource?hash=${hash}&mode=${window.buildMode}`,
                null, request => {
                    if (request.status === 200) {
                        setCode(request.responseText, null, null, true);
                    }
                });
        } else if (hash[0] !== 'F') {
            setCode('');
        }
    } else {
        setCode('');
    }
}

function initCodeworld() {
    const editor = document.getElementById('editor');

    window.codeworldKeywords = {};

    window.codeworldEditor = CodeMirror.fromTextArea(editor, {
        mode: {
            name: 'codeworld',
            overrideKeywords: window.codeworldKeywords
        },
        undoDepth: 50,
        lineNumbers: true,
        autofocus: true,
        matchBrackets: true,
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
            color: '#bbb',
            lineStyle: 'dashed'
        }],
        extraKeys: {
            'Ctrl-Space': 'autocomplete',
            'Shift-Space': 'autocomplete',
            'Tab': 'indentMore',
            'Shift-Tab': 'indentLess',
            'Ctrl-Enter': compile,
            'Ctrl-Up': changeFontSize(1),
            'Ctrl-Down': changeFontSize(-1)
        },
        textHover: onHover,
        gutters: ['CodeMirror-lint-markers'],
        lint: {
            getAnnotations: (text, callback) => {
                if (text.trim() === '') {
                    callback([]);
                    return;
                }

                let request = null;

                function cancelLintRequest() {
                    if (window.codeworldEditor) {
                        window.codeworldEditor.off('change',
                            cancelLintRequest);
                    }
                    if (request) request.abort();
                }

                const data = new FormData();
                data.append('source', text);
                data.append('mode', window.buildMode);
                request = sendHttp('POST', 'errorCheck', data,
                    request => {
                        if (window.codeworldEditor) {
                            window.codeworldEditor.off('change',
                                cancelLintRequest);
                        }

                        if (request.status === 400 || request.status === 200) {
                            const messages = parseCompileErrors(request.responseText);
                            callback(messages);
                        } else if (request.status === 0) {
                            // Request was cancelled because of a later change.  Do nothing.
                        } else {
                            console.log(
                                'Not expected behavior: don\'t know how to ' +
                                'handle request with status ',
                                request.status, request);
                        }
                    });
                if (window.codeworldEditor) {
                    window.codeworldEditor.on('change',
                        cancelLintRequest);
                }
            },
            async: true
        }
    });
    window.codeworldEditor.refresh();
    window.codeworldEditor.on("cursorActivity", function() {
        const prevDiv = document.getElementById("function-details");
        if (prevDiv) prevDiv.remove();

        const cursor = window.codeworldEditor.getCursor();
        const currentToken = window.codeworldEditor.getTokenAt(cursor);
        const functions = currentToken.state.contexts.filter(ctx => ctx.functionName);

        if (!functions.length) return;

        const { functionName, argIndex } = functions.pop();
        const keywordData = window.codeWorldSymbols[functionName];

        // don't show tooltip if function details or argument types are not known
        if (!keywordData || keywordData.declaration === functionName) return;

        const topDiv = document.createElement('div');

        topDiv.title = functionName;
        topDiv.id = "function-details";

        const docDiv = document.createElement('div');
        docDiv.classList.add("function-tooltip-styling");

        const annotation = document.createElement('div');
        renderDeclaration(annotation, functionName, keywordData, 9999, argIndex);
        annotation.className = 'hover-decl';
        docDiv.appendChild(annotation);

        topDiv.appendChild(docDiv);
        window.codeworldEditor.addWidget(cursor, topDiv, true, "above", "left");
    });

    CodeMirror.commands.save = cm => {
        saveProject();
    };
    document.onkeydown = e => {
        if (e.ctrlKey && e.keyCode === 83) { // Ctrl+S
            saveProject();
            return false;
        }
        if (e.ctrlKey && e.keyCode === 73) { // Ctrl+I
            formatSource();
            return false;
        }
    };

    window.reparseTimeoutId = null;
    window.codeworldEditor.on('changes', () => {
        if (window.reparseTimeoutId) clearTimeout(window.reparseTimeoutId);
        window.reparseTimeoutId = setTimeout(
            parseSymbolsFromCurrentCode, 1500);
        window.updateUI();
    });

    window.onbeforeunload = event => {
        if (!isEditorClean()) {
            const msg = 'There are unsaved changes to your project. ' +
                'If you continue, they will be lost!';
            if (event) event.returnValue = msg;
            return msg;
        }
    };

    window.onresize = () => {
        if (window.resizeTimeoutId) clearTimeout(window.resizeTimeoutId);
        window.resizeTimeoutId = setTimeout(() => {
            window.codeworldEditor.setSize();
        }, 1000);
    };
}

class CanvasRecorder {
    constructor(canvas, framerate) {
        const cStream = canvas.captureStream(framerate);

        this.chunks = [];
        this.recorder = new MediaRecorder(cStream);
        this.recorder.ondataavailable = this.addChunk(this.chunks);
        this.recorder.onstop = this.exportStream(this.chunks);
    }

    addChunk(chunks) {
        return e => {
            chunks.push(e.data);
        };
    }

    exportStream(chunks) {
        return () => {
            const blob = new Blob(chunks);

            // Reset data
            chunks = [];

            // Set file name
            const d = new Date();
            const videoFileName = `codeworld_recording_${ 
                d.toDateString().split(' ').join('_')}_${ 
                d.getHours()}:${d.getMinutes()}:${d.getSeconds() 
            }.webm`;

            // Create a new video link
            const a = document.createElement('a');
            document.body.appendChild(a);
            a.style = 'display: none';

            // Save the video
            const url = window.URL.createObjectURL(blob);
            a.href = url;
            a.download = videoFileName;
            a.click();
            window.URL.revokeObjectURL(url);

            // Remove the video link
            a.remove();
        };
    }
}

let canvasRecorder;

function captureStart() {
    const iframe = document.querySelector('#runner');
    const innerDoc = iframe.contentDocument || iframe.contentWindow.document;

    const canvas = innerDoc.querySelector('#screen');

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
    if (window.buildMode === 'haskell') {
        if (force || window.codeworldEditor.getMode().name !== 'haskell') {
            window.codeworldEditor.setOption('mode', 'haskell');
        }
    } else {
        if (force || window.codeworldEditor.getMode().name !== 'codeworld') {
            window.codeworldEditor.setOption(
                'mode', {
                    name: 'codeworld',
                    overrideKeywords: window.codeworldKeywords
                });
        }
    }
}

function getCurrentProject() {
    const doc = window.codeworldEditor.getDoc();
    return {
        'name': window.openProjectName || 'Untitled',
        'source': doc.getValue(),
        'history': doc.getHistory()
    };
}

function folderHandler(folderName, index, state) {
    warnIfUnsaved(() => {
        window.nestedDirs = window.nestedDirs.slice(0, index + 1);
        window.allProjectNames = window.allProjectNames.slice(0, index + 1);
        window.allFolderNames = window.allFolderNames.slice(0, index + 1);
        if (!state) {
            window.nestedDirs.push(folderName);
            window.allProjectNames.push([]);
            window.allFolderNames.push([]);
            discoverProjects(window.nestedDirs.slice(1).join('/'), index + 1);
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
    const isSignedIn = signedIn();
    if (isSignedIn) {
        if (document.getElementById('signout').style.display === 'none') {
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
            if (window.nestedDirs !== '') {
                document.getElementById('deleteButton').style.display = '';
            } else {
                document.getElementById('deleteButton').style.display = 'none';
            }
        }
    } else {
        if (document.getElementById('signout').style.display === '') {
            document.getElementById('signin').style.display = '';
            document.getElementById('signout').style.display = 'none';
            document.getElementById('saveButton').style.display = 'none';
            window.mainLayout.hide('west');
        }
        document.getElementById('navButton').style.display = 'none';
        document.getElementById('deleteButton').style.display = 'none';
    }

    const debugAvailable = document.getElementById('runner').contentWindow.debugAvailable;
    const debugActive = document.getElementById('runner').contentWindow.debugActive;
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

        if (window.nestedDirs.length > 1 && !window.openProjectName) {
            document.getElementById('shareFolderButton').style.display = '';
        } else {
            document.getElementById('shareFolderButton').style.display = 'none';
        }

        if (window.openProjectName || window.nestedDirs.length > 1) {
            document.getElementById('moveButton').style.display = '';
        } else {
            document.getElementById('moveButton').style.display = 'none';
        }
    }

    updateNavBar();

    let title;
    if (window.openProjectName) {
        title = window.openProjectName;
    } else {
        title = '(new)';
    }

    if (!isEditorClean()) {
        title = `* ${title}`;
    }

    // If true - code currently in document is not equal to
    // last compiled code
    const running = document.getElementById('runner').style.display !== 'none';
    const obsolete = !window.codeworldEditor.getDoc().isClean(window.runningGeneration);
    const obsoleteAlert = document.getElementById('obsolete-code-alert');
    if (running && obsolete) {
        obsoleteAlert.classList.add('obsolete-code-alert-fadein');
        obsoleteAlert.classList.remove('obsolete-code-alert-fadeout');
    } else {
        obsoleteAlert.classList.add('obsolete-code-alert-fadeout');
        obsoleteAlert.classList.remove('obsolete-code-alert-fadein');
    }

    document.title = `${title} - CodeWorld`;
}

function updateNavBar() {
    window.allProjectNames.forEach(projectNames => {
        projectNames.sort((a, b) => a.localeCompare(b));
    });

    window.allFolderNames.forEach(folderNames => {
        folderNames.sort((a, b) => a.localeCompare(b));
    });

    const makeDirNode = (name, isOpen, level) => {
        const encodedName = name
            .replace(/&/g, '&amp;')
            .replace(/</g, '&lt;')
            .replace(/>/g, '&gt;');
        const templateName = isOpen ? 'openFolderTemplate' : 'folderTemplate';
        let template = document.getElementById(templateName).innerHTML;
        template = template.replace(/{{label}}/g, encodedName);
        const span = document.createElement('span');
        span.innerHTML = template;
        const elem = span.getElementsByTagName('a')[0];
        elem.style.marginLeft = `${3 + 16 * level}px`;
        elem.onclick = () => {
            folderHandler(name, level, isOpen);
        };
        span.style.display = 'flex';
        span.style.flexDirection = 'column';

        return span;
    };

    const makeProjectNode = (name, level, active) => {
        let title = name;
        if (active && !isEditorClean()) {
            title = `* ${title}`;
        }
        const encodedName = title
            .replace(/&/g, '&amp;')
            .replace(/</g, '&lt;')
            .replace(/>/g, '&gt;');
        let template = document.getElementById('projectTemplate').innerHTML;
        template = template.replace(/{{label}}/g, encodedName);
        template = template.replace(/{{ifactive ([^}]*)}}/g, active ? '$1' :
            '');
        const span = document.createElement('span');
        span.innerHTML = template;
        const elem = span.getElementsByTagName('a')[0];
        elem.style.marginLeft = `${3 + 16 * level}px`;
        elem.onclick = () => {
            loadProject(name, level);
        };
        span.style.display = 'flex';
        span.style.flexDirection = 'column';
        return span;
    };

    let projects = document.getElementById('nav_mine');

    while (projects.lastChild) {
        projects.removeChild(projects.lastChild);
    }

    for (let i = 0; i < window.nestedDirs.length; i++) {
        let nextProjects = null;
        window.allFolderNames[i].forEach(folderName => {
            const active = i + 1 < window.nestedDirs.length && window.nestedDirs[i + 1] === folderName;
            if (!signedIn() && !active) {
                return;
            }
            const span = makeDirNode(folderName, active, i);
            projects.appendChild(span);
            if (active) {
                nextProjects = span.appendChild(document.createElement(
                    'div'));
            }
        });
        window.allProjectNames[i].forEach(projectName => {
            const active = i + 1 === window.nestedDirs.length && window.openProjectName === projectName;
            if (!signedIn() && !active) {
                return;
            }
            const span = makeProjectNode(projectName, i, active);
            projects.appendChild(span);
        });
        if (nextProjects) projects = nextProjects;
    }

    if (projects && window.loadingDir) {
        const template = document.getElementById('loaderTemplate').innerHTML;
        const span = document.createElement('span');
        span.innerHTML = template;
        const elem = span.getElementsByTagName('a')[0];
        elem.style.marginLeft = `${3 + 16 * (window.nestedDirs.length - 1)}px`;
        span.style.display = 'flex';
        span.style.flexDirection = 'column';
        projects.appendChild(span);
    }
}

function moveProject() {
    warnIfUnsaved(() => {
        if (!signedIn()) {
            sweetAlert('Oops!',
                'You must sign in to move this project or folder.',
                'error');
            updateUI();
            return;
        }

        if (!window.openProjectName && window.nestedDirs.length === 1) {
            sweetAlert('Oops!',
                'You must select a project or folder to move.',
                'error');
            updateUI();
            return;
        }

        const tempOpen = window.openProjectName;
        const tempPath = window.nestedDirs.slice(1).join('/');
        setCode('');
        if (!tempOpen) {
            window.nestedDirs.splice(-1);
            window.allProjectNames.splice(-1);
            window.allFolderNames.splice(-1);
        }

        window.move = Object();
        window.move.path = tempPath;
        if (tempOpen) {
            window.move.file = tempOpen;
        }

        discoverProjects('', 0);
        updateNavBar();
    }, false);
}

function moveHere() {
    moveHere_(window.nestedDirs.slice(1).join('/'), window.buildMode, () => {
        window.nestedDirs = [''];
        discoverProjects('', 0);
        cancelMove();
        updateUI();
    });
}

function changeFontSize(incr) {
    return () => {
        const elem = window.codeworldEditor.getWrapperElement();
        const fontParts = window.getComputedStyle(elem)['font-size'].match(
            /^([0-9]+)(.*)$/);
        let fontSize = 12;
        let fontUnit = 'px';
        if (fontParts.length >= 3) {
            fontSize = parseInt(fontParts[1]);
            fontUnit = fontParts[2];
        }
        fontSize += incr;
        if (fontSize < 8) fontSize = 8;
        elem.style.fontSize = fontSize + fontUnit;
        window.codeworldEditor.refresh();
    };
}

function help() {
    let url;
    if (window.buildMode === 'haskell') {
        url = 'doc-haskell/CodeWorld.html';
    } else {
        url = `doc.html?shelf=help/${window.buildMode}.shelf`;
    }

    sweetAlert({
        html: `<iframe id="doc" style="width: 100%; height: 100%" class="dropbox" src="${ 
            url}"></iframe>`,
        customClass: 'helpdoc',
        allowEscapeKey: true,
        allowOutsideClick: true,
        showConfirmButton: false,
    }).then(() => {
        const docIframe = document.getElementById('doc');
        docIframe.contentWindow.savePosition();
    });
}

function editorHelp(doc) {
    const helpText = '<h3>Editor Shortcuts</h3>' +
        '<div id=\'keyboard-shortcuts\'><table><tbody>' +
        '<tr><td>Ctrl + Enter </td><td>  Run the program</td></tr>' +
        '<tr><td>Ctrl + Space / Shift + Space </td><td> Autocomplete</td></tr>' +
        '<tr><td>Ctrl + Up </td><td> Zoom in </td></tr>' +
        '<tr><td>Ctrl + Down </td><td>  Zoom out </td></tr>' +
        '<tr><td>Ctrl + A </td><td>  Select all </td></tr>' +
        '<tr><td>Ctrl + Home </td><td>  Go to start</td></tr>' +
        '<tr><td>Ctrl + End </td><td>  Go to end </td></tr>' +
        '<tr><td>Alt + Left </td><td>  Go to start of line</td></tr>' +
        '<tr><td>Alt + Right </td><td>  Go to end of line</td></tr>' +
        '<tr><td>Ctrl + D </td><td>  Delete line </td></tr>' +
        '<tr><td>Ctrl + Left </td><td>  Go one word left</td></tr>' +
        '<tr><td>Ctrl + Right </td><td>  Go one word right </td></tr>' +
        '<tr><td>Ctrl + Backspace </td><td>  Delete previous word</td></tr>' +
        '<tr><td>Ctrl + Delete </td><td>  Delete next word</td></tr>' +
        '<tr><td>Ctrl + F </td><td>  Search </td></tr>' +
        '<tr><td>Ctrl + G </td><td>  Find next occurrence </td></tr>' +
        '<tr><td>Ctrl + Shift + G </td><td>  Find previous occurrence </td></tr>' +
        '<tr><td>Ctrl + Shift + F </td><td>  Replace </td></tr>' +
        '<tr><td>Ctrl + Shift + R </td><td>  Replace all </td></tr>' +
        '<tr><td>Ctrl + S </td><td> Save </td></tr>' +
        '<tr><td>Ctrl + Z </td><td> Undo </td></tr>' +
        '<tr><td>Ctrl + Shift + Z / Ctrl + Y </td><td> Redo </td></tr>' +
        '<tr><td>Ctrl + U </td><td> Undo selection </td></tr>' +
        '<tr><td>Ctrl + Shift +  U / Alt + U </td><td> Redo selection </td></tr>' +
        '<tr><td>Tab / Ctrl + ] </td><td> Indent </td></tr>' +
        '<tr><td>Shift + Tab / Ctrl + [ </td><td> Unindent </td></tr>' +
        '<tr><td>Ctrl + I </td><td> Reformat (Haskell mode only) </td></tr>' +
        '</tbody></table></div>';
    sweetAlert({
        html: helpText,
        allowEscapeKey: true,
        allowOutsideClick: true,
        showConfirmButton: false,
    });
}

function isEditorClean() {
    const doc = window.codeworldEditor.getDoc();

    if (window.savedGeneration === null) return doc.getValue() === '';
    else return doc.isClean(window.savedGeneration);
}

function setCode(code, history, name, autostart) {
    window.openProjectName = name;

    const doc = codeworldEditor.getDoc();
    doc.setValue(code);
    window.savedGeneration = doc.changeGeneration(true);

    if (history) {
        doc.setHistory(history);
    } else {
        doc.clearHistory();
    }

    codeworldEditor.focus();
    parseSymbolsFromCurrentCode();
    if (autostart) {
        compile();
    } else {
        stop();
    }
}

function loadSample(code) {
    if (isEditorClean()) sweetAlert.close();
    warnIfUnsaved(() => {
        setCode(code);
    }, false);
}

function newProject() {
    warnIfUnsaved(() => {
        setCode('');
    }, false);
}

function newFolder() {
    createFolder(window.nestedDirs.slice(1).join('/'), window.buildMode, () => {
        if (!window.move) {
            setCode('');
        }
    });
}

function loadProject(name, index) {
    if (window.move) {
        return;
    }

    loadProject_(index, name, window.buildMode, project => {
        setCode(project.source, project.history, name);
    });
}

function formatSource() {
    if (window.buildMode === 'codeworld') {
        // Unfortunately, there isn't an acceptable style for CodeWorld yet.
        return;
    }

    const src = window.codeworldEditor.getValue();
    const data = new FormData();
    data.append('source', src);
    data.append('mode', window.buildMode);

    sendHttp('POST', 'indent', data, request => {
        if (request.status === 200) {
            codeworldEditor.getDoc().setValue(request.responseText);
        }
    });
}

function stop() {
    if (document.getElementById('runner').contentWindow.debugActive) {
        document.getElementById('runner').contentWindow.stopDebugMode();
    }
    destroyTreeDialog();
    window.cancelCompile();

    run('', '', '', false, null);
}

function run(hash, dhash, msg, error, generation) {
    window.runningGeneration = generation;
    window.lastRunMessage = msg;

    const runner = document.getElementById('runner');

    // Stop canvas recording if the recorder is active
    if (canvasRecorder && canvasRecorder.recorder.state === 'recording') {
        stopRecording();
    }

    if (hash) {
        window.location.hash = `#${hash}`;
        document.getElementById('shareButton').style.display = '';
    } else {
        window.location.hash = '';
        document.getElementById('shareButton').style.display = 'none';
    }

    if (dhash) {
        const loc = `run.html?dhash=${dhash}&mode=${window.buildMode}`;
        runner.contentWindow.location.replace(loc);
        if (Boolean(navigator.mediaDevices) && Boolean(navigator.mediaDevices.getUserMedia)) {
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

    clearMessages();

    parseCompileErrors(msg).forEach(
        cmError => {
            printMessage(cmError.severity, cmError.fullText);
        }
    );

    if (error) markFailed();

    window.deployHash = dhash;

    updateUI();
    document.getElementById('runner').addEventListener('load', updateUI);
}

function notifyProgramStarted() {
    if (window.lastRunMessage) {
        const msg = window.lastRunMessage;
        window.lastRunMessage = null;
        setTimeout(() => {
            showRequiredChecksInDialog(msg);
        }, 500);
    }
}

function showRequiredChecksInDialog(msg) {
    const outputDiv = document.getElementById('message');
    if (outputDiv.classList.contains('error')) return;
    const matches = msg.match(
        /:: REQUIREMENTS ::((?:.|[\r\n])*):: END REQUIREMENTS ::/);
    if (!matches) {
        return;
    }
    const reqs = matches[1].split(/[\r\n]+/);
    const items = [];
    for (let i = 0; i < reqs.length; ++i) {
        const req = reqs[i];
        if (!req) continue;
        const bullet = req.slice(0, 4).toUpperCase();
        const rest = req.slice(4);
        if (bullet === '[Y] ') {
            // Successful requirement
            items.push([true, htmlEscapeString(rest)]);
        } else if (bullet === '[N] ') {
            // Unsuccessful requirement
            items.push([false, htmlEscapeString(rest)]);
        } else if (bullet === '[?] ') {
            // Indeterminate (usually a parse error in the requirement)
            items.push([undefined, htmlEscapeString(rest)]);
        } else if (items.length > 0) {
            // Detail message for the previous requirement.
            items[items.length - 1].push(req);
        }
    }
    const itemsHtml = items.map(item => {
        const head = item[1];
        const rest = item.slice(2).join('<br>');
        const details = rest ? `<br><span class="req-details">${rest 
        }</span>` : '';
        const itemclass = (item[0] === undefined) ? 'req-indet' : (item[0] ?
            'req-yes' : 'req-no');
        return `<li class="${itemclass}">${head}${details 
        }</li>`;
    });
    sweetAlert({
        title: Alert.title('Requirements'),
        html: `<ul class="req-list">${itemsHtml.join('')}</ul>`,
        confirmButtonText: 'Dismiss',
        showCancelButton: false,
        closeOnConfirm: true
    }).then(() => {
        const runner = document.getElementById('runner');
        if (!runner) return;
        if (runner.style.display === 'none') return;

        setTimeout(() => {
            runner.focus();
            runner.contentWindow.focus();
        }, 0);
    });
}

const htmlEscapeString = (() => {
    const el = document.createElement('div');
    return str => {
        el.textContent = str;
        return el.innerHTML;
    };
})();

function goto(line, col) {
    codeworldEditor.getDoc().setCursor(line - 1, col - 1);
    codeworldEditor.scrollIntoView(null, 100);
    codeworldEditor.focus();
}

function compile() {
    stop();

    const src = window.codeworldEditor.getValue();
    const compileGeneration = window.codeworldEditor.getDoc().changeGeneration(
        true);

    const data = new FormData();
    data.append('source', src);
    data.append('mode', window.buildMode);

    let compileFinished = false;

    window.cancelCompile = () => {
        compileFinished = true;
        window.cancelCompile = () => {};
    };

    sweetAlert({
        title: Alert.title('Compiling'),
        text: 'Your code is compiling.  Please wait...',
        onOpen: () => {
           sweetAlert.showLoading();
           sweetAlert.getCancelButton().disabled = false;
        },
        showConfirmButton: false,
        showCancelButton: true,
        showCloseButton: false,
        allowOutsideClick: false,
        allowEscapeKey: false,
        allowEnterKey: false
    }).then(() => {
        window.cancelCompile();
    });

    sendHttp('POST', 'compile', data, request => {
        if (compileFinished) return;
        sweetAlert.close();
        window.cancelCompile();

        const success = request.status === 200;

        let hash;
        let dhash;
        if (request.responseText.length === 23) {
            hash = request.responseText;
            dhash = null;
        } else {
            try {
                const obj = JSON.parse(request.responseText);
                hash = obj.hash;
                dhash = obj.dhash;
            } catch (e) {
                run('', '',
                    'Sorry!  Your program couldn\'t be run right now.',
                    true, null);
                return;
            }
        }

        const data = new FormData();
        data.append('hash', hash);
        data.append('mode', window.buildMode);

        sendHttp('POST', 'runMsg', data, request => {
            let msg = '';
            if (request.status === 200) {
                msg = request.responseText.replace(
                    /^[\r\n]+|[\r\n]+$/g, '');
            } else if (request.status >= 400) {
                msg =
                    'Sorry!  Your program couldn\'t be run right now.';
            }
            if (msg !== '') msg += '\n\n';

            if (success) {
                run(hash, dhash, msg, false, compileGeneration);
            } else {
                run(hash, '', msg, true, compileGeneration);
            }
        });
    });
}

let isFirstSignin = true;

function signinCallback(result) {
    discoverProjects('', 0);
    cancelMove();
    updateUI();
    if (isFirstSignin && !signedIn() && autohelpEnabled) {
        help();
    }
    isFirstSignin = false;
}

function discoverProjects(path, index) {
    discoverProjects_(path, window.buildMode, index);
}

function saveProjectBase(path, projectName) {
    saveProjectBase_(path, projectName, window.buildMode, () => {
        window.openProjectName = projectName;
        const doc = window.codeworldEditor.getDoc();
        window.savedGeneration = doc.changeGeneration(true);
        window.codeworldEditor.focus();
    });
}

function deleteFolder() {
    const path = window.nestedDirs.slice(1).join('/');
    if (path === '' || window.openProjectName) {
        return;
    }

    deleteFolder_(path, window.buildMode, () => {
        window.savedGeneration = codeworldEditor.getDoc().changeGeneration(true);
        setCode('');
    });
}

function deleteProject() {
    if (!window.openProjectName) {
        deleteFolder();
        return;
    }

    const path = window.nestedDirs.slice(1).join('/');
    deleteProject_(path, window.buildMode, () => {
        window.savedGeneration = codeworldEditor.getDoc().changeGeneration(true);
        setCode('');
    });
}

function shareFolder() {
    shareFolder_(window.buildMode);
}

function downloadProject() {
    const blob = new Blob(
        [window.codeworldEditor.getDoc().getValue()], {
            type: 'text/plain',
            endings: 'native'
        });
    let filename = 'untitled.hs';
    if (window.openProjectName) filename = `${window.openProjectName}.hs`;

    if (window.navigator.msSaveBlob) {
        window.navigator.msSaveBlob(blob, filename);
    } else {
        const elem = window.document.createElement('a');
        elem.href = window.URL.createObjectURL(blob);
        elem.download = filename;
        document.body.appendChild(elem);
        elem.click();
        document.body.removeChild(elem);
    }
}

function parseCompileErrors(rawErrors) {
    const errors = [];
    rawErrors = rawErrors.split('\n\n');
    rawErrors.forEach(err => {
        const lines = err.trim().split('\n');
        const firstLine = lines[0].trim();
        const otherLines = lines.slice(1).map(ln => ln.trim()).join('\n');
        const re1 = /^program\.hs:(\d+):((\d+)-?(\d+)?): (\w+):(.*)/;
        const re2 =
            /^program\.hs:\((\d+),(\d+)\)-\((\d+),(\d+)\): (\w+):(.*)/;

        if (err.trim() === '') {
            // Ignore empty messages.
        } else if (re1.test(firstLine)) {
            const match = re1.exec(firstLine);

            const line = Number(match[1]) - 1;
            let startCol = Number(match[3]) - 1;
            let endCol;
            if (match[4]) {
                endCol = Number(match[4]) - 1;
            } else {
                const token = window.codeworldEditor.getLineTokens(line).find(
                    t => t.start === startCol);
                if (token) {
                    endCol = token.end;
                } else if (startCol >= window.codeworldEditor.getDoc().getLine(
                    line).length) {
                    endCol = startCol;
                    --startCol;
                } else {
                    endCol = startCol + 1;
                }
            }

            const message =
                ((match[6] ? `${match[6].trim()}\n` : '') + otherLines)
                    .replace(/program\.hs:(\d+):((\d+)(-\d+)?)/g,
                        'Line $1, Column $2')
                    .replace(/program\.hs:\((\d+),(\d+)\)-\((\d+),(\d+)\)/g,
                        'Line $1-$3, Column $2-$4');

            errors.push({
                from: CodeMirror.Pos(line, startCol),
                to: CodeMirror.Pos(line, endCol),
                severity: match[5],
                fullText: err,
                message: message
            });
        } else if (re2.test(firstLine)) {
            const match = re2.exec(firstLine);

            const startLine = Number(match[1]) - 1;
            const startCol = Number(match[2]) - 1;
            const endLine = Number(match[3]) - 1;
            const endCol = Number(match[4]) - 1;

            errors.push({
                from: CodeMirror.Pos(startLine, startCol),
                to: CodeMirror.Pos(endLine, endCol),
                severity: match[5],
                fullText: err,
                message: (match[6] ? `${match[6].trim()}\n` :
                    '') + otherLines
            });
        } else {
            console.log('Can not parse error header:', firstLine);
        }
    });
    return errors;
}
