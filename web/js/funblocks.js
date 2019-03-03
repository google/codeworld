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

window.openProjectName = '';
window.lastXML = '';

// This will get bound in Haskell to a function that runs the program
window.runFunc = null;

// Helper functions /////////////////////////////////////////////////

function loadSample(code) {
    if (isEditorClean()) sweetAlert.close();
    warnIfUnsaved(() => {
        loadWorkspace(code);
    }, false);
}

function loadWorkspace(text) {
    const workspace = Blockly.mainWorkspace;
    workspace.clear();
    const xmldom = Blockly.Xml.textToDom(text);
    Blockly.Xml.domToWorkspace(xmldom, workspace);
    window.lastXML = text;
}

function loadXmlHash(hash, autostart) {
    sendHttp('GET', `loadXML?hash=${hash}&mode=blocklyXML`, null, request => {
        if (request.status === 200) {
            loadWorkspace(request.responseText);
            if (autostart) {
                if (runFunc) runFunc();
            }
        }
    });
}

function init() {
    Alert.init().then(Auth.init).then(() => {
        window.nestedDirs = [''];
        window.allProjectNames = [
            []
        ];
        window.allFolderNames = [
            []
        ];
        window.lastXML = null;
        window.showingResult = false;
        window.buildMode = 'codeworld';

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
                    data.append('mode', 'blocklyXML');
                    data.append('shash', hash);
                    data.append('name', result.value);

                    sendHttp('POST', 'shareContent', data,
                        request => {
                            window.location.hash = '';
                            if (request.status === 200) {
                                sweetAlert('Success!',
                                    'The shared folder is moved into your root directory.',
                                    'success');
                            } else {
                                sweetAlert('Oops!',
                                    'Could not load the shared directory. Please try again.',
                                    'error');
                            }
                            initCodeworld();
                            discoverProjects('', 0);
                            updateUI();
                        });
                });
            } else {
                initCodeworld();
                loadXmlHash(hash, true);
            }
        } else {
            initCodeworld();
        }
    });
}

function initCodeworld() {
    window.codeworldKeywords = {};
    registerStandardHints(() => {});

    window.onbeforeunload = event => {
        if (containsUnsavedChanges()) {
            const msg = 'There are unsaved changes to your project. ' +
                'If you continue, they will be lost!';
            if (event) event.returnValue = msg;
            return msg;
        }
    };
}

function getCurrentProject() {
    return {
        'name': window.openProjectName || 'Untitled',
        'source': getWorkspaceXMLText(),
        'history': ''
    };
}

// Sets the generated code
function updateEditor(code) {
    const editor = document.getElementById('genCode');
    CodeMirror.runMode(code, {
        name: 'codeworld',
        overrideKeywords: codeworldKeywords
    }, editor);
}

function run(xmlHash, codeHash, msg, error, dhash) {
    const hash = codeHash;

    if (hash) {
        window.location.hash = `#${xmlHash}`;
        document.getElementById('shareButton').style.display = '';
        document.getElementById('shareFolderButton').style.display = 'none';
    } else {
        window.location.hash = '';
        document.getElementById('shareButton').style.display = 'none';
        document.getElementById('shareFolderButton').style.display = '';
    }

    window.showingResult = hash || msg;

    if (window.showingResult) {
        window.showingDoc = false;
    }

    const runner = document.getElementById('runner');
    if (hash && !error) {
        const loc = `run.html?hash=${hash}&mode=${window.buildMode}`;
        runner.contentWindow.location.replace(loc);
        document.getElementById('runner').style.display = '';
        document.getElementById('runner').contentWindow.focus();
        document.getElementById('message').style.display = 'none';
        window.programRunning = true;
    } else {
        document.getElementById('message').style.display = '';
        runner.contentWindow.location.replace('about:blank');
        document.getElementById('runner').style.display = 'none';
        window.programRunning = false;
    }
    if (!hash && !error && !msg) { // We stopped, don't show message window
        document.getElementById('message').style.display = 'none';
    }

    if (msg) {
        const message = document.getElementById('message');
        message.innerHTML = '';

        printMessage(error ? 'error' : 'log', msg);
        if (error) markFailed();
    }

    document.getElementById('editButton').setAttribute('href', `/#${codeHash}`);
    window.deployHash = dhash;
    cancelMove();
    updateUI();
}

function removeErrors() {
    $('.blocklyDraggable').removeClass('blocklyErrorSelected');
    const blocks = Blockly.getMainWorkspace().getAllBlocks();

    blocks.forEach(block => {
        block.removeErrorSelect();
    });
}

function getWorkspaceXMLText() {
    const workspace = Blockly.getMainWorkspace();
    const xml = Blockly.Xml.workspaceToDom(workspace);
    const xml_text = Blockly.Xml.domToText(xml);
    return xml_text;
}

function containsUnsavedChanges() {
    const blank = '<xml xmlns="http://www.w3.org/1999/xhtml"></xml>';
    return getWorkspaceXMLText() !== (window.lastXML || blank);
}

function isEditorClean() {
    return !containsUnsavedChanges();
}

function compile(src, silent) {
    run('', '', 'Compiling...', false);

    const xml_text = getWorkspaceXMLText();
    const data = new FormData();
    data.append('source', xml_text);
    data.append('mode', 'blocklyXML');

    sendHttp('POST', 'saveXMLhash', data, request => {
        // XML Hash
        const xmlHash = request.responseText;

        const data = new FormData();
        data.append('source', src);
        data.append('mode', window.buildMode);

        sendHttp('POST', 'compile', data, request => {
            const success = request.status === 200;

            // Code hash
            let hash;
            let dhash;
            if (request.responseText.length === 23) {
                hash = request.responseText;
                dhash = null;
            } else {
                const obj = JSON.parse(request.responseText);
                hash = obj.hash;
                dhash = obj.dhash;
            }

            const data = new FormData();
            data.append('hash', hash);
            data.append('mode', window.buildMode);

            sendHttp('POST', 'runMsg', data, request => {
                let msg = '';
                if (request.status === 200) {
                    msg = request.responseText.trim();
                } else if (request.status === 404) {
                    msg =
                        'Sorry!  Your program couldn\'t be run right now.  Please try again.';
                }
                if (msg !== '') msg += '\n\n';
                if (silent) msg = null;

                if (success) {
                    run(xmlHash, hash, msg, false,
                        dhash);
                } else {
                    run(xmlHash, hash, msg, true);
                }
            });
        });
    });
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
            clearWorkspace();
            window.openProjectName = null;
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

    document.getElementById('newButton').style.display = '';
    document.getElementById('saveAsButton').style.display = '';
    document.getElementById('runButtons').style.display = '';

    updateNavBar();
    const NDlength = window.nestedDirs.length;

    if (NDlength !== 1 && (openProjectName === null || openProjectName === '')) {
        document.getElementById('shareFolderButton').style.display = '';
    } else {
        document.getElementById('shareFolderButton').style.display = 'none';
    }

    document.getElementById('moveHereButton').style.display = 'none';
    document.getElementById('cancelMoveButton').style.display = 'none';
    if ((openProjectName !== null && openProjectName !== '') || NDlength !== 1) {
        document.getElementById('moveButton').style.display = '';
    } else {
        document.getElementById('moveButton').style.display = 'none';
    }

    let title;
    if (window.openProjectName) {
        title = window.openProjectName;
    } else {
        title = '(new)';
    }

    if (!isEditorClean()) {
        title = `* ${title}`;
    }

    document.title = `${title} - CodeWorld`;
}

function updateNavBar() {
    let projects = document.getElementById('nav_mine');

    while (projects.lastChild) {
        projects.removeChild(projects.lastChild);
    }

    window.allProjectNames.forEach(projectNames => {
        projectNames.sort((a, b) => {
            a.localeCompare(b);
        });
    });

    window.allFolderNames.forEach(folderNames => {
        folderNames.sort((a, b) => {
            a.localeCompare(b);
        });
    });

    const NDlength = window.nestedDirs.length;
    for (let i = 0; i < NDlength; i++) {
        let tempProjects;
        if (i !== 0) {
            const encodedName = window.nestedDirs[i].replace(/&/g, '&amp;')
                .replace(/</g, '&lt;')
                .replace(/>/g, '&gt;');
            let template = document.getElementById('openFolderTemplate').innerHTML;
            template = template.replace(/{{label}}/g, encodedName);
            const span = document.createElement('span');
            span.innerHTML = template;
            const elem = span.getElementsByTagName('a')[0];
            elem.style.marginLeft = `${3 + 16 * (i - 1)}px`;
            elem.onclick = () => {
                folderHandler(window.nestedDirs[i], i - 1, true);
            };
            span.style.display = 'flex';
            span.style.flexDirection = 'column';
            projects.parentNode.insertBefore(span, projects);
            projects.parentNode.removeChild(projects);
            projects = span.appendChild(document.createElement('div'));
        }
        window.allFolderNames[i].forEach(folderName => {
            const encodedName = folderName.replace(/&/g, '&amp;')
                .replace(/</g, '&lt;')
                .replace(/>/g, '&gt;');
            let template = document.getElementById('folderTemplate').innerHTML;
            template = template.replace(/{{label}}/g, encodedName);
            const span = document.createElement('span');
            span.innerHTML = template;
            const elem = span.getElementsByTagName('a')[0];
            elem.style.marginLeft = `${3 + 16 * i}px`;
            elem.onclick = () => {
                folderHandler(folderName, i, false);
            };
            span.style.display = 'flex';
            span.style.flexDirection = 'column';
            projects.appendChild(span);
            if (i < NDlength - 1) {
                if (folderName === window.nestedDirs[i + 1]) {
                    tempProjects = projects.lastChild;
                }
            }
        });
        window.allProjectNames[i].forEach(projectName => {
            const active = (window.openProjectName === projectName) && (i === NDlength - 1);
            if (!signedIn() && !active) {
                return;
            }

            let title = projectName;
            if (active && !isEditorClean()) {
                title = `* ${title}`;
            }
            const encodedName = title.replace(/&/g, '&amp;')
                .replace(/</g, '&lt;')
                .replace(/>/g, '&gt;');
            let template = document.getElementById('projectTemplate').innerHTML;
            template = template.replace(/{{label}}/g, encodedName);
            template = template.replace(/{{ifactive ([^}]*)}}/g, active ?
                '$1' : '');
            const span = document.createElement('span');
            span.innerHTML = template;
            const elem = span.getElementsByTagName('a')[0];
            elem.style.marginLeft = `${3 + 16 * i}px`;
            elem.onclick = () => {
                loadProject(projectName, i);
            };
            span.style.display = 'flex';
            span.style.flexDirection = 'column';
            projects.appendChild(span);
        });
        if (i + 1 < NDlength) {
            projects = tempProjects;
        }
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

        if ((openProjectName === null || openProjectName === '') &&
            window.nestedDirs.length === 1) {
            sweetAlert('Oops!',
                'You must select a project or folder to move.',
                'error');
            updateUI();
            return;
        }

        const tempOpen = openProjectName;
        const tempPath = window.nestedDirs.slice(1).join('/');
        clearWorkspace();
        window.nestedDirs = [''];
        window.allProjectNames = [
            []
        ];
        window.allFolderNames = [
            []
        ];
        discoverProjects('', 0);
        document.getElementById('newFolderButton').style.display = '';
        document.getElementById('newButton').style.display = 'none';
        document.getElementById('saveButton').style.display = 'none';
        document.getElementById('saveAsButton').style.display = 'none';
        document.getElementById('deleteButton').style.display = 'none';
        document.getElementById('moveButton').style.display = 'none';
        document.getElementById('moveHereButton').style.display = '';
        document.getElementById('cancelMoveButton').style.display = '';
        document.getElementById('runButtons').style.display = 'none';

        window.move = Object();
        window.move.path = tempPath;
        if (tempOpen !== null && tempOpen !== '') {
            window.move.file = tempOpen;
        }
    }, false);
}

function moveHere() {
    function successFunc() {
        window.nestedDirs = [''];
        window.allProjectNames = [
            []
        ];
        window.allFolderNames = [
            []
        ];
        discoverProjects('', 0);
        cancelMove();
        updateUI();
    }

    moveHere_(window.nestedDirs.slice(1).join('/'), 'blocklyXML', successFunc);
}

function help() {
    const url = 'doc.html?shelf=help/blocks.shelf';
    sweetAlert({
        html: `<iframe id="doc" style="width: 100%; height: 100%" class="dropbox" src="${ 
            url}"></iframe>`,
        customClass: 'helpdoc',
        allowEscapeKey: true,
        allowOutsideClick: true,
        showConfirmButton: false,
    });
}

function signinCallback(result) {
    discoverProjects('', 0);
    cancelMove();
    updateUI();
    if (result.wc) {
        // document.getElementById('username').innerHTML = result.wc.wc;
    }

}

function signOut() {

    // call shared sign out
    signout();

    document.getElementById('projects').innerHTML = '';
    window.openProjectName = null;
    cancelMove();
    updateUI();
}

function discoverProjects(path, index) {
    discoverProjects_(path, 'blocklyXML', index);
}

function loadProject(name, index) {
    if (window.move) {
        return;
    }

    function successFunc(project) {
        window.openProjectName = name;
        clearRunCode();
        loadWorkspace(project.source);
        cancelMove();
        updateUI();
        Blockly.getMainWorkspace().clearUndo();
    }
    loadProject_(index, name, 'blocklyXML', successFunc);

}

function saveProjectBase(path, projectName) {
    function successFunc() {
        window.lastXML = getWorkspaceXMLText();
        window.openProjectName = projectName;
        cancelMove();

        if (window.allProjectNames[window.allProjectNames.length - 1].indexOf(projectName) === -1) {
            discoverProjects(path, window.allProjectNames.length - 1);
        }
        updateUI();
    }
    saveProjectBase_(path, projectName, 'blocklyXML', successFunc);
}

function deleteFolder() {
    const path = window.nestedDirs.slice(1).join('/');
    if (path === '' || window.openProjectName !== null) {
        return;
    }

    function successFunc() {
        clearWorkspace();
        window.openProjectName = null;
        Blockly.getMainWorkspace().clearUndo();
    }
    deleteFolder_(path, 'blocklyXML', successFunc);
}

function deleteProject() {
    if (!window.openProjectName) {
        deleteFolder();
        return;
    }

    function successFunc() {
        clearWorkspace();
        window.openProjectName = null;
        Blockly.getMainWorkspace().clearUndo();
    }
    const path = window.nestedDirs.slice(1).join('/');
    deleteProject_(path, 'blocklyXML', successFunc);

}

function newFolder() {
    function successFunc() {
        if (!window.move) {
            clearWorkspace();
            window.openProjectName = null;
            clearRunCode();
            window.lastXML = getWorkspaceXMLText();
            Blockly.getMainWorkspace().clearUndo();
            window.location.hash = '';
        }
    }
    createFolder(window.nestedDirs.slice(1).join('/'), 'blocklyXML', successFunc);
}

function shareFolder() {
    shareFolder_('blocklyXML');
}

function newProject() {
    warnIfUnsaved(() => {
        clearRunCode();
        clearWorkspace();
        window.openProjectName = null;
        cancelMove();
        updateUI();
        window.lastXML = getWorkspaceXMLText();
        Blockly.getMainWorkspace().clearUndo();
        window.location.hash = '';
    }, false);
}

// Clear the running iframe and generated code
function clearRunCode() {
    const runner = document.getElementById('runner');
    runner.contentWindow.location.replace('about:blank');
    updateEditor('');
}

function clearWorkspace() {
    const workspace = Blockly.mainWorkspace;
    workspace.clear();
}
