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
    });
}

function loadWorkspace(text) {
    const workspace = Blockly.mainWorkspace;
    workspace.clear();
    const xmldom = Blockly.Xml.textToDom(text);
    Blockly.Xml.domToWorkspace(xmldom, workspace);
    window.lastXML = text;
}

function loadXmlHash(hash, autostart) {
    sendHttp('GET', `loadXML?hash=${hash}&mode=${window.projectEnv}`, null, request => {
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
        initDirectoryTree();
        window.lastXML = null;
        window.showingResult = false;
        window.buildMode = 'codeworld';
        window.projectEnv = 'blocklyXML';

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
                    showCancelButton: false
                }).then(result => {
                    if (!result) {
                        return;
                    }

                    const data = new FormData();
                    data.append('mode', window.projectEnv);
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
                            discoverProjects('');
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
    updateUI();
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
    return [window.lastXML, blank].indexOf(getWorkspaceXMLText()) === -1;
}

function isEditorClean() {
    return !containsUnsavedChanges();
}

function compile(src, silent) {
    run('', '', 'Compiling...', false);

    const xml_text = getWorkspaceXMLText();
    const data = new FormData();
    data.append('source', xml_text);
    data.append('mode', window.projectEnv);

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

        if ($('#directoryTree').tree('getSelectedNode')) {
            document.getElementById('deleteButton').style.display = '';
        } else {
            document.getElementById('deleteButton').style.display = 'none';
        }
        if (window.openProjectName) {
            document.getElementById('saveButton').style.display = '';
        } else {
            document.getElementById('saveButton').style.display = 'none';
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

    const selected = $('#directoryTree').tree('getSelectedNode');

    if (selected && selected.type === 'directory') {
        document.getElementById('shareFolderButton').style.display = '';
    } else {
        document.getElementById('shareFolderButton').style.display = 'none';
    }

    let title;
    if (window.openProjectName) {
        title = window.openProjectName;
    } else {
        title = '(new)';
    }

    if (!isEditorClean()) {
        title = `* ${title}`;
        const selected = $('#directoryTree').tree('getSelectedNode');
        if (selected && selected.type === 'project') {
            const asterisk = selected.element.getElementsByClassName('unsaved-changes')[0];
            if (asterisk) {
                asterisk.style.display = '';
            }
        }
    } else {
        $('.unsaved-changes').css('display', 'none');
    }

    document.title = `${title} - CodeWorld`;
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

function signinCallback() {
    discoverProjects('');
}

function signOut() {

    // call shared sign out
    signout();

    document.getElementById('projects').innerHTML = '';
    window.openProjectName = null;
    updateUI();
}

function loadProject(name, index) {
    function successFunc(project) {
        window.openProjectName = name;
        clearRunCode();
        loadWorkspace(project.source);
        Blockly.getMainWorkspace().clearUndo();
    }
    loadProject_(index, name, window.projectEnv, successFunc);

}

function saveProject() {
    function successFunc() {
        window.lastXML = getWorkspaceXMLText();
    }

    if (window.openProjectName) {
        saveProjectBase(
            getNearestDirectory(),
            window.openProjectName,
            window.projectEnv,
            successFunc);
    } else {
        saveProjectAs();
    }
}

function saveProjectAs() {
    function successFunc(name) {
        window.lastXML = getWorkspaceXMLText();
        window.openProjectName = name;
    }
    saveProjectAsBase(successFunc);
}

function deleteFolder() {
    const path = getNearestDirectory();
    if (path === '' || window.openProjectName !== null) {
        return;
    }

    function successFunc() {
        clearWorkspace();
        Blockly.getMainWorkspace().clearUndo();
    }
    deleteFolder_(path, window.projectEnv, successFunc);
}

function deleteProject() {
    if (!window.openProjectName) {
        deleteFolder();
        return;
    }

    function successFunc() {
        clearWorkspace();
        Blockly.getMainWorkspace().clearUndo();
    }
    const path = getNearestDirectory();
    deleteProject_(path, window.projectEnv, successFunc);
}

function newFolder() {
    function successFunc() {
        clearRunCode();
        window.lastXML = getWorkspaceXMLText();
        Blockly.getMainWorkspace().clearUndo();
        window.location.hash = '';
    }
    createFolder(getNearestDirectory(), window.projectEnv, successFunc);
}

function shareFolder() {
    shareFolder_(window.projectEnv);
}

function newProject() {
    warnIfUnsaved(() => {
        clearRunCode();
        clearWorkspace();
        updateUI();
        window.lastXML = getWorkspaceXMLText();
        Blockly.getMainWorkspace().clearUndo();
        window.location.hash = '';
    });
}

// Clear the running iframe and generated code
function clearRunCode() {
    const runner = document.getElementById('runner');
    runner.contentWindow.location.replace('about:blank');
    updateEditor('');
}

function clearWorkspace() {
    window.openProjectName = null;
    const workspace = Blockly.mainWorkspace;
    // Deselect nodes
    const treeState = $('#directoryTree').tree('getState');
    treeState.selected_node = [];
    $('#directoryTree').tree('setState', treeState);
    workspace.clear();
}

function clearCode() {
    window.openProjectName = null;
    const workspace = Blockly.mainWorkspace;
    workspace.clear();
}
