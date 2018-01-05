/*
 * Copyright 2018 The CodeWorld Authors. All rights reserved.
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

// Helper functions /////////////////////////////////////////////////

function loadSample(code) {
    if (isEditorClean()) sweetAlert.close();
    warnIfUnsaved(function() {
        loadWorkspace(code);
    }, false);
}

function loadWorkspace(text)
{
  var workspace = Blockly.mainWorkspace;
  workspace.clear();
  var xmldom = Blockly.Xml.textToDom(text);
  Blockly.Xml.domToWorkspace(xmldom, workspace);
  lastXML = text;
}

function loadXmlHash(hash, autostart)
{
   sendHttp('GET', 'loadXML?hash=' + hash + '&mode=blocklyXML', null, function(request) {
     if (request.status == 200) {
          loadWorkspace(request.responseText);
          if(autostart){
            if(runFunc) runFunc();
          }
     }
    });
}

// This will get bound in Haskell to a function that runs the program
runFunc = null;

openProjectName = '';
lastXML = '';
function init()
{
    nestedDirs = [""];
    allProjectNames = [[]];
    allFolderNames = [[]];
    lastXML = null;
    showingResult = false;
    window.buildMode = 'codeworld';

    var hash = location.hash.slice(1);
    if (hash.length > 0) {
        if (hash.slice(-2) == '==') {
            hash = hash.slice(0, -2);
        }
        if (hash[0] == 'F') {
            function go(folderName) {
                var id_token = auth2.currentUser.get().getAuthResponse().id_token;
                var data = new FormData();
                data.append('id_token', id_token);
                data.append('mode', 'blocklyXML');
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
            loadXmlHash(hash,true);
        }
    } else {
        initCodeworld();
    }
}

function initCodeworld() {
    codeworldKeywords = {};
    registerStandardHints( function(){} );
    
    window.onbeforeunload = function(event) {
        if (containsUnsavedChanges()) {
            var msg = 'There are unsaved changes to your project. ' + 'If you continue, they will be lost!';
            if (event) event.returnValue = msg;
            return msg;
        }
    }
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
    var editor = document.getElementById('genCode');
    CodeMirror.runMode(code
      ,{
        name: 'codeworld',
        overrideKeywords: codeworldKeywords
      }
      ,editor);
}

function run(xmlHash, codeHash, msg, error, dhash) {
    var hash = codeHash;

    if (hash) {
        window.location.hash = '#' + xmlHash;
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

    var runner = document.getElementById('runner');
    if (hash && !error) {
        var loc = 'run.html?hash=' + hash + '&mode=' + window.buildMode;
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
    if(!hash && !error && !msg){ // We stopped, don't show message window
      document.getElementById('message').style.display = 'none';
    }

    
    if(msg){
      var message = document.getElementById('message');
      message.innerHTML = '';
      addToMessage(msg);

      if (error) {
          message.classList.add('error');
      } else {
          message.classList.remove('error');
      }
    }

    document.getElementById('editButton').setAttribute('href','/#' + codeHash);
    window.deployHash = dhash;
    cancelMove();
    updateUI();
}

function removeErrors()
{
    $('.blocklyDraggable').removeClass('blocklyErrorSelected');
    var blocks = Blockly.getMainWorkspace().getAllBlocks();

    blocks.forEach(function(block){
      block.removeErrorSelect();
    });
}

function getWorkspaceXMLText()
{
    var workspace = Blockly.getMainWorkspace();
    var xml = Blockly.Xml.workspaceToDom(workspace);
    var xml_text = Blockly.Xml.domToText(xml);
    return xml_text;
}

function containsUnsavedChanges()
{
  var blank = '<xml xmlns="http://www.w3.org/1999/xhtml"></xml>';
  return getWorkspaceXMLText() != (lastXML || blank);
}

function isEditorClean()
{
  return !containsUnsavedChanges();
}

function compile(src,silent) {
    run('', '', 'Compiling...', false);

    var xml_text = getWorkspaceXMLText();
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
                } else if (request.status == 404) {
                    msg = "Sorry!  Your program couldn't be run right now.  Please try again.";
                }
                if(silent) msg = null;

                if (success) {
                    run(xmlHash, hash, 'Running...\n\n' + msg, false, dhash);
                } else {
                    run(xmlHash, hash, msg, true);
                }
            });
        });
    });
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
            clearWorkspace();
            openProjectName = null;
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

    document.getElementById('newButton').style.display = '';
    document.getElementById('saveAsButton').style.display = '';
    document.getElementById('runButtons').style.display = '';

    updateNavBar();
    var NDlength = nestedDirs.length;

    if (NDlength != 1 && (openProjectName == null || openProjectName == '')) {
        document.getElementById('shareFolderButton').style.display = '';
    } else {
        document.getElementById('shareFolderButton').style.display = 'none';
    }

    document.getElementById('moveHereButton').style.display = 'none';
    document.getElementById('cancelMoveButton').style.display = 'none';
    if ((openProjectName != null && openProjectName != '') || NDlength != 1) {
        document.getElementById('moveButton').style.display = '';
    } else {
        document.getElementById('moveButton').style.display = 'none';
    }

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
    var projects = document.getElementById('nav_mine');

    while (projects.lastChild) {
        projects.removeChild(projects.lastChild);
    }

    allProjectNames.forEach(function(projectNames) {
        projectNames.sort(function(a, b) {
            a.localeCompare(b);
        });
    });

    allFolderNames.forEach(function(folderNames) {
        folderNames.sort(function(a, b) {
            a.localeCompare(b);
        });
    });

    var NDlength =  nestedDirs.length;
    for(let i = 0; i < NDlength; i++) {
        var tempProjects;
        if (i != 0) {
            var encodedName = nestedDirs[i].replace('&', '&amp;')
                .replace('<', '&lt;')
                .replace('>', '&gt;');
            var template = document.getElementById('openFolderTemplate').innerHTML;
            template = template.replace('{{label}}', encodedName);
            var span = document.createElement('span');
            span.innerHTML = template;
            var elem = span.getElementsByTagName('a')[0];
            elem.style.marginLeft = (3 + 16 * (i - 1)) + 'px';
            elem.onclick = function() {
                folderHandler(nestedDirs[i], i - 1, true);
            };
            span.style.display = 'flex';
            span.style.flexDirection = 'column';
            projects.parentNode.insertBefore(span, projects);
            projects.parentNode.removeChild(projects);
            projects = span.appendChild(document.createElement('div'));
        }
        allFolderNames[i].forEach(function(folderName) {
            var encodedName = folderName.replace('&', '&amp;')
                .replace('<', '&lt;')
                .replace('>', '&gt;');
            var template = document.getElementById('folderTemplate').innerHTML;
            template = template.replace('{{label}}', encodedName);
            var span = document.createElement('span');
            span.innerHTML = template;
            var elem = span.getElementsByTagName('a')[0];
            elem.style.marginLeft = (3 + 16 * i) + 'px';
            elem.onclick = function() {
                folderHandler(folderName, i, false);
            };
            span.style.display = 'flex';
            span.style.flexDirection = 'column';
            projects.appendChild(span);
            if (i < NDlength - 1) {
                if (folderName == nestedDirs[i + 1]) {
                    tempProjects = projects.lastChild;
                }
            }
        });
        allProjectNames[i].forEach(function(projectName) {
            var active = (window.openProjectName == projectName) && (i == NDlength - 1);
            if(!signedIn() && !active) {
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
            elem.style.marginLeft = (3 + 16 * i) + 'px';
            elem.onclick = function() {
                loadProject(projectName, i);
            }
            span.style.display = 'flex';
            span.style.flexDirection = 'column';
            projects.appendChild(span);
        });
        if ( i + 1 < NDlength ) {
            projects = tempProjects;
        }
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
        clearWorkspace();
        nestedDirs = [""];
        allProjectNames = [[]];
        allFolderNames = [[]];
        discoverProjects("", 0);
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
        if (tempOpen != null && tempOpen != '') {
            window.move.file = tempOpen;
        }
    }, false);
}

function moveHere() {
    function successFunc() {
        nestedDirs = [""];
        allProjectNames = [[]];
        allFolderNames = [[]];
        discoverProjects("", 0);
        cancelMove();
        updateUI();
    }

    moveHere_(nestedDirs.slice(1).join('/'), 'blocklyXML', successFunc);
}

function help(doc) {
    var url = 'doc.html?help/blocks.md';
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

function signinCallback(result) {
    discoverProjects("", 0);
    cancelMove();
    updateUI();
    if(result.wc)
    {
      // document.getElementById('username').innerHTML = result.wc.wc;
    }

}

function signOut() {
  
  // call shared sign out
  signout();

  document.getElementById('projects').innerHTML = '';
  openProjectName = null;
  cancelMove();
  updateUI();
}

function discoverProjects(path, index){
  discoverProjects_(path, 'blocklyXML', index);
}

function loadProject(name, index) {
  if (window.move) {
    return;
  }
  function successFunc(project){
    openProjectName = name;
    clearRunCode();
    loadWorkspace(project.source);
    cancelMove();
    updateUI();
    Blockly.getMainWorkspace().clearUndo();
  }
  loadProject_(index, name,'blocklyXML',successFunc);

}


function saveProjectBase(path, projectName) {
    function successFunc() {
      lastXML = getWorkspaceXMLText();
      window.openProjectName = projectName;
      cancelMove();
      updateUI();

      if (allProjectNames[allProjectNames.length -1].indexOf(projectName) == -1) {
        discoverProjects(path, allProjectNames.length - 1);
      }
    }
    saveProjectBase_(path, projectName, 'blocklyXML', successFunc);
}

function deleteFolder() {
    var path = nestedDirs.slice(1).join('/');
    if (path == "" || window.openProjectName != null) {
        return;
    }
    function successFunc() {
        clearWorkspace();
        openProjectName = null;
        Blockly.getMainWorkspace().clearUndo();
    }
    deleteFolder_(path, 'blocklyXML', successFunc);
}

function deleteProject() {
    if(!window.openProjectName) {
        deleteFolder();
        return;
    }
    function successFunc() {
        clearWorkspace();
        openProjectName = null;
        Blockly.getMainWorkspace().clearUndo();
    }
    var path = nestedDirs.slice(1).join('/');
    deleteProject_(path, 'blocklyXML', successFunc);

}

function newFolder() {
    function successFunc() {
        if (!window.move) {
            clearWorkspace();
            openProjectName = null;
            clearRunCode();
            lastXML = getWorkspaceXMLText();
            Blockly.getMainWorkspace().clearUndo();
            window.location.hash = '';
        }
    }
    createFolder(nestedDirs.slice(1).join('/'), 'blocklyXML', successFunc);
}

function shareFolder() {
    shareFolder_('blocklyXML');
}

function newProject() {
  warnIfUnsaved(function()
  {
    clearRunCode();
    clearWorkspace();
    openProjectName = null;
    cancelMove();
    updateUI();
    lastXML = getWorkspaceXMLText();
    Blockly.getMainWorkspace().clearUndo();
    window.location.hash = '';
  }, false);
}

// Clear the running iframe and generated code
function clearRunCode()
{
    var runner = document.getElementById('runner');
    runner.contentWindow.location.replace('about:blank');
    updateEditor('');
}

function clearWorkspace()
{
    var workspace = Blockly.mainWorkspace;
    workspace.clear();
}
