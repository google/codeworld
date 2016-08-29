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

// Helper functions /////////////////////////////////////////////////

function loadSample(code) {
    if (isEditorClean()) sweetAlert.close();
    warnIfUnsaved(function() {
        loadWorkspace(code);
    });
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

function getHash(){
  var hash = location.hash.slice(1);
  if (hash.length > 0) {
    if (hash.slice(-2) == '==') {
        hash = hash.slice(0, -2);
    }
    return hash;
  }
  else return '';
}

openProjectName = '';
lastXML = '';
function init()
{
    allProjectNames = [];
    lastXML = null;
    showingResult = false;
    window.buildMode = 'codeworld';

    var hash = location.hash.slice(1);
    if (hash.length > 0) {
        if (hash.slice(-2) == '==') {
            hash = hash.slice(0, -2);
        }
        loadXmlHash(hash,true);
    } 
    
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
    } else {
        window.location.hash = '';
        document.getElementById('shareButton').style.display = 'none';
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
    if(!hash){ // We stopped, don't show message window
      document.getElementById('message').style.display = 'none';
    }


    var message = document.getElementById('message');
    message.innerHTML = '';
    addToMessage(msg);

    if (error) {
        message.classList.add('error');
    } else {
        message.classList.remove('error');
    }

    document.getElementById('editButton').setAttribute('href','/#' + codeHash);
    window.deployHash = dhash;
    updateUI();
}

function removeErrors()
{
    $('.blocklyDraggable').removeClass('blocklyErrorSelected');
    var blocks = Blockly.getMainWorkspace().getAllBlocks();

    blocks.forEach(function(block){
      block.removeErrorSelect();
      block.setWarningText(null);
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

function compile(src) {
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

                if (success) {
                    run(xmlHash, hash, 'Running...\n\n' + msg, false, dhash);
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
    discoverProjects();
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
  updateUI();
}

function discoverProjects(){
  discoverProjects_('blocklyXML');
}

function loadProject(name) {

  function successFunc(project){
    openProjectName = name;
    clearRunCode();
    loadWorkspace(project.source);
    updateUI();
    Blockly.getMainWorkspace().clearUndo();
  }
  loadProject_(name,'blocklyXML',successFunc);

}


function saveProjectBase(projectName) {
    function successFunc() {
      lastXML = getWorkspaceXMLText();
      window.openProjectName = projectName;
      updateUI();

      if (allProjectNames.indexOf(projectName) == -1) {
        discoverProjects();
      }
    }
    saveProjectBase_(projectName, 'blocklyXML', successFunc);
}

function deleteProject() {

  function successFunc()
  {
    clearWorkspace();
    openProjectName = null;
    Blockly.getMainWorkspace().clearUndo();
  }
  deleteProject_('blocklyXML', successFunc);

}

function newProject() {
  warnIfUnsaved(function()
  {
    clearRunCode();
    clearWorkspace();
    openProjectName = null;
    discoverProjects();
    updateUI();
    lastXML = getWorkspaceXMLText();
    Blockly.getMainWorkspace().clearUndo();
    window.location.hash = '';
  });
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
