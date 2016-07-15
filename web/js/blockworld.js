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

function loadWorkspace(text)
{
  var workspace = Blockly.mainWorkspace;
  workspace.clear();
  var xmldom = Blockly.Xml.textToDom(text);
  Blockly.Xml.domToWorkspace(xmldom, workspace);
  lastXML = text;
}

function loadXmlHash(hash)
{
   sendHttp('GET', 'loadXML?hash=' + hash + '&mode=blocklyXML', null, function(request) {
     if (request.status == 200) {
          loadWorkspace(request.responseText);
     }
    });
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
        loadXmlHash(hash);

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

function usingHaskellPrelude() {
  return false;
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

function run(xmlHash, codeHash, msg, error) {
    var hash = codeHash;
    window.showingResult = hash || msg;

    if (window.showingResult) {
        window.showingDoc = false;
    }

    if (hash) {
        window.location.hash = '#' + xmlHash;
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
    document.getElementById('editButton').setAttribute('href','/#' + codeHash);
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
    run('', '', 'Building...', false);

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
            var codeHash = request.responseText;

            var data = new FormData();
            data.append('hash', codeHash);
            data.append('mode', window.buildMode);

            sendHttp('POST', 'runMsg', data, function(request) {
                var msg = '';
                if (request.status == 200) {
                    msg = request.responseText;
                } else if (request.status == 404) {
                    msg = "Sorry!  Your program couldn't be run right now.  Please try again.";
                }

                if (success) {
                    run(xmlHash, codeHash, 'Running...\n\n' + msg, false);
                } else {
                    run(xmlHash, codeHash, msg, true);
                }
            });
        });
    });
}

function updateUI()
{

  var isSignedIn = signedIn();
  // Update the user interface

  if (isSignedIn) {
        document.getElementById('signin').style.display = 'none';
        document.getElementById('navbarsignin').style.display = '';
        document.getElementById('signout').style.display = '';
        document.getElementById('saveAsButton').style.display = '';

        if (window.openProjectName) {
            document.getElementById('saveButton').style.display = '';
        } else {
            document.getElementById('saveButton').style.display = 'none';
        }
    } else {
        document.getElementById('navbarsignin').style.display = 'none';
        document.getElementById('signin').style.display = '';
        document.getElementById('signout').style.display = 'none';
        document.getElementById('saveButton').style.display = 'none';
        document.getElementById('saveAsButton').style.display = 'none';
    }

  if (window.showingResult) {
        // document.getElementById('result').style.display = '';
        
        if (window.programRunning) {
            document.getElementById('editButton').style.display = '';
        } else {
            document.getElementById('editButton').style.display = 'none';
        }
    } else {
        // document.getElementById('result').style.display = 'none';
        document.getElementById('editButton').style.display = 'none';
    }

  var projects = document.getElementById('projects');
  projects.innerHTML = '';
  allProjectNames.forEach(function(projectName) {
        var active = projectName == openProjectName;
        if (!isSignedIn && !active) {
            return;
        }

        var title = projectName;

        var encodedName = title.replace('&', '&amp;')
            .replace('<', '&lt;')
            .replace('>', '&gt;');

        var button = document.createElement('button');
        button.innerHTML = encodedName;
        if(projectName == openProjectName)
        {
          button.setAttribute('class', 'btn btn-primary');
          button.setAttribute('style','height:32px; width:100%;');
        }
        else
        {
          button.setAttribute('class','btn btn-default');
          button.setAttribute('style','height:32px; width:100%;');
        }
        button.onclick=function() {loadProject(projectName)};
        
        projects.appendChild(button);

    });

}

function signinCallback(result) {
    discoverProjects();
    updateUI();
    if(result.wc)
    {
      document.getElementById('username').innerHTML = result.wc.wc;
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
