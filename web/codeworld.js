/*
 * Copyright 2015 Google Inc. All rights reserved.
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

/*
 * Initializes the programming environment.  This is called after the
 * entire document body and other JavaScript has loaded.
 */
function init() {
  showingBrowse = true;
  showingDoc = false;
  showingResult = false;
  allProjectNames = [];

  usingHaskellPrelude = false;

  var editor = document.getElementById('editor');

  window.codeworldEditor = CodeMirror.fromTextArea(editor, {
    mode: { name: 'codeworld', overrideKeywords: {} },
    lineNumbers: true,
    autofocus: true,
    matchBrackets: true,
    styleActiveLine: true,
    showTrailingSpace: true,
    indentWithTabs: false,
    autoClearEmptyLines: true,
    rulers: [{column: 80, color: "#bbb", lineStyle: "dashed"}],
    extraKeys: { "Ctrl-Space": "autocomplete",
                 "Tab"       : "indentMore",
                 "Shift-Tab" : "indentLess",
                 "Ctrl-Enter": compile }
  });

  CodeMirror.commands.save = function(cm) { saveProject(); }
  document.onkeydown = function(e) {
    if (e.ctrlKey && e.keyCode === 83) {
      saveProject();
      return false;
    }
  };

  window.codeworldEditor.on('changes', window.updateUI);

  var hints = [
    "main", "--", "{-", "-}", "::", "->", "<-", "..", "case", "of", "if",
    "then", "else", "data", "let", "in", "where", "type"
  ];
  hints.sort();

  var hintBlacklist = [
    // Symbols that only exist to implement RebindableSyntax or map to
    // built-in Haskell types.
    "IO",
    "fromDouble",
    "fromInt",
    "fromInteger",
    "fromRational",
    "fromString",
    "ifThenElse",
    "negate",
    "toDouble",
    "toInt",

    // Deprecated symbols from the Prelude.  Right now, we can't detect
    // these automatically
    "addVectors",
    "color",
    "cycle",
    "rotate",
    "rotateVector",
    "scaleVector",
    "scale",
    "seedRandoms",
    "shuffle",
    "sort",
    "subtractVectors",
    "translate"
  ];
  CodeMirror.registerHelper('hintWords', 'codeworld', hints);

  var hash = location.hash.slice(1);
  if (hash.length > 0) {
    if (hash.slice(-2) == '==') {
      hash = hash.slice(0, -2);
    }
    loadFile('loadSource?hash=' + hash);
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

    // Override the syntax classification of words from the standard library.
    var keywordOverrides = {};

    // Special case for main, since it's morally a built-in name.
    keywordOverrides['main'] = 'builtin';

    lines = lines.sort().filter(function(item, pos, array) {
      return !pos || item != array[pos - 1];
    });

    lines.forEach(function(line) {
      var startOfWord = 0;
      if (line.startsWith("type ")) {
        startOfWord += 5;
      } else if (line.startsWith("data ")) {
        startOfWord += 5;
      } else if (line.startsWith("newtype ")) {
        startOfWord += 8;
      } else if (line.startsWith("class ")) {
        return;
      } else if (line.startsWith("instance ")) {
        return;
      } else if (line.startsWith("-- ")) {
        return;
      }

      var endOfWord = line.indexOf(" ", startOfWord);
      if (endOfWord == -1) {
        endOfWord = line.length;
      }
      if (endOfWord == startOfWord) {
        return;
      }

      if (line[startOfWord] == "(" && line[endOfWord - 1] == ")") {
        startOfWord++;
        endOfWord--;
      }

      var word = line.substr(startOfWord, endOfWord - startOfWord);

      if (/^[A-Z:]/.test(word)) {
        keywordOverrides[word] = 'builtin-2';
      } else {
        keywordOverrides[word] = 'builtin';
      }

      if (word.length > 1 && hintBlacklist.indexOf(word) < 0) {
        hints.push(word);
      }
    });

    window.codeworldEditor.setOption(
        'mode', { name: 'codeworld', overrideKeywords: keywordOverrides });

    hints.sort();
    CodeMirror.registerHelper('hintWords', 'codeworld', hints);
  });

  window.onbeforeunload = function(event) {
    if (!isEditorClean()) {
      var msg = 'There are unsaved changes to your project. '
              + 'If you continue, they will be lost!';
      if (event) event.returnValue = msg;
      return msg;
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

function toggleDoc(root) {
  window.showingDoc = !window.showingDoc;
  updateUI();

  if (window.showingDoc) {
    stop();
    var loc = document.getElementById('doc').contentWindow.location;
    loc.search = root;
  }
}

function discoverProjects() {
  if (!signedIn()) {
    allProjectNames = window.openProjectName ? [window.openProjectName] : [];
    updateUI();
    return;
  }

  var data = new FormData();
  data.append('id_token', gapi.auth.getToken().id_token);

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

function setCode(code, history, name) {
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
  stop();
}

function loadFile(name) {
  sendHttp('GET', name, null, function(request) {
    if (request.status == 200) {
      var code = request.responseText;
      var startMarker = '{----- BEGIN LICENSE TEXT -----';
      var endMarker = '----- END LICENSE TEXT -----}';

      var start = code.indexOf(startMarker);
      var end = code.indexOf(endMarker);

      if (start != -1 && end != -1) {
        code = code.substring(0, start) +
               code.substring(end + endMarker.length);
        while (code[0] == '\n') code = code.substring(1);
      }

      setCode(code);
    }
  });
}

function warnIfUnsaved(f) {
  if (isEditorClean()) {
    f();
  } else {
    var msg = 'There are unsaved changes to your project. '
            + 'Continue and throw away your changes?';
    sweetAlert({
      title: 'Warning',
      text: msg,
      type: 'warning',
      showCancelButton: true,
      confirmButtonColor: '#DD6B55',
      confirmButtonText: 'Yes, discard my changes!'
    }, f);
  }
}

function loadSample(code) {
  warnIfUnsaved(function() { setCode(code); });
}

function newProject() {
  warnIfUnsaved(function() { setCode(''); });
}

function loadProject(name) {
  if (!signedIn()) {
    sweetAlert('Oops!', 'You must sign in to open projects.', 'error');
    updateUI();
    return;
  }

  warnIfUnsaved(function() {
    var data = new FormData();
    data.append('id_token', gapi.auth.getToken().id_token);
    data.append('name', name);

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
    var url = runner.contentWindow.location.href;

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
  if (!window.usingHaskellPrelude) {
    msg = msg
        .replace(/\u2018/g, '')
        .replace(/\u2019/g, '')
        .replace(/IO action main/g, 'variable main')
        .replace(/module Main/g,    'the program')
        .replace(/\[GHC\.Types\.Char\] -> /g, '')
        .replace(/base(-[0-9.]*)?\:(.|\n)*?->( |\n)*/g, '')
        .replace(/integer-gmp(-[0-9\.]*)?:(.|\n)*?->( |\n)*/g, '')
        .replace(/Main\./g, '')
        .replace(/main :: t/g, 'main :: Program')
        .replace(/Prelude\./g, '')
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
        .replace(/in string\/character literal/,
                 'in text literal');
  }

  msg = msg
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/(user\/)?(P..\/)?P[A-Za-z0-9_=\-]*\.hs:(\d+):((\d+)(-\d+)?)/g,
               '<a href="#" onclick="goto($3, $5);">Line $3, Column $4</a>')
      .replace(/(user\/)?(P..\/)?P[A-Za-z0-9_=\-]*\.hs:\((\d+),(\d+)\)-\((\d+),(\d+)\)/g,
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
    runner.contentWindow.location.replace('run.html?hash=' + hash);
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

function compile() {
  run('', 'Building...', false);

  var src = window.codeworldEditor.getValue();
  var data = new FormData();
  data.append('source', src);

  sendHttp('POST', 'compile', data, function(request) {
    var hash = request.responseText;
    var success = request.status == 200;

    sendHttp('GET', '/runMsg?hash=' + hash, null, function(request) {
      window.usingHaskellPrelude = /HaskellPrelude/.test(src);
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

    window.clientId = request.responseText;
    return f(window.clientId);
  });
}

function signinCallback(result) {
  discoverProjects();
  updateUI();
}

function signin() {
  withClientId(function(clientid) {
    if (window.gapi) {
      gapi.auth.signIn({
        callback: signinCallback,
        clientid: clientid,
        scope: 'profile',
        cookiepolicy: 'single_host_origin',
      });
    }
  });
}

function signout() {
  if (window.gapi) gapi.auth.signOut();
  discoverProjects();
}

function signedIn() {
  return (window.gapi &&
          window.gapi.auth &&
          gapi.auth.getToken() &&
          gapi.auth.getToken().id_token);
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
    var project = { 'name':    projectName,
                    'source':  doc.getValue(),
                    'history': doc.getHistory() };

    var data = new FormData();
    data.append('id_token', gapi.auth.getToken().id_token);
    data.append('project', JSON.stringify(project));

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
    data.append('id_token', gapi.auth.getToken().id_token);
    data.append('name', window.openProjectName);

    sendHttp('POST', 'deleteProject', data, function(request) {
      if (request.status == 200) {
        savedGeneration = codeworldEditor.getDoc().changeGeneration(true);
        setCode('');
      }

      discoverProjects();
      updateUI();
    });
  }

  var msg = 'Deleting a project will throw away all work, and cannot be undone. '
          + 'Are you sure?';
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
  withClientId(function(clientId) {
    gapi.auth.init(signinCallback);

    // Refresh sign-in every 15 minutes to avoid letting it expire.
    setInterval(function() {
      if (signedIn()) {
        gapi.auth.authorize({
          clientid: clientId,
          scope: 'profile',
          cookiepolicy: 'single_host_origin',
          immediate: true,
        }, signinCallback);
      }
    }, 1000 * 60 * 15);
  });

  discoverProjects();
  updateUI();

  sendHttp('GET', 'rts.js');
  sendHttp('GET', 'lib.base.js');
  sendHttp('GET', 'out.base.js');
}
