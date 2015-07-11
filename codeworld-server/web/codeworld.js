/*
 * Copyright 2014 Google Inc. All rights reserved.
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

function init() {
  showingBrowse = true;
  showingDoc = false;
  showingResult = false;

  usingHaskellPrelude = false;

  updateVisibility();

  sendHttp('GET', 'autocomplete.txt', null, function(request) {
    var words = [];
    if (request.status != 200) {
      console.log('Failed to load autocomplete word list.');
    } else {
      words = request.responseText.split('\n');
    }

    var editor = document.getElementById('editor');

    // Override the syntax classification of words from the standard library.
    var keywordOverrides = {};

    var hints = [
      "main", "--", "{-", "-}", "::", "->", "<-", "..", "case", "of", "if",
      "then", "else", "data", "let", "in", "where"
    ];
    var hintBlacklist = [
      "IO", "fromDouble", "fromInt", "fromInteger", "fromRational", "fromString",
      "ifThenElse", "toDouble", "toInt"
    ];

    words.forEach(function(word) {
      if (/^[A-Z:]/.test(word)) {
        keywordOverrides[word] = 'builtin-2';
      } else {
        keywordOverrides[word] = 'builtin';
      }

      if (word.length > 1 && hintBlacklist.indexOf(word) < 0) {
        hints.push(word);
      }
    });

    // Special case for main, since it's morally a built-in name.
    keywordOverrides['main'] = 'builtin';

    window.codeworldEditor = CodeMirror.fromTextArea(editor, {
      mode: { name: 'codeworld', overrideKeywords: keywordOverrides },
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
    CodeMirror.registerHelper('hintWords', 'codeworld', hints);

    window.codeworldEditor.on('changes', window.setTitle);

    var hash = location.hash.slice(1);
    if (hash.length > 0) {
      if (hash.indexOf('==', hash.length - 2) === -1) {
        hash += '==';
      }
      loadFile('user/' + hash + '.hs');
      window.location.hash = '';
    } else {
      setCode('');
    }
  });

  onbeforeunload = function(event) {
    if (!isEditorClean()) {
      var msg = 'There are unsaved changes to your project. '
              + 'If you continue, they will be lost!';
      if (event) event.returnValue = msg;
      return msg;
    }
  }
}

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

function updateVisibility() {
  if (signedIn()) {
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
  }
}

function toggleBrowser() {
  window.showingBrowse = !window.showingBrowse;
  updateVisibility();
}

function toggleDoc(root) {
  window.showingDoc = !window.showingDoc;
  updateVisibility();
  if (window.showingDoc) {
    stop();

    var loc = document.getElementById('doc').contentWindow.location;
    loc.search = root;
  }
}

function discoverProjects() {
  var projects = document.getElementById('nav_mine');
  var newProject = document.getElementById('newButton');

  if (!signedIn()) {
    while (projects.lastChild && projects.lastChild != newProject) {
      projects.removeChild(projects.lastChild);
    }
    return;
  }

  var data = new FormData();
  data.append('id_token', gapi.auth.getToken().id_token);

  sendHttp('POST', 'listProjects', data, function(request) {
    while (projects.lastChild && projects.lastChild != newProject) {
      projects.removeChild(projects.lastChild);
    }

    if (request.status != 200) {
      return;
    }

    JSON.parse(request.responseText).forEach(function(projectName) {
      var encodedName = projectName.replace('&', '&amp;')
                                   .replace('<', '&lt;')
                                   .replace('>', '&gt;');

      var template = document.getElementById('projectTemplate').innerHTML;
      template = template.replace('{{label}}', encodedName);

      var span = document.createElement('span');
      span.innerHTML = template;
      var elem = span.getElementsByTagName('a')[0];
      elem.onclick = function() {
        loadProject(projectName);
      };

      projects.appendChild(span.removeChild(elem));
    });
  });
}

function setTitle() {
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

function isEditorClean() {
  var doc = window.codeworldEditor.getDoc();

  if (window.savedGeneration == null) return doc.getValue() == '';
  else return doc.isClean(window.savedGeneration);
}

function setCode(code, history, name) {
  var msg = 'There are unsaved changes to your project. '
          + 'Continue and throw away your changes?';
  if (!isEditorClean() && !confirm(msg)) {
    return;
  }

  var doc = codeworldEditor.getDoc();

  openProjectName = name;
  doc.setValue(code);
  if (history) {
    doc.setHistory(history);
  } else {
    doc.clearHistory();
  }
  codeworldEditor.focus();

  savedGeneration = doc.changeGeneration(true);
  setTitle();
  updateVisibility();
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

function loadProject(name) {
  if (!signedIn()) {
    alert('You must sign in to open projects.');
    return;
  }

  var data = new FormData();
  data.append('id_token', gapi.auth.getToken().id_token);
  data.append('name', name);

  sendHttp('POST', 'loadProject', data, function(request) {
    if (request.status == 200) {
      var project = JSON.parse(request.responseText);
      setCode(project.source, project.history, name);
    }
  });
}

function share() {
  var runner = document.getElementById('runner');
  if (runner.contentWindow.location.href == 'about:blank') {
    alert('You must run your program before sharing it.');
  } else {
    prompt('Copy and share the following link:',
           runner.contentWindow.location.href);
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
        .replace(/IO action main/g, 'variable main')
        .replace(/\[GHC\.Types\.Char\] -> /g, '')
        .replace(/base\:GHC\.Base\.String -> /g, '')
        .replace(/integer-gmp:(.|\n)*?-> /g, '')
        .replace(/Main\./g, '')
        .replace(/Prelude\./g, '')
        .replace(/IO \(\)/g, 'Program')
        .replace(/IO [a-z][a-zA-Z0-9_]*/g, 'Program')
        .replace(/Perhaps you intended to use TemplateHaskell/g, '')
        .replace(/imported from [^)\n]*/g, 'defined in the standard library')
        .replace(/\(and originally defined in [^)]*\)/g, '')
        .replace(/the first argument/g, 'the parameter(s)')
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
      .replace(/(user\/)?P[A-Za-z0-9_=\-]*\.hs:(\d+):((\d+)(-\d+)?)/g,
               '<a href="#" onclick="goto($2, $4);">Line $2, Column $3</a>')
      .replace(/(user\/)?P[A-Za-z0-9_=\-]*\.hs:\((\d+),(\d+)\)-\((\d+),(\d+)\)/g,
               '<a href="#" onclick="goto($2, $3);">Line $2-$4, Column $3-$5</a>');

  var message = document.getElementById('message');
  message.innerHTML += msg
}

function run(hash, msg, error) {
  window.showingResult = hash || msg;
  if (window.showingResult) {
    window.showingDoc = false;
  }

  var runner = document.getElementById('runner');
  if (hash && !error) {
    window.location.hash = '#' + hash;
    runner.contentWindow.location.replace('run.html?hash=' + hash);
    document.getElementById('runner').contentWindow.focus();
    window.programRunning = true;
  } else {
    window.location.hash = '';
    runner.contentWindow.location.replace('about:blank');
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

  updateVisibility();
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

    sendHttp('GET', 'user/' + hash + '.err.txt', null, function(request) {
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
        run('', msg, true);
      }
    });
  });
}

function withClientId(f) {
  if (window.clientId) return f(window.clientId);

  sendHttp('GET', 'clientId.txt', null, function(request) {
    if (request.status != 200 || request.responseText == '') {
      window.alert('Missing API client key.');
      return null;
    }

    window.clientId = request.responseText;
    return f(window.clientId);
  });
}

function signinCallback(result) {
  discoverProjects();
  updateVisibility();
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
}

function signedIn() {
  return (window.gapi &&
          window.gapi.auth &&
          gapi.auth.getToken() &&
          gapi.auth.getToken().id_token);
}

function saveProject() {
  if (!signedIn()) {
    alert('You must sign in to save files.');
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
    alert('You must sign in to save files.');
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

  window.codeworldEditor.openDialog(text, saveProjectBase, { value: defaultName });
}

function saveProjectBase(projectName) {
  if (projectName == null || projectName == '') return;

  if (!signedIn) {
    alert('You must sign in to save files.');
    return;
  }

  var doc = window.codeworldEditor.getDoc();
  var project = { 'name':    projectName,
                  'source':  doc.getValue(),
                  'history': doc.getHistory() };

  var data = new FormData();
  data.append('id_token', gapi.auth.getToken().id_token);
  data.append('project', JSON.stringify(project));

  sendHttp('POST', 'saveProject', data, function(request) {
    if (request.status != 200) {
      alert('Could not save your project!!!');
      return;
    }

    window.openProjectName = projectName;
    window.savedGeneration = doc.changeGeneration(true);
    discoverProjects();
    setTitle();
    updateVisibility();
  });
}

function deleteProject() {
  if (!window.openProjectName) return;

  if (!signedIn) {
    alert('You must sign in to do this.');
    return;
  }

  var msg = 'Deleting a project will throw away all work, and cannot be undone. '
          + 'Are you sure?';
   if (!confirm(msg)) {
    return;
  }

  var data = new FormData();
  data.append('id_token', gapi.auth.getToken().id_token);
  data.append('name', window.openProjectName);

  sendHttp('POST', 'deleteProject', data, function(request) {
    if (request.status == 200) {
      savedGeneration = codeworldEditor.getDoc().changeGeneration(true);
      setCode('');
    }

    discoverProjects();
    updateVisibility();
  });
}

(function() {
  function loadAsync(src, callback) {
    var po = document.createElement('script');
    po.type = 'text/javascript';
    po.async = true;
    po.src = src;
    if (callback) po.onload = callback;
    var s = document.getElementsByTagName('script')[0];
    s.parentNode.insertBefore(po, s);
  }

  loadAsync('https://apis.google.com/js/client:plusone.js', function() {
    withClientId(function(clientId) {
      gapi.auth.init(signinCallback);

      // Refresh sign-in every 45 minutes to avoid letting it expire.
      setInterval(function() {
        if (signedIn()) {
          gapi.auth.authorize({
            clientid: clientId,
            scope: 'profile',
            cookiepolicy: 'single_host_origin',
            immediate: true,
          }, signinCallback);
        }
      }, 1000 * 60);
    });

    discoverProjects();
    updateVisibility();
  });

  sendHttp('GET', 'deep_eq.js');
  sendHttp('GET', 'user/base.jsexe/lib.base.js');
  sendHttp('GET', 'user/base.jsexe/rts.js');
  sendHttp('GET', 'user/base.jsexe/lib1.base.js');
  sendHttp('GET', 'user/base.jsexe/out.base.js');
})();
