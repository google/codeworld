function init() {
  showingBrowse = true;
  showingDoc = false;
  showingResult = false;

  updateVisibility();

  var editor = document.getElementById('editor');
  codeworldEditor = CodeMirror.fromTextArea(editor, {
    mode: 'haskell',
    lineNumbers: true,
    autofocus: true,
    matchBrackets: true,
    styleActiveLine: true,
    autoCloseBrackets: true,
    showTrailingSpace: true,
    rulers: [{column: 80, color: "#bbb", lineStyle: "dashed"}]
  });

  CodeMirror.commands.save = function(cm) { saveProject(); }

  var hash = location.hash.slice(1);
  if (hash.length > 0) {
    loadFile('user/' + hash + '.hs');
  } else {
    setCode('');
  }

  discoverExamples();

  onbeforeunload = function(event) {
    if (!isEditorClean()) {
      var msg = 'There are unsaved changes to your project. '
              + 'If you continue, they will be lost!';
      if (event) event.returnValue = msg;
      return msg;
    }
  }
}

function sendHttp(method, url, body, auth, callback) {
  var request = new XMLHttpRequest();

  if (callback) {
    request.onreadystatechange = function() {
      if (request.readyState == 4) callback(request);
    };
  }

  request.open(method, url, true);

  if (auth) {
    var accessToken = gapi.auth.getToken().access_token;
    request.setRequestHeader('Authorization', 'Bearer ' + accessToken);
  }

  request.send(body);
}

function updateVisibility() {
  var accessToken = gapi.auth.getToken();
  if (accessToken) {
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

function toggleDoc() {
  window.showingDoc = !window.showingDoc;
  if (window.showingDoc) {
    var path = document.getElementById('docPath').innerText;
    document.getElementById('doc').contentWindow.location.replace(path);
  }
  updateVisibility();
}

function discoverExamples() {
  sendHttp('GET', 'listExamples', null, false, function(request) {
    if (request.status != 200) {
      return;
    }

    var examples = document.getElementById('nav_examples');
    while (examples.firstChild) {
      examples.removeChild(examples.firstChild);
    }

    JSON.parse(request.responseText).forEach(function(filename) {
      if (filename == '') {
        return;
      }

      var name = filename.replace(/\.[^/.]+$/, '');

      var template = document.getElementById('exampleTemplate').innerHTML;
      template = template.replace('{{label}}', 'Try: ' + name);

      var span = document.createElement('span');
      span.innerHTML = template;
      var elem = span.getElementsByTagName('a')[0];
      elem.onclick = function() {
        loadFile('examples/' + filename);
      };

      examples.appendChild(span.removeChild(elem));
    });
  });
}

function discoverProjects() {
  var projects = document.getElementById('nav_mine');
  var newProject = document.getElementById('newButton');

  if (!window.gapi || !gapi.auth.getToken()) {
    while (projects.lastChild && projects.lastChild != newProject) {
      projects.removeChild(projects.lastChild);
    }
    return;
  }

  var data = new FormData();
  data.append('id_token', gapi.auth.getToken().id_token);

  sendHttp('POST', 'listProjects', data, false, function(request) {
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

  stop();
}

function loadFile(name) {
  sendHttp('GET', name, null, false, function(request) {
    if (request.status == 200) {
      setCode(request.responseText);
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

  sendHttp('POST', 'loadProject', data, false, function(request) {
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

function run(hash, msg, error) {
  window.showingResult = hash || msg;

  var runner = document.getElementById('runner');
  if (hash && !error) {
    runner.contentWindow.location.replace('run.html?hash=' + hash);
    document.getElementById('runner').contentWindow.focus();
    window.programRunning = true;
  } else {
    runner.contentWindow.location.replace('about:blank');
    window.programRunning = false;
  }

  var message = document.getElementById('message');
  message.textContent = msg;

  if (error) {
    message.classList.add('error');
  } else {
    message.classList.remove('error');
  }

  updateVisibility();
}

function compile() {
  run('', 'Building...', false);

  var data = new FormData();
  data.append('source', window.codeworldEditor.getValue());

  sendHttp('POST', 'compile', data, false, function(request) {
    var hash = request.responseText;
    var success = request.status == 200;

    sendHttp('GET', 'user/' + hash + '.err.txt', null, false, function(request) {
      var msg = '';
      if (request.status == 200) {
        msg = request.responseText;
      }

      if (success && msg == '') {
        msg = 'Running...';
      }

      run(hash, msg, !success);
    });
  });
}

function signin() {
  function callback(result) {
    discoverProjects();
    updateVisibility();
  }

  if (window.gapi) {
    gapi.auth.signIn({
      callback: callback,
      clientid: '94846197422-jnkt1qd737993e7llrfa5pb1bqc72nog.apps.googleusercontent.com',
      scope: 'profile',
      cookiepolicy: 'single_host_origin',
    });
  }
}

function signout() {
  if (window.gapi) gapi.auth.signOut();
}

function signedIn() {
  return (window.gapi && gapi.auth.getToken());
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

  sendHttp('POST', 'saveProject', data, false, function(request) {
    if (request.status != 200) {
      alert('Could not save your project!!!');
      return;
    }

    window.openProjectName = projectName;
    window.savedGeneration = doc.changeGeneration(true);
    discoverProjects();
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

  sendHttp('POST', 'deleteProject', data, false, function(request) {
    if (request.status == 200) {
      savedGeneration = codeworldEditor.getDoc().changeGeneration(true);
      setCode('');
    }

    discoverProjects();
    updateVisibility();
  });
}

(function() {
  function loadAsync(src) {
    var po = document.createElement('script');
    po.type = 'text/javascript';
    po.async = true;
    po.src = src;
    var s = document.getElementsByTagName('script')[0];
    s.parentNode.insertBefore(po, s);
  }
  loadAsync('https://apis.google.com/js/client:plusone.js');
  sendHttp('GET', 'deep_eq.js');
  sendHttp('GET', 'user/base.jsexe/lib.base.js');
  sendHttp('GET', 'user/base.jsexe/rts.js');
  sendHttp('GET', 'user/base.jsexe/lib1.base.js');
  sendHttp('GET', 'user/base.jsexe/out.base.js');
})();
