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

  // We override the syntax classification of certain words, because the
  // standard library is different.
  var keywordOverrides = {
    '$!':              'variable',
    '$':               'variable',
    '=<<':             'variable',
    '>>=':             'variable',
    '>>':              'variable',
    '^^':              'variable',
    '**':              'variable',
    '&':               'builtin',
    '<>':              'builtin',
    'Bounded':         'variable-2',
    'Char':            'variable-2',
    'Double':          'variable-2',
    'Enum':            'variable-2',
    'EQ':              'variable-2',
    'Eq':              'variable-2',
    'FilePath':        'variable-2',
    'Float':           'variable-2',
    'Floating':        'variable-2',
    'Fractional':      'variable-2',
    'Functor':         'variable-2',
    'GT':              'variable-2',
    'IOError':         'variable-2',
    'Int':             'variable-2',
    'Integer':         'variable-2',
    'Integral':        'variable-2',
    'LT':              'variable-2',
    'Monad':           'variable-2',
    'Num':             'variable-2',
    'Ord':             'variable-2',
    'Ordering':        'variable-2',
    'Rational':        'variable-2',
    'Read':            'variable-2',
    'ReadS':           'variable-2',
    'Real':            'variable-2',
    'RealFloat':       'variable-2',
    'RealFrac':        'variable-2',
    'Show':            'variable-2',
    'ShowS':           'variable-2',
    'String':          'variable-2',
    'Color':           'builtin',
    'Event':           'builtin',
    'KeyPress':        'builtin',
    'KeyRelease':      'builtin',
    'MouseMovement':   'builtin',
    'MousePress':      'builtin',
    'MouseRelease':    'builtin',
    'Number':          'builtin',
    'Picture':         'builtin',
    'Point':           'builtin',
    'Program':         'builtin',
    'Text':            'builtin',
    'Vector':          'builtin',
    'acosh':           'variable',
    'appendFile':      'variable',
    'asTypeOf':        'variable',
    'asinh':           'variable',
    'atanh':           'variable',
    'catch':           'variable',
    'compare':         'variable',
    'cosh':            'variable',
    'curry':           'variable',
    'decodeFloat':     'variable',
    'div':             'variable',
    'divMod':          'variable',
    'elem':            'variable',
    'encodeFloat':     'variable',
    'enumFrom':        'variable',
    'enumFromThen':    'variable',
    'enumFromThenTo':  'variable',
    'enumFromTo':      'variable',
    'exponent':        'variable',
    'fail':            'variable',
    'floatDigits':     'variable',
    'floatRadix':      'variable',
    'floatRange':      'variable',
    'fmap':            'variable',
    'foldl':           'variable',
    'foldl1':          'variable',
    'foldr':           'variable',
    'foldr1':          'variable',
    'fromEnum':        'variable',
    'fromInteger':     'variable',
    'fromIntegral':    'variable',
    'fromRational':    'variable',
    'getChar':         'variable',
    'getContents':     'variable',
    'getLine':         'variable',
    'head':            'variable',
    'interact':        'variable',
    'ioError':         'variable',
    'isDenormalized':  'variable',
    'isIEEE':          'variable',
    'isInfinite':      'variable',
    'isNaN':           'variable',
    'isNegativeZero':  'variable',
    'iterate':         'variable',
    'lex':             'variable',
    'mapM':            'variable',
    'mapM_':           'variable',
    'maxBound':        'variable',
    'minBound':        'variable',
    'mod':             'variable',
    'notElem':         'variable',
    'pred':            'variable',
    'print':           'variable',
    'putChar':         'variable',
    'putStr':          'variable',
    'putStrLn':        'variable',
    'quot':            'variable',
    'quotRem':         'variable',
    'read':            'variable',
    'readFile':        'variable',
    'readIO':          'variable',
    'readList':        'variable',
    'readLn':          'variable',
    'readParen':       'variable',
    'reads':           'variable',
    'readsPrec':       'variable',
    'realToFrac':      'variable',
    'recip':           'variable',
    'rem':             'variable',
    'return':          'variable',
    'scaleFloat':      'variable',
    'scanl':           'variable',
    'scanl1':          'variable',
    'scanr':           'variable',
    'scanr1':          'variable',
    'seq':             'variable',
    'sequence':        'variable',
    'sequence_':       'variable',
    'showChar':        'variable',
    'showList':        'variable',
    'showParen':       'variable',
    'showString':      'variable',
    'shows':           'variable',
    'showsPrec':       'variable',
    'significand':     'variable',
    'sinh':            'variable',
    'subtract':        'variable',
    'succ':            'variable',
    'tail':            'variable',
    'tanh':            'variable',
    'toEnum':          'variable',
    'toInteger':       'variable',
    'toRational':      'variable',
    'uncurry':         'variable',
    'unzip':           'variable',
    'unzip3':          'variable',
    'userError':       'variable',
    'writeFile':       'variable',
    'zip':             'variable',
    'zip3':            'variable',
    'zipWith':         'variable',
    'zipWith3':        'variable',
    'addVectors':      'builtin',
    'animationOf':     'builtin',
    'append':          'builtin',
    'appendAll':       'builtin',
    'aquamarine':      'builtin',
    'arc':             'builtin',
    'azure':           'builtin',
    'black':           'builtin',
    'blank':           'builtin',
    'blue':            'builtin',
    'bright':          'builtin',
    'brown':           'builtin',
    'characters':      'builtin',
    'chartreuse':      'builtin',
    'circle':          'builtin',
    'color':           'builtin',
    'coordinatePlane': 'builtin',
    'cyan':            'builtin',
    'dark':            'builtin',
    'empty':           'builtin',
    'first':           'builtin',
    'fromOperator':    'builtin',
    'gray':            'builtin',
    'green':           'builtin',
    'grey':            'builtin',
    'interactionOf':   'builtin',
    'isInteger':       'builtin',
    'isMember':        'builtin',
    'join':            'builtin',
    'light':           'builtin',
    'line':            'builtin',
    'magenta':         'builtin',
    'muted':           'builtin',
    'nub':             'builtin',
    'numberOfCharacters': 'builtin',
    'numberOfLines':   'builtin',
    'numberOfWords':   'builtin',
    'orange':          'builtin',
    'permutations':    'builtin',
    'pictureOf':       'builtin',
    'pictures':        'builtin',
    'pink':            'builtin',
    'polygon':         'builtin',
    'purple':          'builtin',
    'quotient':        'builtin',
    'reciprocal':      'builtin',
    'rectangle':       'builtin',
    'red':             'builtin',
    'remainder':       'builtin',
    'replace':         'builtin',
    'rest':            'builtin',
    'rose':            'builtin',
    'rotate':          'builtin',
    'rotateVector':    'builtin',
    'scale':           'builtin',
    'scaleVector':     'builtin',
    'search':          'builtin',
    'sector':          'builtin',
    'simulationOf':    'builtin',
    'solidCircle':     'builtin',
    'solidRectangle':  'builtin',
    'sort':            'builtin',
    'strip':           'builtin',
    'stripPrefix':     'builtin',
    'stripSuffix':     'builtin',
    'subsequences':    'builtin',
    'substring':       'builtin',
    'text':            'builtin',
    'thickArc':        'builtin',
    'thickCircle':     'builtin',
    'thickLine':       'builtin',
    'thickRectangle':  'builtin',
    'toLower':         'builtin',
    'toOperator':      'builtin',
    'toUpper':         'builtin',
    'translate':       'builtin',
    'translucent':     'builtin',
    'transpose':       'builtin',
    'violet':          'builtin',
    'white':           'builtin',
    'withDefault':     'builtin',
    'yellow':          'builtin',
  };

  updateVisibility();

  var editor = document.getElementById('editor');
  codeworldEditor = CodeMirror.fromTextArea(editor, {
    mode: { name: 'haskell', overrideKeywords: keywordOverrides },
    lineNumbers: true,
    autofocus: true,
    matchBrackets: true,
    styleActiveLine: true,
    autoCloseBrackets: true,
    showTrailingSpace: true,
    rulers: [{column: 80, color: "#bbb", lineStyle: "dashed"}],
    extraKeys: {"Ctrl-Space": "autocomplete"}
  });

  CodeMirror.commands.save = function(cm) { saveProject(); }

  sendHttp('GET', 'autocomplete.txt', null, function(request) {
    if (request.status != 200) {
      console.log('Failed to load autocomplete word list.');
      return;
    }

    CodeMirror.registerHelper('hintWords', 'haskell',
                              request.responseText.split('\n'));
  });

  var hash = location.hash.slice(1);
  if (hash.length > 0) {
    loadFile('user/' + hash + '.hs');
    window.location.hash = '';
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
  if (window.showingDoc) {
    var loc = document.getElementById('doc').contentWindow.location;
    loc.hash = root;
    loc.reload(true);
  }
  updateVisibility();
}

function discoverExamples() {
  sendHttp('GET', 'listExamples', null, function(request) {
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
  var message = document.getElementById('message');
  message.innerHTML += msg
      .replace('&', '&amp;')
      .replace('<', '&lt;')
      .replace('>', '&gt;')
      .replace(/your program:(\d+):(\d+)/g,
               '<a href="#" onclick="goto($1, $2);">Line $1, Column $2</a>');
}

function run(hash, msg, error) {
  window.showingResult = hash || msg;

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

  var data = new FormData();
  data.append('source', window.codeworldEditor.getValue());

  sendHttp('POST', 'compile', data, function(request) {
    var hash = request.responseText;
    var success = request.status == 200;

    sendHttp('GET', 'user/' + hash + '.err.txt', null, function(request) {
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

function signin() {
  function callback(result) {
    discoverProjects();
    updateVisibility();
  }

  withClientId(function(clientid) {
    if (window.gapi) {
      gapi.auth.signIn({
        callback: callback,
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
  return (window.gapi && window.gapi.auth && gapi.auth.getToken());
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
    discoverProjects();
    updateVisibility();
  });

  sendHttp('GET', 'deep_eq.js');
  sendHttp('GET', 'user/base.jsexe/lib.base.js');
  sendHttp('GET', 'user/base.jsexe/rts.js');
  sendHttp('GET', 'user/base.jsexe/lib1.base.js');
  sendHttp('GET', 'user/base.jsexe/out.base.js');
})();
