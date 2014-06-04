function init() {                                                                                                                     
  var editor = document.getElementById('editor');                                                                                     
  window.codeworldEditor = CodeMirror.fromTextArea(editor, {                                                                          
    mode: 'haskell',                                                                                                                  
    lineNumbers: true,                                                                                                                
    autofocus: true,                                                                                                                  
    matchBrackets: true,                                                                                                              
    styleActiveLine: true,                                                                                                            
    autoCloseBrackets: true,                                                                                                          
    showTrailingSpace: true,                                                                                                          
    rulers: [{column: 80, color: "#bbb", lineStyle: "dashed"}]                                                                        
  });                                                                                                                                 

  CodeMirror.commands.save = function(cm) { saveFile(); }

  var hash = window.location.hash.slice(1);
  if (hash.length > 0) {
    load('user/' + hash + '.hs');
  }
}

function setCode(code, metadata) {
  window.openFileMetadata = metadata;

  stop();
  codeworldEditor.setValue(code);
  codeworldEditor.getDoc().clearHistory();

  if (window.openFileMetadata) {
    document.getElementById('saveButton').style.display = 'inline-block';
  } else {
    document.getElementById('saveButton').style.display = 'none';
  }
}

function load(file) {
  var modified = codeworldEditor.getDoc().historySize().undo > 0 ||
                 codeworldEditor.getDoc().historySize().redo > 0;
  if (!modified || confirm('Replace the program?')) {

    if (file == '') {
      setCode('');
      return;
    }

    var request = new XMLHttpRequest();
    request.open('GET', file, false);
    request.send();

    if (request.status == 200) {
      setCode(request.responseText);
    }
  }
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
  var runner = document.getElementById('runner');

  if (hash != '' && !error) {
    runner.contentWindow.location.replace('run.html?hash=' + hash);
    runner.contentWindow.focus();
    document.getElementById('shareButton').style.display = 'inline-block';
  } else {
    runner.contentWindow.location.replace('about:blank');
    document.getElementById('shareButton').style.display = 'none';
  }

  var message = document.getElementById('message');
  message.textContent = msg;

  if (error) {
    message.classList.add('error');
  } else {
    message.classList.remove('error');
  }
}

function compile() {
  run('', 'Building...', false);

  var source = window.codeworldEditor.getValue();
  var request = new XMLHttpRequest();
  request.open('POST', 'compile', false);
  request.send(source);

  var hash = request.responseText;
  var success = request.status == 200;

  request.open('GET', 'user/' + hash + '.err.txt', false);
  request.send();
  var msg = '';
  if (request.status == 200) {
    msg = request.responseText;
  }

  if (success && msg == '') {
    msg = 'Program starting...';
  }

  run(hash, msg, !success);
}

function openFile() {
  if (!window.gapi || !gapi.client.drive || !google.picker) {
    return;
  }

  function callback(data) {
    if (data[google.picker.Response.ACTION] != google.picker.Action.PICKED) {
      return;
    }

    var id = data[google.picker.Response.DOCUMENTS][0][google.picker.Document.ID];
    var request = gapi.client.drive.files.get({ 'fileId': id });

    request.execute(function(file) {
      if (file.fileSize > 2000000) {
        alert('File is too large.');
        return;
      }

      if (file.downloadUrl) {
        var accessToken = gapi.auth.getToken().access_token;
        var xhr = new XMLHttpRequest();
        xhr.open('GET', file.downloadUrl, false);
        xhr.setRequestHeader('Authorization', 'Bearer ' + accessToken);
        xhr.send();

        if (xhr.status == 200) {
          setCode(xhr.responseText, file);
        }
      }
    });
  }

  var cwView = new google.picker.DocsView();
  cwView.setMode(google.picker.DocsViewMode.LIST);
  cwView.setMimeTypes('text/x-haskell');
  cwView.setLabel('CodeWorld Files');

  var allView = new google.picker.DocsView();
  allView.setMode(google.picker.DocsViewMode.LIST);
  allView.setIncludeFolders(true);

  var picker = new google.picker.PickerBuilder()
    .setTitle('Open a CodeWorld Project')
    .setAppId('codeworld-site')
    .setOAuthToken(gapi.auth.getToken().access_token)
    .disableFeature(google.picker.Feature.MINE_ONLY)
    .addView(cwView)
    .addView(allView)
    .addView(google.picker.ViewId.RECENTLY_PICKED)
    .setCallback(callback)
    .build();
  picker.setVisible(true);
}

function saveFile() {
  if (!window.gapi || !gapi.auth.getToken()) {
    alert('You must sign in to save files.');
    return;
  }

  if (window.openFileMetadata) {
    saveFileBase(openFileMetadata.title);
  } else {
    saveFileAs();
  }
}

function saveFileAs() {
  if (!window.gapi || !gapi.auth.getToken()) {
    alert('You must sign in to save files.');
    return;
  }

  window.codeworldEditor.focus();
  var text = 'Save As: <input type="text" style="width: 10em"/>';

  var defaultName;
  if (window.openFileMetadata) {
    defaultName = window.openFileMetadata.title;
  } else {
    defaultName = '';
  }

  window.codeworldEditor.openDialog(text, saveFileBase, { value: defaultName });
}

function saveFileBase(name) {
  if (name == null || name == '') return;

  if (!window.gapi || !gapi.client.drive || !google.picker) {
    alert('Cannot save your program right now!');
    return;
  }

  var source = window.codeworldEditor.getValue();
  var data = btoa(source);

  var method;
  var path;
  var metadata;
  if (window.openFileMetadata && window.openFileMetadata.title == name) {
    method = 'PUT';
    path = '/upload/drive/v2/files/' + window.openFileMetadata.id,
    metadata = window.openFileMetadata;
  } else {
    method = 'POST';
    path = '/upload/drive/v2/files',
    metadata = {
      'title': name,
      'mimeType': 'text/x-haskell',
    };
  }

  const boundary = '-------314159265358979323846';
  const delimiter = "\r\n--" + boundary + "\r\n";
  const close_delim = "\r\n--" + boundary + "--";

  var multipartRequestBody =
      delimiter +
      'Content-Type: application/json\r\n\r\n' +
      JSON.stringify(metadata) +
      delimiter +
      'Content-Type: text/x-haskell\r\n' +
      'Content-Transfer-Encoding: base64\r\n\r\n' +
      data +
      close_delim;

  request = gapi.client.request({
      'path': path,
      'method': method,
      'params': {'uploadType': 'multipart'},
      'headers': {
        'Content-Type': 'multipart/mixed; boundary="' + boundary + '"'
      },
      'body': multipartRequestBody
  });

  request.execute(function(file) {
    if (file) {
      window.openFileMetadata = file;
    } else {
      alert('Cannot save your program right now!');
    }
  });
}

function signin() {
  function signinCallback(authResult) {
    if (authResult['status']['signed_in']) {
      document.getElementById('signin').style.display = 'none';
      document.getElementById('signout').style.display = 'inline-block';
      document.getElementById('openButton').style.display = 'inline-block';
      document.getElementById('saveAsButton').style.display = 'inline-block';
      gapi.client.load('drive', 'v2');
      gapi.load('picker');
    } else {
      document.getElementById('signin').style.display = 'inline-block';
      document.getElementById('signout').style.display = 'none';
      document.getElementById('openButton').style.display = 'none';
      document.getElementById('saveButton').style.display = 'none';
      document.getElementById('saveAsButton').style.display = 'none';
    }
  }

  if (window.gapi) {
    gapi.auth.signIn({
      callback: signinCallback,
      clientid: '94846197422-jnkt1qd737993e7llrfa5pb1bqc72nog.apps.googleusercontent.com',
      scope: 'profile https://www.googleapis.com/auth/drive',
      cookiepolicy: 'single_host_origin',
    });
  }
}

function signout() {
  if (window.gapi) gapi.auth.signOut();
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
  loadAsync('https://apis.google.com/js/api.js');
})();
