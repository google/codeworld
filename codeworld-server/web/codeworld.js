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

  var hash = window.location.hash.slice(1);
  if (hash.length > 0) {
    load('user/' + hash + '.hs');
  }
}

function setCode(code, fileId) {
  if (!fileId) {
    fileId = '';
  }
  window.openFileId = fileId;

  stop();
  codeworldEditor.setValue(code);
  codeworldEditor.getDoc().clearHistory();
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
  var runner = document.getElementById('runner');
  var message = document.getElementById('message');

  runner.contentWindow.location.replace('about:blank');
  message.classList.remove('error');
  message.textContent = '';
}

function compile() {
  var runner = document.getElementById('runner');
  var message = document.getElementById('message');

  runner.contentWindow.location.replace('about:blank');
  message.classList.remove('error');
  message.textContent = 'Starting...';

  var source = window.codeworldEditor.getValue();
  var request = new XMLHttpRequest();
  request.open('POST', 'compile', false);
  request.send(source);

  var hash = request.responseText;
  var success = request.status == 200;

  if (success) {
    runner.contentWindow.location.replace('run.html?hash=' + hash);
    runner.contentWindow.focus();
  }

  message.textContent = '';

  request.open('GET', 'user/' + hash + '.err.txt', false);
  request.send();
  var msg = request.responseText;

  if (request.status == 200) {
    message.textContent = request.responseText;
    if (!success) {
      message.classList.add('error');
    }
  } else {
    message.textContent = '';
  }
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
      if (file.downloadUrl) {
        var accessToken = gapi.auth.getToken().access_token;
        var xhr = new XMLHttpRequest();
        xhr.open('GET', file.downloadUrl, false);
        xhr.setRequestHeader('Authorization', 'Bearer ' + accessToken);
        xhr.send();

        if (xhr.status == 200) {
          setCode(xhr.responseText, id);
        }
      }
    });
  }

  var view = new google.picker.DocsView(google.picker.ViewId.DOCS);
  view.setMode(google.picker.DocsViewMode.LIST);
  var picker = new google.picker.PickerBuilder()
    .setAppId('codeworld-site')
    .setOAuthToken(gapi.auth.getToken().access_token)
    .addView(view)
    .addView(new google.picker.DocsUploadView())
    .setCallback(callback)
    .build();
  picker.setVisible(true);
}

function saveFile() {
  alert('Saving files is not yet implemented.');
  if (!window.gapi || !gapi.client.drive) {
    return;
  }
}

function signinCallback(authResult) {
  if (authResult['status']['signed_in']) {
    document.getElementById('signin').style.display = 'none';
    document.getElementById('signout').style.display = 'inline-block';
    document.getElementById('openButton').style.display = 'inline-block';
    document.getElementById('saveButton').style.display = 'inline-block';
    gapi.client.load('drive', 'v2');
    gapi.load('picker');
  } else {
    document.getElementById('signin').style.display = 'inline-block';
    document.getElementById('signout').style.display = 'none';
    document.getElementById('openButton').style.display = 'none';
    document.getElementById('saveButton').style.display = 'none';
  }
}

function signin() {
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
