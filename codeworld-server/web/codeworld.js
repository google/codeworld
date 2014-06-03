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

function load(file) {
  var modified = codeworldEditor.getDoc().historySize().undo > 0 ||
                 codeworldEditor.getDoc().historySize().redo > 0;
  if (!modified || confirm('Replace the program?')) {
    stop();

    if (file == '') {
      codeworldEditor.setValue('');
      codeworldEditor.getDoc().clearHistory();
      return;
    }

    var request = new XMLHttpRequest();
    request.open('GET', file, false);
    request.send();

    if (request.status == 200) {
      codeworldEditor.setValue(request.responseText);
      codeworldEditor.getDoc().clearHistory();
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

function signinCallback(authResult) {
  if (authResult['status']['signed_in']) {
    document.getElementById('signin').style.display = 'none';
    document.getElementById('signout').style.display = 'inline-block';
    window.id_token = authResult['id_token'];
  } else {
    document.getElementById('signin').style.display = 'inline-block';
    document.getElementById('signout').style.display = 'none';
    console.log('Sign-in state: ' + authResult['error']);
  }
}

function signin() {
  if (window.gapi) {
    gapi.auth.signIn({
      callback: signinCallback,
      clientid: '94846197422-jnkt1qd737993e7llrfa5pb1bqc72nog.apps.googleusercontent.com',
      scope: 'profile',
      cookiepolicy: 'single_host_origin',
    });
  }
}

function signout() {
  if (window.gapi) gapi.auth.signOut();
}

(function() {
  var po = document.createElement('script');
  po.type = 'text/javascript';
  po.async = true;
  po.src = 'https://apis.google.com/js/client:plusone.js';
  var s = document.getElementsByTagName('script')[0];
  s.parentNode.insertBefore(po, s);
})();
