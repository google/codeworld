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
