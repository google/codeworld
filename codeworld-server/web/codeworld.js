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
