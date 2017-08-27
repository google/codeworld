function reportRuntimeError(err, str) {
  if (window.parent.addToMessage) {
    var message = window.parent.addToMessage('\n' + str);

    if (err) {
      var message = window.parent.document.getElementById('message');
      message.classList.add('error');
    }
  } else {
    console.log(str);
  }
}

function start() {
  h$base_writeStdout = function(fd, fdo, buf, buf_offset, n, c) {
    reportRuntimeError(false, h$decodeUtf8(buf, n, buf_offset));
    c(n);
  };
  h$base_writeStderr = function(fd, fdo, buf, buf_offset, n, c) {
    reportRuntimeError(false, h$decodeUtf8(buf, n, buf_offset));
    c(n);
  };
  h$log = function() {
    var s = '';
    for(var i=0;i<arguments.length;i++) { s = s + arguments[i]; }
    reportRuntimeError(false, s+'\n');
  };
  h$errorMsg = function(str) {
    for(var i=1;i<arguments.length;i++) {
      str = str.replace(/%s/, arguments[i]);
    }
    reportRuntimeError(true, str);
  };
  h$base_stdout_fd.write = h$base_writeStdout;
  h$base_stderr_fd.write = h$base_writeStderr;

  h$run(h$mainZCZCMainzimain);
}
