/*
 * Copyright 2017 The CodeWorld Authors. All rights reserved.
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

function addMessage(err, str) {
  if (window.parent && window.parent.addToMessage) {
    var message = window.parent.addToMessage(str);

    if (err) {
      var message = window.parent.document.getElementById('message');
      message.classList.add('error');
    }
  } else {
    console.log(str);
  }
}

function showCanvas() {
  if (!window.parent) {
    return;
  }

  var runner = window.parent.document.getElementById('runner');
  if (!runner) {
    return;
  }

  runner.style.display = '';
  runner.focus();
  runner.contentWindow.focus();
}

function start() {
  h$base_writeStdout = function(fd, fdo, buf, buf_offset, n, c) {
    addMessage(false, h$decodeUtf8(buf, n, buf_offset));
    c(n);
  };
  h$base_writeStderr = function(fd, fdo, buf, buf_offset, n, c) {
    addMessage(false, h$decodeUtf8(buf, n, buf_offset));
    c(n);
  };
  h$log = function() {
    var s = '';
    for(var i=0;i<arguments.length;i++) { s = s + arguments[i]; }
    addMessage(false, s+'\n');
  };
  h$errorMsg = function(str) {
    for(var i=1;i<arguments.length;i++) {
      str = str.replace(/%s/, arguments[i]);
    }
    addMessage(true, '\n' + str);
  };
  h$base_stdout_fd.write = h$base_writeStdout;
  h$base_stderr_fd.write = h$base_writeStderr;

  h$run(h$mainZCZCMainzimain);
}
