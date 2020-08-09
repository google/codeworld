/*
 * Copyright 2020 The CodeWorld Authors. All rights reserved.
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

// Tracks when the program started, and whether the program has done
// anything observable (as best we can tell).  This is used to decide
// whether deferred errors are triggering substantially after the
// program start (in which case they should trigger a new message),
// or at startup when the compile errors were just printed anyway.
window.programStartTime = Date.now();
window.hasObservableOutput = false;

window.addEventListener('message', (event) => {
  if (!event.data.type) return;

  if (event.data.type === 'graphicsShown') {
    window.dispatchEvent(new Event('resize'));
  } else if (event.data.type === 'startRecord') {
    const canvas = document.getElementById('screen');
    if (canvas !== null) {
      canvasRecorder = new CanvasRecorder(canvas, 30);
      canvasRecorder.recorder.start();
    }
  } else if (event.data.type === 'stopRecord') {
    if (canvasRecorder && canvasRecorder.recorder.state === 'recording') {
      canvasRecorder.recorder.stop();
    }
  }
});

class CanvasRecorder {
  constructor(canvas, framerate) {
    const cStream = canvas.captureStream(framerate);

    this.chunks = [];
    this.recorder = new MediaRecorder(cStream);
    this.recorder.ondataavailable = this.addChunk(this.chunks);
    this.recorder.onstop = this.exportStream(this.chunks);
  }

  addChunk(chunks) {
    return (e) => {
      chunks.push(e.data);
    };
  }

  exportStream(chunks) {
    return () => {
      const blob = new Blob(chunks);

      // Reset data
      chunks = [];

      // Set file name
      const d = new Date();
      const videoFileName = `codeworld_recording_${d
        .toDateString()
        .split(' ')
        .join('_')}_${d.getHours()}:${d.getMinutes()}:${d.getSeconds()}.webm`;

      // Create a new video link
      const a = document.createElement('a');
      document.body.appendChild(a);
      a.style = 'display: none';

      // Save the video
      const url = window.URL.createObjectURL(blob);
      a.href = url;
      a.download = videoFileName;
      a.click();
      window.URL.revokeObjectURL(url);

      // Remove the video link
      a.remove();
    };
  }
}

let canvasRecorder;

function addMessage(type, str) {
  const recentStart = Date.now() - window.programStartTime < 1000;
  const printDeferred = window.hasObservableOutput || !recentStart;

  window.hasObservableOutput = true;

  // Catch exceptions to protect against cross-domain access errors.
  try {
    if (
      type === 'error' &&
      window.buildMode === 'codeworld' &&
      /[(]deferred.*error[)]/.test(str)
    ) {
      if (printDeferred) {
        const match = /^(program[.]hs:[^ ]*)/.exec(str);
        if (match) str = `${match[1]} Giving up because of the error here.`;
        else str = 'Giving up because of errors in the code.';
      } else {
        str = '';
      }
    }

    if (window.parent) {
      window.parent.postMessage(
        {
          type: 'consoleOut',
          msgType: type,
          str: str,
        },
        '*'
      );
      return;
    }
  } catch (e) {
    // Ignore and fall through to console.log below.
  }

  console.log(str);
}

function notifyStarted() {
  try {
    window.parent.postMessage(
      {
        type: 'programStarted',
      },
      '*'
    );
  } catch (e) {
    // Ignore exceptions, which are expected if there's no parent.
  }
}

function showCanvas() {}

function start() {
  const modeMatch = /\bmode=([A-Za-z0-9]*)\b/.exec(location.search);
  window.buildMode = modeMatch ? modeMatch[1] : 'codeworld';

  let hasWarnedStdin = false;

  window.h$base_writeStdout = (fd, fdo, buf, buf_offset, n, c) => {
    addMessage('log', h$decodeUtf8(buf, n, buf_offset));
    c(n);
  };
  window.h$base_writeStderr = (fd, fdo, buf, buf_offset, n, c) => {
    addMessage('log', h$decodeUtf8(buf, n, buf_offset));
    c(n);
  };
  window.h$base_readStdin = (fd, fdo, buf, buf_offset, n, c) => {
    if (!hasWarnedStdin) {
      addMessage('warning', 'CodeWorld programs cannot read from the console.');
      hasWarnedStdin = true;
    }
    c(0);
  };

  window.h$log = (...args) => {
    let s = '';
    for (let i = 0; i < args.length; i++) {
      s = s + args[i];
    }
    addMessage('log', s);
  };
  window.h$errorMsg = (str, ...args) => {
    for (let i = 0; i < args.length; i++) {
      str = str.replace(/%s/, args[i]);
    }
    addMessage('error', str);
  };
  window.h$base_stdout_fd.write = window.h$base_writeStdout;
  window.h$base_stderr_fd.write = window.h$base_writeStderr;
  window.h$base_stdin_fd.read = window.h$base_readStdin;

  const showObserver = new MutationObserver(() => {
    window.hasObservableOutput = true;

    // Catch exceptions to protect against cross-domain access errors.
    // If the frame is cross-domain, then it's embedded, in which case
    // there is no need to show it.
    try {
      if (!window.parent) {
        return;
      }

      window.parent.postMessage(
        {
          type: 'showGraphics',
        },
        '*'
      );
    } catch (e) {
      // Ignore, and assume the canvas is already shown.
    }
    showObserver.disconnect();
  });
  showObserver.observe(document, {
    childList: true,
    attributes: true,
    characterData: true,
    subtree: true,
  });

  // Update program start time in case loading/setup took a while.
  window.programStartTime = Date.now();
  notifyStarted();

  h$gcInterval = 25;
  window.h$run(window.h$mainZCZCMainzimain);

  setTimeout(() => {
    h$gcInterval = 1000;
  }, 200);
}

function init() {
  let paramList = location.search.slice(1).split('&');
  const params = {};
  for (let i = 0; i < paramList.length; i++) {
    const name = decodeURIComponent(paramList[i].split('=')[0]);
    const value = decodeURIComponent(paramList[i].slice(name.length + 1));
    params[name] = value;
  }
  // params from the hash
  paramList = location.hash.slice(1).split('&');
  for (let i = 0; i < paramList.length; i++) {
    const name = decodeURIComponent(paramList[i].split('=')[0]);
    const value = decodeURIComponent(paramList[i].slice(name.length + 1));
    params[name] = value;
  }

  const hash = params['hash'];
  const dhash = params['dhash'];
  let mode = params['mode'];

  if (!mode) mode = 'codeworld';

  let query = `?mode=${encodeURIComponent(mode)}`;
  if (hash) query += `&hash=${encodeURIComponent(hash)}`;
  if (dhash) query += `&dhash=${encodeURIComponent(dhash)}`;

  const uri = `runJS${query}`;

  const loadScript = document.createElement('script');
  loadScript.setAttribute('type', 'text/javascript');
  loadScript.setAttribute('src', uri);
  document.body.appendChild(loadScript);
}

init();

window.init = init;
window.start = start;
window.showCanvas = showCanvas;
