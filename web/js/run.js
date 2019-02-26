/*
 * Copyright 2019 The CodeWorld Authors. All rights reserved.
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
'use strict';

function addMessage(type, str) {
    // Catch exceptions to protect against cross-domain access errors.
    try {
        if (window.parent && window.parent.printMessage) {
            window.parent.printMessage(type, str);
            if (type === 'error') window.parent.markFailed();
            return;
        }
    } catch (e) {
        // Ignore and fall through to console.log below.
    }

    console.log(str);
}

function showCanvas() {
    // Catch exceptions to protect against cross-domain access errors.
    // If the frame is cross-domain, then it's embedded, in which case
    // there is no need to show it.
    try {
        if (!window.parent) {
            return;
        }

        const runner = window.parent.document.getElementById('runner');
        if (!runner) {
            return;
        }

        runner.style.display = '';
        runner.focus();
        runner.contentWindow.focus();
    } catch (e) {
        // Ignore, and assume the canvas is already shown.
    }
}

function start() {
    window.h$base_writeStdout = (fd, fdo, buf, buf_offset, n, c) => {
        addMessage('log', h$decodeUtf8(buf, n, buf_offset));
        c(n);
    };
    window.h$base_writeStderr = (fd, fdo, buf, buf_offset, n, c) => {
        addMessage('log', h$decodeUtf8(buf, n, buf_offset));
        c(n);
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

    window.h$run(window.h$mainZCZCMainzimain);
}
