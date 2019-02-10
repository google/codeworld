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

function addMessage(err, str) {
    // Catch exceptions to protect against cross-domain access errors.
    try {
        if (window.parent && window.parent.addToMessage) {
            let message = window.parent.addToMessage(str);

            if (err) {
                let message = window.parent.document.getElementById('message');
                message.classList.add('error');
            }

            return;
        }
    } catch (e) {}

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

        let runner = window.parent.document.getElementById('runner');
        if (!runner) {
            return;
        }

        runner.style.display = '';
        runner.focus();
        runner.contentWindow.focus();
    } catch (e) {}
}

function start() {
    h$base_writeStdout = (fd, fdo, buf, buf_offset, n, c) => {
        addMessage(false, h$decodeUtf8(buf, n, buf_offset));
        c(n);
    };
    h$base_writeStderr = (fd, fdo, buf, buf_offset, n, c) => {
        addMessage(false, h$decodeUtf8(buf, n, buf_offset));
        c(n);
    };
    h$log = () => {
        let s = '';
        for (let i = 0; i < arguments.length; i++) {
            s = s + arguments[i];
        }
        addMessage(false, s + '\n');
    };
    h$errorMsg = (str) => {
        for (let i = 1; i < arguments.length; i++) {
            str = str.replace(/%s/, arguments[i]);
        }
        addMessage(true, '\n' + str);
    };
    h$base_stdout_fd.write = h$base_writeStdout;
    h$base_stderr_fd.write = h$base_writeStderr;

    h$run(h$mainZCZCMainzimain);
}