/*
 * Copyright 2016 The CodeWorld Authors. All rights reserved.
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

/*
 * Utility function for sending an HTTP request to fetch a resource.
 *
 * Args:
 *   - method: The HTTP method to use, such as 'GET'
 *   - url: The URL to fetch, whether absolute or relative.
 *   - body: The request body to send.  Use null for no body.
 *   - callback: A callback function to send when complete.  (optional)
 *
 * If provided, the callback will be given the XmlHttpRequest object, so
 * it can inspect the response code and headers as well as the contents.
 */
function sendHttp(method, url, body, callback) {
    var request = new XMLHttpRequest();

    if (callback) {
        request.onreadystatechange = function() {
            if (request.readyState == 4) callback(request);
        };
    }

    request.open(method, url, true);
    request.send(body);
}

function addToMessage(msg) {
    var message = document.getElementById('message');
    message.innerHTML += msg
}

function updateEditor(code) {
    var editor = document.getElementById('genCode');
    CodeMirror.runMode(code
      ,{name: 'codeworld'}
      ,editor);
}

function run(hash, msg, error) {
    window.showingResult = hash || msg;
    if (window.showingResult) {
        window.showingDoc = false;
    }

    if (hash) {
        window.location.hash = '#' + hash;
    } else {
        window.location.hash = '';
    }

    var runner = document.getElementById('runner');
    if (hash && !error) {
        var loc = 'run.html?hash=' + hash + '&mode=' + window.buildMode;
        runner.contentWindow.location.replace(loc);
        document.getElementById('runner').style.display = '';
        document.getElementById('runner').contentWindow.focus();
        window.programRunning = true;
    } else {
        runner.contentWindow.location.replace('about:blank');
        document.getElementById('runner').style.display = 'none';
        window.programRunning = false;
    }

    var message = document.getElementById('message');
    message.innerHTML = '';
    addToMessage(msg);

    if (error) {
        message.classList.add('error');
    } else {
        message.classList.remove('error');
    }

}

function compile(src) {
    run('', 'Building...', false);

    var data = new FormData();
    data.append('source', src);
    data.append('mode', window.buildMode);

    sendHttp('POST', 'compile', data, function(request) {
        var hash = request.responseText;
        var success = request.status == 200;

        var data = new FormData();
        data.append('hash', hash);
        data.append('mode', window.buildMode);

        sendHttp('POST', 'runMsg', data, function(request) {
            var msg = '';
            if (request.status == 200) {
                msg = request.responseText;
            } else if (request.status == 404) {
                msg = "Sorry!  Your program couldn't be run right now.  Please try again.";
            }

            if (success) {
                run(hash, 'Running...\n\n' + msg, false);
            } else {
                run(hash, msg, true);
            }
        });
    });
}


