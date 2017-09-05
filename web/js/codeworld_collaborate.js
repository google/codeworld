/*
 * Copyright 2017 The CodeWorld Authors. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://w...content-available-to-author-only...e.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

function shareForCollaboration() {
    if (!signedIn()) {
        sweetAlert('Oops!', 'You must sign in to collaborate with others!', 'error');
        updateUI();
        return;
    }
    if (window.openProjectName == '' || window.openProjectName == null) {
        sweetAlert('Oops!', 'You must select a project for collaboration!', 'error');
        updateUI();
        return;
    }
    if (window.nestedDirs.length > 1 && window.nestedDirs[1] == 'commentables') {
        sweetAlert('Oops!', 'Cannot collaborate on a project in `commentables` directory.');
        updateUI();
        return;
    }
    var path = nestedDirs.slice(1).join('/');
    var msg = 'Copy this link to collaborate with others!';
    var id_token = auth2.currentUser.get().getAuthResponse().id_token;
    var data = new FormData();
    data.append('id_token', id_token);
    data.append('mode', window.buildMode);
    data.append('path', path);
    data.append('name', window.openProjectName);

    sendHttp('POST', 'collabShare', data, function(request) {
        if (request.status != 200) {
            sweetAlert('Oops!', 'Could not generate link for collaboration! Please try again.', 'error');
            return;
        }

        var a = document.createElement('a');
        a.href = window.location.href;
        a.hash = '#' + request.responseText;
        sweetAlert({
            html: true,
            title: '<i class="mdi mdi-72px mdi-account-multiple-plus"></i>&nbsp; Collaborate',
            text: msg,
            type: 'input',
            inputValue: a.href,
            showConfirmButton: false,
            showCancelButton: true,
            cancelButtonText: 'Done',
            animation: 'slide-from-bottom'
        });
    });
}

function addToCollaborate(hash) {
    var data = new FormData();
    data.append('mode', window.buildMode);
    data.append('collabHash', hash);

    function go() {
        var id_token = auth2.currentUser.get().getAuthResponse().id_token;
        data.append('id_token', id_token);
        sendHttp('POST', 'addToCollaborate', data, function(request) {
            if(request.status == 200) {
                initCodeworld();
                registerStandardHints(function(){setMode(true);});
                setCode('');
                nestedDirs = [""];
                allProjectNames = [[]];
                allFolderNames = [[]];
                discoverProjects("", 0);
                updateUI();
                sweetAlert('Success!', 'The commentable folder is moved into the specifed directory.', 'success');
                return;
            } else {
                if (request.status == 404) {
                    sweetAlert('Oops!', request.responseText, 'error');
                } else {
                    sweetAlert('Oops!', 'Could not add you to the file. Please try again!', 'error');
                }
                initCodeworld();
                registerStandardHints(function(){setMode(true);});
                discoverProjects("", 0);
                updateUI();
            }
        });
    }
    sweetAlert({
        title: 'Directory to store to:',
        type: 'input',
        showCancelButton: true,
        chowConfirmButton:true,
        confirmButtonText: 'Next',
        inputValue: '',
        closeOnConfirm: false
    }, function (path) {
        if ((path.startsWith('/commentables/') || path.startsWith('commentables/') || path == '/commentables' || path == 'commentables')) {
            return;
        }
        if (path[0] == '/') {
            path = path.slice(1);
        }
        data.append('path', path);
        sweetAlert({
            title: 'Name of the file',
            type: 'input',
            showCancelButton: true,
            showConfirmButton: true,
            confirmButtonText: 'Next',
            closeOnConfirm: false
        }, function (name) {
            if (name == undefined || name == '') {
                return;
            }
            data.append('name', name);
            sweetAlert({
                title: 'Choose a user name for this file:',
                type: 'input',
                showCancelButton: true,
                showConfirmButton: true,
                confirmButtonText: 'Add',
                closeOnConfirm: true
            }, function (userIdent) {
                if (userIdent == '' || userIdent == undefined) {
                    return;
                }
                data.append('userIdent', userIdent);
                    go();
            });
        });
    });
}

function listCurrentOwners() {
    if (!signedIn()) {
        sweetAlert('Oops!', 'You must sign in to see owners of this project!', 'error');
        updateUI();
        return;
    }
    if (window.openProjectName == '' || window.openProjectName == null) {
        sweetAlert('Oops!', 'You must select a project for seeing owners of this project!', 'error');
        updateUI();
        return;
    }
    if (window.nestedDirs.length > 1 && window.nestedDirs[1] == 'commentables') {
        sweetAlert('Oops!', 'No owner exists in a project in `commentables` directory.');
        updateUI();
        return;
    }
    var data = new FormData();
    data.append('id_token', auth2.currentUser.get().getAuthResponse().id_token);
    data.append('mode', window.buildMode);
    data.append('name', window.openProjectName);
    data.append('path', window.nestedDirs.slice(1).join('/'));

    sendHttp('POST', 'listCurrentOwners', data, function(request) {
        if (request.status != 200) {
            sweetAlert('Oops!', 'Could not load owners of the the file! Please try again.', 'error');
            return;
        }
        window.owners = JSON.parse(request.responseText);
        document.getElementById('listCurrentOwners').style.display = 'none';

        document.getElementById('newFolderButton').style.display = 'none';
        document.getElementById('newButton').style.display = 'none';
        document.getElementById('saveButton').style.display = 'none';
        document.getElementById('deleteButton').style.display = 'none';
        document.getElementById('downloadButton').style.display = 'none';
        document.getElementById('moveButton').style.display = 'none';
        document.getElementById('moveHereButton').style.display = 'none';
        document.getElementById('cancelButton').style.display = '';
        document.getElementById('copyButton').style.display = 'none';
        document.getElementById('copyHereButton').style.display = 'none';
        document.getElementById('runButtons').style.display = 'none';
        document.getElementById('testButton').style.display = 'none';
        document.getElementById('viewCommentVersions').style.display = 'none';
        document.getElementById('listCurrentOwners').style.display = 'none';

        var projects = document.getElementById('nav_mine');
        while (projects.lastChild) {
            projects.removeChild(projects.lastChild);
        }

        function isActiveUser(a) {
            if (window.activeOwners == undefined) {
                return a == window.userIdent;
            }
            return window.activeOwners.find(function(b) {
                return b == a;
            }) != undefined;
        }

        for(let i = 0; i < window.owners.length; i++) {
            var template = document.getElementById('projectTemplate').innerHTML;
            template = template.replace('{{label}}', window.owners[i]);
            template = template.replace(/{{ifactive ([^}]*)}}/, (!(isActiveUser(window.owners[i])) ? "$1" : ""));
            var span = document.createElement('span');
            span.innerHTML = template;
            var elem = span.getElementsByTagName('a')[0];
            projects.appendChild(span);
        }
    });
}

function initializeCollaboration() {
    if (!signedIn()) {
        sweetAlert('Oops!', 'Please sign in to open the file properly!', 'error');
        updateUI();
        return;
    }
    if (window.openProjectName == '' || window.openProjectName == null) {
        sweetAlert('Oops!', 'Please select a project to continue!', 'error');
        updateUI();
        return;
    }
    if (window.nestedDirs.length > 1 && window.nestedDirs[1] == "commentables") {
        sweetAlert('Oops!', 'An error occured. Please reload to fix it.', 'error');
        updateUI();
        return;
    }
    var id_token = auth2.currentUser.get().getAuthResponse().id_token;
    var url = window.location.hostname +
              ((window.location.port == '') ? '' : (':' + window.location.port)) +
              window.location.pathname +
              '?id_token=' + id_token +
              '&mode=' + window.buildMode +
              '&path=' + window.nestedDirs.slice(1).join('/') +
              '&name=' + window.openProjectName;

    var EditorClient = ot.EditorClient;
    var SocketIOAdapter = ot.SocketIOAdapter;
    var CodeMirrorAdapter = ot.CodeMirrorAdapter;

    window.socket = io.connect(url);

    socket.on('doc', function (obj) {
        go(obj.str, obj.revision, obj.clients, new SocketIOAdapter(socket));
    });

    socket.on('add_user', function(obj) {
        flag = 0;
        for (i = 0; i < window.activeOwners.length; i++) {
            if (window.activeOwners[i] == obj) {
                flag = 1;
                break;
            }
        }
        if (flag == 0) {
            window.activeOwners.push(obj);
        }
    });

    socket.on('delete_user', function(obj) {
        var temp = new Array();
        for (i = 0; i < window.activeOwners.length; i++) {
            if (window.activeOwners[i] == obj) {
                continue;
            } else {
                temp.push(window.activeOwners[i]);
            }
        }
        window.activeOwners = temp;
    });

    (function () {
        if (window.socket == undefined) {
            return;
        }
        var emit = socket.emit;
        var queue = [];
        socket.emit = function () {
            queue.push(arguments);
            return socket;
        };
        setInterval(function () {
            if (window.socket == undefined) {
                return;
            }
            if (queue.length) {
                emit.apply(socket, queue.shift());
            }
        }, 800);
    })();

    function go(str, revision, clients, serverAdapter) {
        window.codeworldEditor.setValue(str);
        window.cmClient = new EditorClient(
            revision, clients,
            serverAdapter, new CodeMirrorAdapter(window.codeworldEditor)
        );
        window.cmClient.serverAdapter.ownUserName = window.userIdent;
        window.activeOwners = clients;
    }
}
