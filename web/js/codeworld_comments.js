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

function getCommentVersions() {
    if (!signedIn()) {
        sweetAlert('Oops!', 'Could not load previous comment versions.', 'error');
        updateUI();
        return;
    }
    var id_token = auth2.currentUser.get().getAuthResponse().id_token;
    var data = new FormData();
    data.append('id_token', id_token);
    data.append('mode', window.buildMode);
    data.append('path', window.nestedDirs.slice(1).join('/'));
    data.append('name', window.openProjectName);
    var handler = (window.nestedDirs.length > 1 && window.nestedDirs[1] == "commentables") ? 'listVersions' : 'listOwnerVersions';

    sendHttp('POST', handler, data, function(request) {
        if (request.status != 200) {
            sweetAlert('Oops!', 'Could not load previous comment versions', 'error');
            return;
        }
        var versions = JSON.parse(request.responseText);
        function sortNumber(a, b) {
            return parseInt(b) - parseInt(a);
        }
        addCommentVersions(versions.sort(sortNumber));
        updateUI();
    });
}

function addCommentVersions(versions) {
    document.getElementById('viewCommentVersions').style.display = '';
    window.maxVersion = parseInt(versions[0]);
    window.currentVersion = parseInt(versions[0]);
    return;
}

function viewCommentVersions() {
    if (window.openProjectName == '' || window.openProjectName == null) {
        updateUI();
        return;
    }
    if (window.currentVersion == undefined || window.maxVersion == undefined) {
        updateUI();
        return;
    }
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

    var projects = document.getElementById('nav_mine');
    while (projects.lastChild) {
        projects.removeChild(projects.lastChild);
    }

    for(let i = 0; i <= window.maxVersion; i++) {
        var template = document.getElementById('projectTemplate').innerHTML;
        template = template.replace('{{label}}', 'Version ' + i + ((i != window.maxVersion) ? ' (ReadOnly)' : ''));
        template = template.replace(/{{ifactive ([^}]*)}}/, (i == window.currentVersion ? "$1" : ""));
        var span = document.createElement('span');
        span.innerHTML = template;
        var elem = span.getElementsByTagName('a')[0];
        elem.onclick = function() {
            loadCommentVersionSource(i);
        };
        projects.appendChild(span);
    }
}

function loadCommentVersionSource(idx) {
    warnIfUnsaved(function () {
        if (!signedIn()) {
            sweetALert('Oops!', 'You must sign in to see the source!', 'error');
            updateUI();
            return;
        }
        if (window.currentVersion == undefined) {
            return;
        }
        var data = new FormData();
        var id_token = auth2.currentUser.get().getAuthResponse().id_token;
        data.append('id_token', id_token);
        data.append('mode', window.buildMode);
        data.append('name', window.openProjectName);
        data.append('path', window.nestedDirs.slice(1).join('/'));
        data.append('versionNo', idx);
        var handler = (window.nestedDirs.length > 1 && window.nestedDirs[1] == "commentables") ? 'viewCommentSource' : 'viewOwnerCommentSource';
        sendHttp('POST', handler, data, function (request) {
            if (request.status != 200) {
                sweetAlert('Oops!', 'Could not load the source of this version. Please try again!', 'error');
                updateUI();
                return;
            }
            var doc = codeworldEditor.getDoc();
            window.currentVersion  = idx;
            if (window.currentVersion == window.maxVersion) {
                window.codeworldEditor.setOption('readOnly', false);
            } else {
                window.codeworldEditor.setOption('readOnly', true);
            }
            doc.setValue(request.responseText);
            updateUI();
        });
        return;
    }, false);
}

function loadProjectForComments(index, name, buildMode, successFunc) {
    var data = new FormData();
    data.append('id_token', auth2.currentUser.get().getAuthResponse().id_token);
    data.append('name', name);
    data.append('mode', buildMode);
    data.append('path', window.nestedDirs.slice(1, index + 1).join('/'));
    sendHttp('POST', 'listVersions', data, function(request) {
        if (request.status != 200) {
            sweetAlert('Oops!', 'Something went wrong. Please try again!', 'error');
            return;
        }
        var versions = JSON.parse(request.responseText);
        function sortNumber(a,b) {
            return parseInt(b) - parseInt(a);
        }
        versions.sort(sortNumber);
        data.append('versionNo', versions[0]);
        sendHttp('POST', 'viewCommentSource', data, function(request) {
            if (request.status != 200) {
                sweetAlert('Oops!', 'Something went wrong. Please try again!', 'error');
                return;
            }
            window.project = {
                'source': request.responseText,
                'name': name
            };
            window.nestedDirs = window.nestedDirs.slice(0, index + 1);
            window.allProjectNames = window.allProjectNames.slice(0, index + 1);
            window.allFolderNames = window.allFolderNames.slice(0, index + 1);
            setCode(project.source, project.history, name);
            updateUI();
            addCommentVersions(versions);
        });
    });
}

function addSharedComment(hash) {
    var data = new FormData();
    data.append('mode', window.buildMode);
    data.append('chash', hash);

    function go() {
        var id_token = auth2.currentUser.get().getAuthResponse().id_token;
        data.append('id_token', id_token);
        sendHttp('POST', 'addSharedComment', data, function(request) {
            if(request.status == 200) {
                initCodeworld();
                registerStandardHints(function(){setMode(true);});
                setCode('');
                nestedDirs = [""];
                allProjectNames = [[]];
                allFolderNames = [[]];
                discoverProjects("", 0);
                updateUI();
                sweetAlert('Success!', 'The shared folder is moved into the specifed directory.', 'success');
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

    // improve upon this UI
    sweetAlert({
        title: 'Path to store to(relative with respect to commentables):',
        type: 'input',
        showCancelButton: true,
        showConfirmButton: true,
        comfirmButtonText: 'Next',
        inputValue: '/commentables/',
        closeOnConfirm: false
    }, function (path) {
        if (path == undefined || path == '') {
            return;
        }
        if (!(path.startsWith('/commentables/') || path.startsWith('commentables/') || path == '/commentables' || path == 'commentables')) {
            if (path[0] == '/') {
                path = 'commentables' + path;
            } else {
                path = 'commentables/' + path;
            }
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

function shareForFeedback() {
    if (!signedIn()) {
        sweetAlert('Oops!', 'You must sign in to ask for feedback!', 'error');
        updateUI();
        return;
    }
    if (openProjectName == '' || openProjectName == null) {
        sweetAlert('Oops!', 'You must select a project for feedback!', 'error');
        updateUI();
        return;
    }
    var path = nestedDirs.slice(1).join('/');
    var msg = 'Copy this link to ask for feedback from others!';
    var id_token = auth2.currentUser.get().getAuthResponse().id_token;
    var data = new FormData();
    data.append('id_token', id_token);
    data.append('mode', window.buildMode);
    data.append('path', path);
    data.append('name', openProjectName);

    sendHttp('POST', 'commentShare', data, function(request) {
        if (request.status != 200) {
            sweetAlert('Oops!', 'Could not generate link for feedback! Please try again.', 'error');
            return;
        }

        var a = document.createElement('a');
        a.href = window.location.href;
        a.hash = '#' + request.responseText;
        sweetAlert({
            html: true,
            title: '<i class="mdi mdi-72px mdi-comment-text-outline">&nbsp; Ask Feedback</i>',
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

function addPresentCommentInd() {
   /* if (!signedIn()) {
        sweelAlert('Oops!', 'You must sign in to see and write comments!', 'error');
        return;
    }

    function go(request) {
        if (request.status != 200) {
            sweetAlert('Oops!', 'Sorry! Could not load an indicator of where comments are present.', 'error');
            return;
        }
        window.lineSet = new Set(JSON.parse(request.responseText));
        for (i of lineSet) {
            document.getElementsByClassName('CodeMirror-gutter-elt')[Number(i) + 1].innerHTML = '<i style="color: #8642f4;">c</i>&nbsp;' + i;
        }
        if (window.lineSet.size !== 0) {
            var w = document.getElementsByClassName('CodeMirror-gutter')[0].style.width.slice(0, -2);
            document.getElementsByClassName('CodeMirror-gutter')[0].style.width = (Number(w) + 2) + 'px';
        }
    }

    var data = new FormData();
    data.append('mode', window.buildMode);
    data.append('id_token', auth2.currentUser.get().getAuthResponse().id_token);
    if (window.chash != undefined){
        data.append('chash', window.chash);
        sendHttp('POST', 'listComments', data, function(request) {
            go(request);
        });
    } else {
        data.append('path', nestedDirs.slice(1).join('/'));
        data.append('name', openProjectName);
        sendHttp('POST', 'listOwnerComments', data, function(request) {
            go(request);
        });
    }*/
}

function toggleUserComments(cm, line, gutter) {
    if (window.openProjectName == null || window.openProjectName == '') {
        return;
    }
    doc = window.codeworldEditor.getDoc();
    if (window.openCommentLines == undefined) {
        window.openCommentLines = new Object();
        window.openComments = new Object();
    }
    if (window.openCommentLines[line + 1] != undefined) {
        if (window.openProjectName == window.openCommentLines[line + 1].currentProject) {
            if (window.nestedDirs.join('/') == window.openCommentLines[line + 1].currentDir) {
                window.openCommentLines[line + 1].clear();
                window.openCommentLines[line + 1] = undefined;
                window.openComments[line + 1] = undefined;
                return;
            }
        }
    }
    generateComments(line + 1);
}

function generateComments(line) {
    if (!signedIn()) {
        sweetAlert('Oops!', 'You must sign in to see comments.', 'error');
        updateUI();
        return;
    }
    if (window.currentVersion == undefined) {
        sweetAlert('Oops!', 'Something went wrong. Please try again!', 'error');
        updateUI();
        return;
    }
    let comments = document.createElement('div');
    comments.classList.add('comments');
    let header = document.createElement('h2');
    header.innerText = 'Comments at Line ' + line;
    comments.appendChild(header);

    function go(request) {
        if (request.status != 200) {
            if (request.status == 404) {
                sweetAlert('Oops!', request.responseText, 'error');
            } else {
                sweetAlert('Oops!', 'Something went wrong. Please try again!', 'error');
            }
            return;
        }
        var commentData = JSON.parse(request.responseText);
        window.openComments[line] = new Array();
        for (i in commentData) {
            window.openComments[line].push(commentData[i]);
            comments.appendChild(generateCommentBlock(i, line));
        }
        comments.appendChild(generateCommentArea(line));
        $(comments).fadeIn('slow');
        window.openCommentLines[line] = doc.addLineWidget(line - 1, comments, {
            coverGutter: true
        });
        window.openCommentLines[line].currentProject = window.openProjectName;
        window.openCommentLines[line].currentDir = window.nestedDirs.join('/');
    }

    var data = new FormData();
    data.append('id_token', auth2.currentUser.get().getAuthResponse().id_token);
    data.append('mode', window.buildMode);
    data.append('lineNo', line);
    data.append('versionNo', window.currentVersion);
    data.append('path', nestedDirs.slice(1).join('/'));
    data.append('name', openProjectName);
    var handler = (window.nestedDirs.length > 1 && window.nestedDirs[1] == 'commentables') ? 'readComment' : 'readOwnerComment';
    sendHttp('POST', handler, data, go);
}

function toggleReplies(ind, line) {
    var commentBlock = window.openCommentLines[line].node.getElementsByClassName('commentBlock')[ind];
    if (commentBlock.getElementsByClassName('showTheReplies')[0].innerHTML == 'show replies...') {
        commentBlock.getElementsByClassName('showTheReplies')[0].innerHTML = 'hide replies...';
        replies = document.createElement('div');
        replies.classList.add('replies');
        for (i in window.openComments[line][ind]['replies']) {
            replies.appendChild(generateReplyBlock(i, ind, line));
        }
        replies.appendChild(generateReplyArea(ind, line));
        commentBlock.appendChild(replies);
    } else {
        commentBlock.removeChild(commentBlock.lastChild);
        commentBlock.getElementsByClassName('showTheReplies')[0].innerHTML = 'show replies...';
    }
}

function generateCommentBlock(ind, line) {
    let commentBlock = document.createElement('div');
    commentBlock.classList.add('commentBlock');
    commentBlock.appendChild(generateCommentDiv(ind, line));
    let showRepliesButton = document.createElement('span');
    showRepliesButton.classList.add('showTheReplies');
    showRepliesButton.setAttribute('onclick', 'toggleReplies(' + ind + ', ' + line + ')');
    showRepliesButton.innerHTML = 'show replies...';
    commentBlock.appendChild(showRepliesButton);
    return commentBlock;
}

function generateCommentDiv(ind, line) {
    let commentDiv = document.createElement('div');
    let info = document.createElement('p');
    info.classList.add('commentInfo');
    info.innerHTML = '<span class="user">' + window.openComments[line][ind]['userIdent'] + '</span>';
    let timeInfo = document.createElement('time');
    timeInfo.setAttribute('datetime', window.openComments[line][ind]['dateTime']);
    timeInfo.innerHTML = '&nbsp;' + (new Date(window.openComments[line][ind]['dateTime'])).toString();
    info.appendChild(timeInfo);
    let deleteButton = document.createElement('span');
    deleteButton.setAttribute('onclick', 'deleteComment(' + ind + ', ' + line + ', ' + ')');
    deleteButton.classList.add('deleteTheComment');
    deleteButton.innerHTML = 'delete';
    info.appendChild(deleteButton);
    commentDiv.appendChild(info);
    let commentInfo = document.createElement('div');
    commentInfo.classList.add('markdown');
    commentInfo.classList.add('comment');
    commentInfo.innerHTML = '&nbsp;&nbsp;' + window.openComments[line][ind]['comment'];
    commentDiv.appendChild(commentInfo);
    return commentDiv;
}

function generateReplyBlock(ind, commentIdx, line) {
    let replyBlock = document.createElement('div');
    replyBlock.classList.add('replyBlock');
    replyBlock.appendChild(generateReplyDiv(ind, commentIdx, line));
    return replyBlock;
}

function generateReplyDiv(ind, commentIdx, line) {
    let replyDiv = document.createElement('div');
    let info = document.createElement('p');
    info.classList.add('replyInfo');
    info.innerHTML = '<span class="user">' + window.openComments[line][commentIdx]['replies'][ind]['userIdent'] + '</span>';
    let timeInfo = document.createElement('time');
    timeInfo.setAttribute('datetime', window.openComments[line][commentIdx]['replies'][ind]['dateTime']);
    timeInfo.innerHTML = '&nbsp;' + (new Date(window.openComments[line][commentIdx]['replies'][ind]['dateTime'])).toString();
    info.appendChild(timeInfo);
    let deleteButton = document.createElement('span');
    deleteButton.setAttribute('onclick', 'deleteReply(' + ind + ', ' + commentIdx + ', ' + line + ')');
    deleteButton.classList.add('deleteTheReply');
    deleteButton.innerHTML = 'delete';
    info.appendChild(deleteButton);
    replyDiv.appendChild(info);
    let replyInfo = document.createElement('div');
    replyInfo.classList.add('markdown');
    replyInfo.classList.add('reply');
    replyInfo.innerHTML = '&nbsp;&nbsp;' + window.openComments[line][commentIdx]['replies'][ind]['reply'];
    replyDiv.appendChild(replyInfo);
    return replyDiv;
}

function generateCommentArea(line) {
    let commentArea = document.createElement('div');
    commentArea.innerHTML = '<textarea class="commentField"></textarea>';
    let submitArea = document.createElement('div');
    let submitButton = document.createElement('a');
    submitButton.classList.add('cw-button');
    submitButton.classList.add('blue');
    submitButton.classList.add('writeTheComment');
    submitButton.setAttribute('onclick', 'writeComment(' + line + ')');
    submitButton.innerText = 'Write Comment';
    submitArea.appendChild(submitButton);
    commentArea.appendChild(submitArea);
    return commentArea;
}

function generateReplyArea(commentIdx, line) {
    let replyArea = document.createElement('div');
    replyArea.innerHTML = '<textarea class="replyField"></textarea>';
    let submitArea = document.createElement('div');
    let submitButton = document.createElement('a');
    submitButton.classList.add('cw-button');
    submitButton.classList.add('blue');
    submitButton.classList.add('writeTheReply');
    submitButton.setAttribute('onclick', 'writeReply(' + commentIdx + ', ' + line + ')');
    submitButton.innerText = 'Write Reply';
    submitArea.appendChild(submitButton);
    replyArea.appendChild(submitArea);
    return replyArea;
}

function writeComment(line) {
    if (!signedIn()) {
        sweetAlert('Oops!', 'You must sign in to write a comment.', 'error');
        updateUI();
        return;
    }
    if (window.currentVersion == undefined || window.maxVersion == undefined) {
        updateUI();
        return;
    }

    function go(request, comment, dateTime) {
        if (request.status != 200) {
            if (request.status == 404) {
                sweetAlert('Oops!', request.responseText, 'error');
            } else {
                sweetAlert('Oops!', 'Could not comment. Please try again!', 'error');
            }
            return;
        }
        function goAgain(userIdent) {
            var comments = window.openCommentLines[line].node;
            comments.getElementsByClassName('commentField')[0].value = '';
            window.openComments[line].push({
                'comment': comment,
                'replies': [],
                'userIdent': userIdent,
                'dateTime': dateTime,
                'status': 'present'
            });
            comments.insertBefore(generateCommentBlock(comments.getElementsByClassName('commentBlock').length, line), comments.lastChild);
        }
        if (window.nestedDirs.length > 1 && window.nestedDirs[1] == 'commentables') {
            var data = new FormData();
            data.append('id_token', auth2.currentUser.get().getAuthResponse().id_token);
            data.append('mode', window.buildMode);
            data.append('path', window.nestedDirs.slice(1).join('/'));
            data.append('name', window.openProjectName);
            sendHttp('POST', 'getUserIdent', data, function(request) {
                if (request.status != 200) {
                    if (request.status == 404) {
                        sweetAlert('Oops!', request.responseText, 'error');
                    } else {
                        sweetAlert('Oops!', 'Something went wrong. Please reload the project!', 'error');
                    }
                    return;
                }
                goAgain(request.responseText);
            });
        } else {
            goAgain('Anonymous Owner');
        }
    }

    var data = new FormData();
    data.append('id_token', auth2.currentUser.get().getAuthResponse().id_token);
    data.append('mode', window.buildMode);
    data.append('path', window.nestedDirs.slice(1).join('/'));
    data.append('name', window.openProjectName);
    data.append('lineNo', line);
    data.append('versionNo', window.currentVersion);
    var comment = window.openCommentLines[line].node.getElementsByClassName('commentField')[0].value;
    var dateTime = (new Date()).toJSON();
    if (comment == '') {
        return;
    }
    data.append('comment', comment);
    data.append('dateTime', JSON.stringify(dateTime));
    var handler = (window.nestedDirs.length > 1 && window.nestedDirs[1] == 'commentables') ? 'writeComment' : 'writeOwnerComment';
    sendHttp('POST', handler, data, function(request) {
        go(request, comment, dateTime);
    });
}

function writeReply(commentIdx, line) {
    if (!signedIn()) {
        sweetAlert('Oops!', 'You must sign in to write a reply.', 'error');
        updateUI();
        return;
    }
    if (window.currentVersion == undefined || window.maxVersion == undefined) {
        updateUI();
        return;
    }

    function go(request, reply, dateTime) {
        if (request.status != 200) {
            if (request.status == 404) {
                sweetAlert('Oops!', request.responseText, 'error');
            } else {
                sweetAlert('Oops!', 'Could not reply. Please try again!', 'error');
            }
            return;
        }
        function goAgain(userIdent) {
            var commentBlock = window.openCommentLines[line].node.getElementsByClassName('commentBlock')[commentIdx];
            commentBlock.getElementsByClassName('replyField')[0].value = '';
            window.openComments[line][commentIdx]['replies'].push({
                'reply': reply,
                'dateTime': dateTime,
                'userIdent': userIdent,
                'status': 'present'
            });
            var replies = commentBlock.getElementsByClassName('replies')[0];
            replies.insertBefore(generateReplyBlock(replies.getElementsByClassName('replyBlock').length, commentIdx, line), replies.lastChild);
        }
        if (window.nestedDirs.length > 1 && window.nestedDirs[1] == 'commentables') {
            var data = new FormData();
            data.append('id_token', auth2.currentUser.get().getAuthResponse().id_token);
            data.append('mode', window.buildMode);
            data.append('path', window.nestedDirs.slice(1).join('/'));
            data.append('name', window.openProjectName);
            sendHttp('POST', 'getUserIdent', data, function(request) {
                if (request.status != 200) {
                    if (request.status == 404) {
                        sweetAlert('Oops!', request.responseText, 'error');
                    } else {
                        sweetAlert('Oops!', 'Something went wrong. Please reload the project!', 'error');
                    }
                    return;
                }
                goAgain(request.responseText);
            });
        } else {
            goAgain('Anonymous Owner');
        }
    }

    var data = new FormData();
    data.append('id_token', auth2.currentUser.get().getAuthResponse().id_token);
    data.append('mode', window.buildMode);
    data.append('lineNo', line);
    data.append('versionNo', window.currentVersion);
    data.append('path', window.nestedDirs.slice(1).join('/'));
    data.append('name', window.openProjectName);
    data.append('comment', JSON.stringify(window.openComments[line][commentIdx]));
    var reply = window.openCommentLines[line].node.getElementsByClassName('commentBlock')[commentIdx].getElementsByClassName('replyField')[0].value;
    var dateTime = (new Date()).toJSON();
    if (reply == '') {
        return;
    }
    data.append('reply', reply);
    data.append('dateTime', JSON.stringify(dateTime));
    var handler = (window.nestedDirs.length > 1 && window.nestedDirs[1] == 'commentables') ? 'writeReply' : 'writeOwnerReply';
    sendHttp('POST', handler, data, function(request) {
        go(request, reply, dateTime);
    });
}

function deleteComment(ind, line) {
    if (!signedIn()) {
        sweetAlert('Oops!', 'You must sign in to delete a comment.', 'error');
        updateUI();
        return;
    }
    if (window.currentVersion == undefined || window.maxVersion == undefined) {
        updateUI();
        return;
    }

    function go(request) {
        if (request.status != 200) {
            if (request.status == 404) {
                sweetAlert('Oops!', request.responseText, 'error');
            }else {
                sweetAlert('Oops!', 'Could not delete the comment. Please try again!', 'error');
            }
            return;
        }
        var comments = window.openCommentLines[line].node;
        var commentBlocks = comments.getElementsByClassName('commentBlock');
        var l = window.openComments[line][ind]['replies'].length;
        if (l == 0) {
            var l = commentBlocks.length;
            for (let i = l - 1; i >= ind; i--) {
                comments.removeChild(commentBlocks[i]);
            }
            window.openComments[line].splice(ind, 1);
            for (let i = ind; i < l; i++) {
                if (i != ind) {
                    comments.insertBefore(generateCommentBlock(i - 1, line), comments.lastChild);
                }
            }
        } else {
            commentBlocks[ind].getElementsByClassName('user')[0].innerHTML = 'none';
            commentBlocks[ind].getElementsByClassName('comment')[0].innerHTML = '&nbsp;&nbsp;deleted';
            window.openComments[line][ind]['userIdent'] = 'none';
            window.openComments[line][ind]['comment'] = 'deleted';
            window.openComments[line][ind]['status'] = 'deleted';
        }
    }

    var data = new FormData();
    data.append('id_token', auth2.currentUser.get().getAuthResponse().id_token);
    data.append('mode', window.buildMode);
    data.append('lineNo', line);
    data.append('versionNo', window.currentVersion);
    data.append('path', window.nestedDirs.slice(1).join('/'));
    data.append('name', window.openProjectName);
    data.append('comment', JSON.stringify(window.openComments[line][ind]));
    var handler = (window.nestedDirs.length > 1 && window.nestedDirs[1] == 'commentables') ? 'deleteComment' : 'deleteOwnerComment';
    sendHttp('POST', handler, data, function(request) {
        go(request);
    });
}

function deleteReply(ind, commentIdx, line) {
    if (!signedIn()) {
        sweetAlert('Oops!', 'You must sign in to delete a reply.', 'error');
        updateUI();
        return;
    }
    if (window.currentVersion == undefined || window.maxVersion == undefined) {
        updateUI();
        return;
    }

    function go(request) {
        if (request.status != 200) {
            if (request.status == 404) {
                sweetAlert('Oops!', request.responseText, 'error');
            } else {
                sweetAlert('Oops!', 'Could not delete the reply. Please try again!', 'error');
            }
            return;
        }
        var comments = window.openCommentLines[line].node;
        var commentBlocks = comments.getElementsByClassName('commentBlock');
        var replies = commentBlocks[commentIdx].getElementsByClassName('replies')[0];
        var l = replies.getElementsByClassName('replyBlock').length;
        for (let i = l - 1; i >= ind; i--) {
            replies.removeChild(replies.getElementsByClassName('replyBlock')[i]);
        }
        window.openComments[line][commentIdx]['replies'][ind]['status'] = 'deleted';
        window.openComments[line][commentIdx]['replies'].splice(ind, 1);
        for (let i = ind; i < l; i++) {
            if (i != ind)
                replies.insertBefore(generateReplyBlock(i - 1, commentIdx, line), replies.lastChild);
        }
        if (l == 1) {
            if (window.openComments[line][commentIdx]['userIdent'] == 'none') {
                var l1 = commentBlocks.length;
                for (let i = l1 - 1; i >= commentIdx; i--) {
                    comments.removeChild(commentBlocks[i]);
                }
                window.openComments[line].splice(commentIdx, 1);
                for (let i = commentIdx; i < l1; i++) {
                    if (i != commentIdx) {
                        comments.insertBefore(generateCommentBlock(i - 1, line), comments.lastChild);
                    }
                }
            }
        }
    }

    var data = new FormData();
    data.append('id_token', auth2.currentUser.get().getAuthResponse().id_token);
    data.append('mode', window.buildMode);
    data.append('lineNo', line);
    data.append('versioNo', window.currentVersion);
    data.append('comment', JSON.stringify(window.openComments[line][commentIdx]));
    data.append('reply', JSON.stringify(window.openComments[line][commentIdx]['replies'][ind]));
    data.append('path', nestedDirs.slice(1).join('/'));
    data.append('name', openProjectName);
    var handler = (window.nestedDirs.length > 1 && window.nestedDirs[1] == 'commentables') ? 'deleteReply' : 'deleteOwnerReply';
    sendHttp('POST', handler, data, function(request) {
        go(request);
    });
}

function generateTestEnv() {
    warnIfUnsaved(function() {
        if (!signedIn()) {
            sweetAlert('Oops!', 'You need to login to test the code.', 'error');
            updateUI();
            return;
        }
        if (window.currentVersion == undefined || window.maxVersion == undefined) {
            updateUI();
            return;
        }
        if (!(window.nestedDirs.length > 1 && window.nestedDirs[1] == 'commentables')) {
            if (window.currentVersion == window.maxVersion) {
                updateUI();
                return;
            }
        }
        if (openProjectName == '' || openProjectName == null) {
            updateUI();
            return;
        }
        window.testEnv = new Object();
        window.testEnv.project = getCurrentProject()['source'];
        window.testEnv.prevName = window.openProjectName;
        window.openProjectName = null;
        document.getElementById('newFolderButton').style.display = 'none';
        document.getElementById('newButton').style.display = 'none';
        document.getElementById('saveButton').style.display = 'none';
        document.getElementById('testButton').style.display = 'none';
        document.getElementById('deleteButton').style.display = 'none';
        document.getElementById('downloadButton').style.display = '';
        document.getElementById('copyButton').style.display = 'none';
        document.getElementById('copyHereButton').style.display = 'none';
        document.getElementById('moveButton').style.display = 'none';
        document.getElementById('moveHereButton').style.display = 'none';
        document.getElementById('cancelButton').style.display = '';
        document.getElementById('viewCommentVersions').style.display = 'none';
        var projects = document.getElementById('nav_mine');
        while (projects.lastChild) {
            projects.removeChild(projects.lastChild);
        }
        document.getElementById('cancelButton').onclick = function() {
            document.getElementById('cancelButton').onclick = function() {
                updateUI();
            };
            window.openProjectName = window.testEnv.prevName;
            var doc = window.codeworldEditor.getDoc();
            doc.setValue(window.testEnv.project);
            window.testEnv = undefined;
            window.location.hash = '#';
            updateUI();
        };
        var doc = window.codeworldEditor.getDoc();
        doc.setValue(window.testEnv.project);
        window.codeworldEditor.setOption('readOnly', false);
        doc.clearHistory();
    }, false);
}
