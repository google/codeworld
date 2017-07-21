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

function checkForCommentHash() {
    var hash = window.location.hash.slice(1);
    if (hash.length > 0) {
        if (hash.slice(-2) == '==') {
            hash = hash.slice(0, -2);
        }
        if (hash[0] == 'C') {
            document.getElementById('askFeedbackButton').style.display = '';
            document.getElementById('stopButton').style.display = '';
            document.getElementById('compileButton').style.display = '';
        }
    }
    return;
}

function shareForFeedback() {
    if (!signedIn()) {
        sweetAlert('Oops!', 'You must sign in to ask for feedback!', 'error');
        updateUI();
        return;
    }
    if (openProjectName == '' || openProjectName == null) {
        var hash = window.location.hash.slice(1);
        if (hash.length > 0) {
            if (hash.slice(-2) == '==') {
                hash = hash.slice(0, -2);
            }
            if (hash[0] == 'C') {
                sweetAlert({
                    html: true,
                    title: '<i class="mdi mdi-72px mdi-comment-text-outline">&nbsp; Ask Feedback</i>',
                    text: msg,
                    type: 'input',
                    inputValue: window.location.href,
                    showConfirmButton: false,
                    showCancelButton: true,
                    cancelButtonText: 'Done',
                    animation: 'slide-from-bottom'
                });
            } else {
                sweetAlert('Oops!', 'You must select your project for feedback!', 'error');
                updateUI();
            }
        } else {
            sweetAlert('Oops!', 'You must select a project for feedback!', 'error');
            updateUI();
        }
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
    var hash = window.location.hash.slice(1);
    if (hash.length > 0) {
        if (hash.slice(-2) == '==') {
            hash = hash.slice(0, -2);
        }
        if (hash.length > 0 && hash[0] != 'C') {
            return;
        }
    } else if (openProjectName == null || openProjectName == '') {
        return;
    }
    doc = codeworldEditor.getDoc();
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
        return;
    }
    let comments = document.createElement('div');
    comments.classList.add('comments');
    let header = document.createElement('h2');
    header.innerText = 'Comments at Line ' + line;
    comments.appendChild(header);

    function go(request) {
        if (request.status != 200) {
            return;
        }
        var commentData = JSON.parse(request.responseText);
        if (commentData['lineNo'] !== line) {
            return;
        }
        window.openComments[line] = new Array();
        for (i in commentData['comments']) {
            window.openComments[line].push(commentData['comments'][i]);
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
    data.append('mode', window.buildMode);
    data.append('lineNo', line);
    if (window.chash != undefined) {
        data.append('chash', window.chash);
        sendHttp('POST', 'readComment', data, go);
    } else {
        data.append('id_token', auth2.currentUser.get().getAuthResponse().id_token);
        data.append('path', nestedDirs.slice(1).join('/'));
        data.append('name', openProjectName);
        sendHttp('POST', 'readOwnerComment', data, go);
    }
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
        return;
    }

    function go(request, commentDesc) {
        if (request.status != 200) {
            sweetAlert('Oops!', 'Could not comment. Please try again!', 'error');
            return;
        }
        var comments = window.openCommentLines[line].node;
        comments.getElementsByClassName('commentField')[0].value = '';
        window.openComments[line].push(commentDesc);
        comments.insertBefore(generateCommentBlock(comments.getElementsByClassName('commentBlock').length, line), comments.lastChild);
    }

    var data = new FormData();
    data.append('id_token', auth2.currentUser.get().getAuthResponse().id_token);
    data.append('lineNo', line);
    data.append('mode', window.buildMode);
    var commentDesc = new Object();
    commentDesc.comment = window.openCommentLines[line].node.getElementsByClassName('commentField')[0].value;
    if (commentDesc.comment == '') {
        return;
    }
    commentDesc.replies = [];
    commentDesc.userIdent = randomString();
    commentDesc.dateTime = (new Date()).toJSON();
    data.append('comment', JSON.stringify(commentDesc));
    if (window.chash != undefined){
        data.append('chash', window.chash);
        sendHttp('POST', 'writeComment', data, function(request) {
            go(request, commentDesc);
        });
    } else {
        data.append('path', nestedDirs.slice(1).join('/'));
        data.append('name', openProjectName);
        sendHttp('POST', 'writeOwnerComment', data, function(request) {
            go(request, commentDesc);
        });
    }
}

function writeReply(commentIdx, line) {
    if (!signedIn()) {
        sweetAlert('Oops!', 'You must sign in to write a reply.', 'error');
        return;
    }

    function go(request, replyDesc) {
        if (request.status != 200) {
            sweetAlert('Oops!', 'Could not reply. Please try again!', 'error');
            return;
        }
        var commentBlock = window.openCommentLines[line].node.getElementsByClassName('commentBlock')[commentIdx];
        commentBlock.getElementsByClassName('replyField')[0].value = '';
        window.openComments[line][commentIdx]['replies'].push(replyDesc);
        var replies = commentBlock.getElementsByClassName('replies')[0];
        replies.insertBefore(generateReplyBlock(replies.getElementsByClassName('replyBlock').length, commentIdx, line), replies.lastChild);
    }

    var data = new FormData();
    data.append('id_token', auth2.currentUser.get().getAuthResponse().id_token);
    data.append('lineNo', line);
    data.append('mode', window.buildMode);
    data.append('comment', JSON.stringify(window.openComments[line][commentIdx]));
    var replyDesc = new Object();
    replyDesc.reply = window.openCommentLines[line].node.getElementsByClassName('commentBlock')[commentIdx].getElementsByClassName('replyField')[0].value;
    if (replyDesc.reply == '') {
        return;
    }
    replyDesc.userIdent = randomString();
    replyDesc.dateTime = (new Date()).toJSON();
    data.append('reply', JSON.stringify(replyDesc));
    if (window.chash != undefined){
        data.append('chash', window.chash);
        sendHttp('POST', 'writeReply', data, function(request) {
            go(request, replyDesc);
        });
    } else {
        data.append('path', nestedDirs.slice(1).join('/'));
        data.append('name', openProjectName);
        sendHttp('POST', 'writeOwnerReply', data, function(request) {
            go(request, replyDesc);
        });
    }
}

function deleteComment(ind, line) {
    if (!signedIn()) {
        sweetAlert('Oops!', 'You must sign in to delete a comment.', 'error');
        return;
    }

    function go(request) {
        if (request.status != 200) {
            sweetAlert('Oops!', 'Could not delete the comment. Please try again!', 'error');
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
        }
    }

    var data = new FormData();
    data.append('id_token', auth2.currentUser.get().getAuthResponse().id_token);
    data.append('lineNo', line);
    data.append('mode', window.buildMode);
    data.append('comment', JSON.stringify(window.openComments[line][ind]));
    if (window.chash != undefined){
        data.append('chash', window.chash);
        sendHttp('POST', 'deleteComment', data, function(request) {
            go(request);
        });
    } else {
        data.append('path', nestedDirs.slice(1).join('/'));
        data.append('name', openProjectName);
        sendHttp('POST', 'deleteOwnerComment', data, function(request) {
            go(request);
        });
    }
}

function deleteReply(ind, commentIdx, line) {
    if (!signedIn()) {
        sweetAlert('Oops!', 'You must sign in to delete a reply.', 'error');
        return;
    }

    function go(request) {
        if (request.status != 200) {
            sweetAlert('Oops!', 'Could not delete the reply. Please try again!', 'error');
            return;
        }
        var comments = window.openCommentLines[line].node;
        var commentBlocks = comments.getElementsByClassName('commentBlock');
        var replies = commentBlocks[commentIdx].getElementsByClassName('replies')[0];
        var l = replies.getElementsByClassName('replyBlock').length;
        for (let i = l - 1; i >= ind; i--) {
            replies.removeChild(replies.getElementsByClassName('replyBlock')[i]);
        }
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
    data.append('lineNo', line);
    data.append('mode', window.buildMode);
    data.append('comment', JSON.stringify(window.openComments[line][commentIdx]));
    data.append('reply', JSON.stringify(window.openComments[line][commentIdx]['replies'][ind]));
    if (window.chash != undefined){
        data.append('chash', window.chash);
        sendHttp('POST', 'deleteReply', data, function(request) {
            go(request);
        });
    } else {
        data.append('path', nestedDirs.slice(1).join('/'));
        data.append('name', openProjectName);
        sendHttp('POST', 'deleteOwnerReply', data, function(request) {
            go(request);
        });
    }
}
/*
function shiftLineByX(lineNo, x) {
    if (!signedIn()) {
        sweetAlert('Oops!', 'Please sign in to continue, otherwise the comments in the file will be misplaced.', 'error');
        return;
    }
    if (openProjectName == null || openProjectName == '') {
        return;
    }
    if (window.openCommentLines != undefined) {
        return;
    }
    console.log(lineNo)
    if (window.currentShift != undefined) {
        if (window.pendingShifts == undefined) {
            window.pendingShifts = [[],[]];
        }
        if (openProjectName != window.currentShiftFile || nestedDirs.slice(1).join('/') != window.currentShiftDir) {
            return;
        }
        window.pendingShifts[0].push(lineNo);
        window.pendingShifts[1].push(x);
    } else {
        window.currentShift = [[lineNo], [x]];
        window.currentShiftFile = openProjectName;
        window.currentShiftDir = nestedDirs.slice(1).join('/');
        var data = new FormData();
        data.append('id_token', auth2.currentUser.get().getAuthResponse().id_token);
        data.append('mode', window.buildMode);
        data.append('path', window.currentShiftDir);
        data.append('name', window.currentShiftFile);
        data.append('shifts', JSON.stringify([[lineNo], [x]]));
        sendHttp('POST', 'shiftLinesByXs', data, function(request) {
            if (request.status != 200) {
                sweetAlert('Oops!', 'Could not update comments according to the new line changes! Reverting back to previous version.', 'error');
                revertBack();
                return;
            }
            if (window.pendingShifts != undefined) {
                var data = new FormData();
                data.append('id_token', auth2.currentUser.get().getAuthResponse().id_token);
                data.append('mode', window.buildMode);
                data.append('path', window.currentShiftDir);
                data.append('name', window.currentShiftFile);
                data.append('shifts', JSON.stringify(window.pendingShifts));
                window.currentShift = window.pendingShifts;
                window.pendingShifts = undefined;
                sendHttp('POST', 'shiftLinesByXs', data, function(request) {
                    if (request.status != 200) {
                        sweetAlert('Oops!', 'Could not update comments according to the new line changes!', 'error');
                        return;
                    }
                    window.currentShift = undefined;
                    window.currentShiftFile = undefined;
                    window.currentShiftDir = undefined;
                });

            } else {
                window.currentShift = undefined;
                window.currentShiftFile = undefined;
                window.currentShiftDir = undefined;
            }
        });
    }
}

function revertBack() {

}*/

function randomString(length = 32, chars = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ') {
    var result = '';
    for (var i = length; i > 0; i--) {
        result += chars[Math.floor(Math.random() * chars.length)];
    }
    return result;
}
