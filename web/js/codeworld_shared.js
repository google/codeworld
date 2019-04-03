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
    const sendHttpFunc = signedIn() ? window.auth2.sendHttpAuth : sendHttpRaw;
    return sendHttpFunc(method, url, body, callback);
}

function sendHttpRaw(method, url, body, callback) {
    const request = new XMLHttpRequest();

    if (callback) {
        request.onreadystatechange = () => {
            if (request.readyState === 4) callback(request);
        };
    }

    request.open(method, url, true);
    request.send(body);

    return request;
}

const Html = (() => {
    const mine = {};

    mine.encode = str => $('<div/>').text(str).html();

    return mine;
})();

const Alert = (() => {
    const mine = {};

    mine.init = () =>
        Promise.resolve($.getScript(
            'https://cdnjs.cloudflare.com/ajax/libs/limonte-sweetalert2/7.19.2/sweetalert2.all.min.js'
        ))
            .catch(e => console.log('Alert.init failed'));

    // Build SweetAlert title HTML
    mine.title = (text, iconClass) =>
        `<i class="mdi mdi-72px ${iconClass}"></i>&nbsp; ${Html.encode(text)}`;

    return mine;
})();

const hintBlacklist = [
    // Symbols that only exist to implement RebindableSyntax or map to
    // built-in Haskell types.
    'Bool',
    'IO',
    'fail',
    'fromCWText',
    'fromDouble',
    'fromInt',
    'fromInteger',
    'fromRational',
    'fromString',
    'ifThenElse',
    'toCWText',
    'toDouble',
    'toInt',

    // Deprecated exports.
    'path',
    'thickPath',
    'text',
    'styledText',

    // Old-style colors.
    'White',
    'Black',
    'Gray',
    'Grey',
    'Red',
    'Orange',
    'Yellow',
    'Green',
    'Blue',
    'Purple',
    'Pink',
    'Brown',
    'cyan',
    'magenta',
    'azure',
    'chartreuse',
    'aquamarine',
    'violet',
    'rose'
];

// codeWorldSymbols is variable containing annotations and documentation
// of builtin and user-defined variables.
// Expected format:
// codeWorldSymbols = {
//   codeWorldLogo: {
//     declaration: "codeWorldLogo :: Picture",
//     symbolStart: 0,
//     symbolEnd: 13,
//     doc: "The CodeWorld logo."
//   }
// }
window.codeWorldSymbols = {};
window.codeWorldBuiltinSymbols = {};

function getWordStart(word, line) {
    return line.indexOf(word);
}

function getWordEnd(word, line) {
    const wordStart = getWordStart(word, line);
    if (wordStart !== -1) {
        return wordStart + word.length;
    }
    return -1;
}

function parseSymbolsFromCurrentCode() {
    const lines = window.codeworldEditor.getValue().split('\n');
    const parseResults = {};
    let lineIndex = 0;

    lines.forEach(line => {
        lineIndex++;

        const docString = `Defined in your code on line ${ 
            lineIndex.toString()}.`;
        if (/^\w+\(.*/.test(line)) {
            // f(x, y) =
            const word = line.split('(')[0].trim();
            if (parseResults[word]) return;
            parseResults[word] = {
                declaration: word,
                doc: docString
            };
        } else if (/^\S+\s*=/.test(line)) {
            // foo =
            const word = line.split('=')[0].trim();
            if (parseResults[word]) return;
            parseResults[word] = {
                declaration: word,
                doc: docString
            };
        } else if (/^data\s.+/.test(line)) {
            // data Foo
            const match = /^data\s+(\S+)\b.*/.exec(line);
            const word = match[1];
            if (parseResults[word]) return;
            parseResults[word] = {
                declaration: line.slice(0, getWordEnd(word, line)),
                symbolStart: getWordStart(word, line),
                symbolEnd: getWordEnd(word, line),
                doc: docString
            };
        } else if (/^type\s.+/.test(line)) {
            // type Foo = Bar
            const match = /^type\s+(\S+\b).*/.exec(line);
            const word = match[1];
            if (parseResults[word]) return;
            parseResults[word] = {
                declaration: line,
                symbolStart: getWordStart(word, line),
                symbolEnd: getWordEnd(word, line),
                doc: docString
            };
        } else if (/^\([^()]+\)\s*::/.test(line)) {
            // (*#^) :: Type
            const splitted = line.split('::');
            let word = splitted[0].trim();
            word = word.slice(1, word.length - 1);
            if (parseResults[word]) return;
            parseResults[word] = {
                declaration: line,
                symbolStart: getWordStart(word, line),
                symbolEnd: getWordEnd(word, line),
                doc: docString
            };
        } else if (/^\S+\s*::/.test(line)) {
            // foo :: Type
            const splitted = line.split('::');
            const word = splitted[0].trim();
            if (parseResults[word]) return;
            parseResults[word] = {
                declaration: line,
                symbolStart: getWordStart(word, line),
                symbolEnd: getWordEnd(word, line),
                doc: docString
            };
        }
    });
    if (window.buildMode === 'codeworld') {
        window.codeWorldSymbols = Object.assign({}, parseResults,
            window.codeWorldBuiltinSymbols);
    } else {
        window.codeWorldSymbols = Object.assign({}, parseResults);
    }
}

function renderDeclaration(decl, keyword, keywordData, maxLen, argIndex = -1) {
    if (keywordData.symbolStart > 0) {
        decl.appendChild(document.createTextNode(
            keywordData.declaration.slice(0, keywordData.symbolStart)));
    }

    const wordElem = document.createElement('span');
    wordElem.className = 'hint-word';
    wordElem.appendChild(document.createTextNode(keyword));
    decl.appendChild(wordElem);

    let leftover = "";
    if (keywordData.symbolEnd < keywordData.declaration.length) {
        leftover = keywordData.declaration.slice(keywordData.symbolEnd).replace(
            /\s+/g, ' ');
        if (keywordData.symbolEnd + leftover.length > maxLen && leftover.length >
            3) {
            leftover = `${leftover.slice(0, maxLen - 3 - keywordData.symbolEnd) 
            }...`;
        }
    }
    if (argIndex >= 0) {
        const [head, args, tail] = (/^(\s*::\s*[(]*)([\w,\s]*)([)]*\s*->.*)$/).exec(leftover).slice(1);
        let tokens = args.split(",");
        argIndex = Math.min(argIndex, tokens.length - 1);
        tokens[argIndex] = `<strong>${tokens[argIndex]}</strong>`;
        leftover = `${head}${tokens.join(',')}${tail}`;
    }


    if (leftover.length) {
        const argElem = document.createElement('span');
        argElem.innerHTML = leftover;
        decl.appendChild(argElem);
    }
    return decl;
}

function renderHover(keyword) {
    const topDiv = document.createElement('div');

    if (!window.codeWorldSymbols[keyword]) {
        return;
    }
    topDiv.title = keyword;
    const keywordData = window.codeWorldSymbols[keyword];

    const docDiv = document.createElement('div');

    const annotation = document.createElement('div');
    renderDeclaration(annotation, keyword, keywordData, 9999);
    annotation.className = 'hover-decl';
    docDiv.appendChild(annotation);

    if (keywordData.doc) {
        const description = document.createElement('div');
        description.innerHTML = keywordData.doc;
        description.className = 'hover-doc';
        docDiv.appendChild(description);
    }

    const fadeDiv = document.createElement('div');
    fadeDiv.className = 'fade';

    topDiv.appendChild(docDiv);
    topDiv.appendChild(fadeDiv);
    return topDiv;
}

function onHover(cm, data, node) {
    if (data && data.token && data.token.string) {
        const token_name = data.token.string;
        if (hintBlacklist.indexOf(token_name) === -1) {
            return renderHover(token_name);
        }
    }
}

// Hints and hover tooltips
function registerStandardHints(successFunc) {
    CodeMirror.registerHelper('hint', 'codeworld', cm => {
        const cur = cm.getCursor();
        const token = cm.getTokenAt(cur);

        // If the current token is whitespace, it can be split.
        let term, from;
        if (/^\s+$/.test(token.string)) {
            term = '';
            from = cur;
        } else {
            term = token.string.substr(0, cur.ch - token.start);
            from = CodeMirror.Pos(cur.line, token.start);
        }

        const found = [];
        const hints = Object.keys(window.codeWorldSymbols);
        for (let i = 0; i < hints.length; i++) {
            const hint = hints[i];
            if (hint.startsWith(term)) {
                found.push({
                    text: hint,
                    render: elem => {
                        renderDeclaration(elem, hint,
                            window.codeWorldSymbols[hint], 50);
                    }
                });
            }
        }

        found.sort((a, b) => {
            function startsWithLetter(c) {
                return /^[a-zA-Z].*/.test(c);
            }

            if (startsWithLetter(a.text) && !startsWithLetter(b.text)) {
                return -1;
            } else if (startsWithLetter(b.text) && !startsWithLetter(a.text)) {
                return 1;
            } else {
                return a.text.toLowerCase() < b.text.toLowerCase() ? -1 : 1;
            }
        });

        if (found.length > 0) {
            const data = {
                list: found,
                from: from,
                to: cur
            };

            const deleteOldHintDocs = () => {
                $('.hint-description').remove();
            };

            CodeMirror.on(data, 'close', deleteOldHintDocs);
            CodeMirror.on(data, 'pick', deleteOldHintDocs);

            // Tracking of hint selection
            CodeMirror.on(
                data, 'select',
                (selection, elem) => {
                    const hintsWidgetRect = elem.parentElement.getBoundingClientRect();
                    const doc = document.createElement('div');
                    deleteOldHintDocs();
                    const hover = renderHover(selection.text);
                    if (hover) {
                        doc.className += 'hint-description';
                        doc.style.top = `${hintsWidgetRect.top}px`;
                        doc.style.left = `${hintsWidgetRect.right 
                        }px`;
                        doc.appendChild(hover);
                        document.body.appendChild(doc);
                    }
                }
            );
            return data;
        }
    });

    sendHttp('GET', 'codeworld-base.txt', null, request => {
        let lines = [];
        if (request.status !== 200) {
            console.log('Failed to load autocomplete word list.');
        } else {
            lines = request.responseText.split('\n');
        }

        const startLine = lines.indexOf('module Prelude') + 1;
        let endLine = startLine;
        while (endLine < lines.length) {
            if (lines[endLine].startsWith('module ')) {
                break;
            }
            endLine++;
        }
        lines = lines.slice(startLine, endLine);

        // Special case for "program", since it is morally a built-in name.
        window.codeworldKeywords['program'] = 'builtin';

        window.codeWorldBuiltinSymbols['program'] = {
            declaration: 'program :: Program',
            doc: 'Your program.',
            symbolStart: 0,
            symbolEnd: 7
        };

        let doc = '';
        lines.forEach(line => {
            if (line.startsWith('type Program')) {
                // We must intervene to hide the IO type.
                line = 'data Program';
            } else if (line.startsWith('type Truth')) {
                line = 'data Truth';
            } else if (line.startsWith('True ::')) {
                line = 'True :: Truth';
            } else if (line.startsWith('False ::')) {
                line = 'False :: Truth';
            } else if (line.startsWith('newtype ')) {
                // Hide the distinction between newtype and data.
                line = `data ${line.substr(8)}`;
            } else if (line.startsWith('pattern ')) {
                // Hide the distinction between patterns and constructors.
                line = line.substr(8);
            } else if (line.startsWith('class ')) {
                doc = '';
                return;
            } else if (line.startsWith('instance ')) {
                doc = '';
                return;
            } else if (line.startsWith('infix ')) {
                doc = '';
                return;
            } else if (line.startsWith('infixl ')) {
                doc = '';
                return;
            } else if (line.startsWith('infixr ')) {
                doc = '';
                return;
            }

            // Filter out strictness annotations.
            line = line.replace(/(\s)!([A-Za-z([])/g, '$1$2');

            // Filter out CallStack constraints.
            line = line.replace(/:: HasCallStack =>/g, '::');

            if (line.startsWith('-- |')) {
                doc = `${line.replace(/-- \| /g, '')}\n`;
            } else if (line.startsWith('-- ')) {
                doc += `${line.replace(/-- {3}/g, '')}\n`;
            } else {
                let wordStart = 0;
                if (line.startsWith('type ') || line.startsWith('data ')) {
                    wordStart += 5;

                    // Hide kind annotations.
                    const kindIndex = line.indexOf(' ::');
                    if (kindIndex !== -1) {
                        line = line.substr(0, kindIndex);
                    }
                }

                let wordEnd = line.indexOf(' ', wordStart);
                if (wordEnd === -1) {
                    wordEnd = line.length;
                }
                if (wordStart === wordEnd) {
                    doc = '';
                    return;
                }

                if (line[wordStart] === '(' && line[wordEnd - 1] ===
                    ')') {
                    wordStart++;
                    wordEnd--;
                }

                const word = line.substr(wordStart, wordEnd -
                    wordStart);
                if (hintBlacklist.indexOf(word) < 0) {
                    window.codeWorldBuiltinSymbols[word] = {
                        declaration: line,
                        symbolStart: wordStart,
                        symbolEnd: wordEnd
                    };
                    if (doc) {
                        window.codeWorldBuiltinSymbols[word].doc = doc;
                    }
                }

                if (hintBlacklist.indexOf(word) >= 0) {
                    window.codeworldKeywords[word] = 'deprecated';
                } else if (/^[A-Z:]/.test(word)) {
                    window.codeworldKeywords[word] = 'builtin-2';
                } else {
                    window.codeworldKeywords[word] = 'builtin';
                }
                doc = '';
            }
        });

        successFunc();
    });
}

function signin() {
    if (window.auth2) {
        window.auth2.signIn({
            prompt: 'login'
        });
    }
}

function signout() {
    if (window.auth2) window.auth2.signOut();
}

function signedIn() {
    return Boolean(window.auth2 && window.auth2.isSignedIn.get());
}

const Auth = (() => {
    const mine = {};

    function initLocalAuth() {
        Promise.resolve($.getScript('/js/codeworld_local_auth.js'))
            .then(() => onAuthInitialized(LocalAuth.init()))
            .catch(e => console.log('initLocalAuth failed'));
    }

    function initGoogleAuth() {
        Promise.resolve($.getScript(
            'https://apis.google.com/js/platform.js'))
            .then(() => gapi.load('auth2', () =>
                withClientId(clientId => {
                    function sendHttpAuth(method, url, body,
                        callback) {
                        if (body !== null && signedIn()) {
                            const idToken = window.auth2.currentUser
                                .get().getAuthResponse().id_token;
                            body.append('id_token', idToken);
                        }

                        const request = new XMLHttpRequest();

                        if (callback) {
                            request.onreadystatechange = () => {
                                if (request.readyState === 4) {
                                    callback(request);
                                }
                            };
                        }

                        request.open(method, url, true);
                        request.send(body);

                        return request;
                    }

                    const auth2 = Object.assign({
                        sendHttpAuth: sendHttpAuth
                    }, gapi.auth2.init({
                        client_id: clientId,
                        scope: 'profile',
                        fetch_basic_profile: false
                    }));

                    onAuthInitialized(auth2);
                })
            ))
            .catch(e => console.log('initGoogleAuth failed'));
    }

    function onAuthInitialized(auth) {
        window.auth2 = auth;
        window.auth2.currentUser.listen(signinCallback);

        if (window.auth2.isSignedIn.get()) {
            window.auth2.signIn();
        }

        discoverProjects('', 0);
        updateUI();
    }

    function onAuthDisabled() {
        window.auth2 = null;
        document.getElementById('signin').style.display = 'none';
        discoverProjects('', 0);
        updateUI();
    }

    mine.init = () =>
        sendHttp('GET', 'authMethod', null, resp => {
            if (resp.status === 200) {
                const obj = JSON.parse(resp.responseText);
                switch (obj.authMethod) {
                case 'Local':
                    initLocalAuth();
                    break;
                case 'Google':
                    initGoogleAuth();
                    break;
                default:
                    onAuthDisabled();
                    break;
                }
            } else {
                onAuthDisabled();
            }
        });

    return mine;
})();

function withClientId(f) {
    if (window.clientId) return f(window.clientId);

    sendHttp('GET', 'clientId.txt', null, request => {
        if (request.status !== 200 || request.responseText === '') {
            sweetAlert('Oops!',
                'Missing API client key.  You will not be able to sign in.',
                'warning');
            return null;
        }

        window.clientId = request.responseText.trim();
        return f(window.clientId);
    });
}

function discoverProjects_(path, buildMode, index) {
    if (!signedIn()) {
        window.allProjectNames = window.openProjectName ? [
            [window.openProjectName]
        ] : [
            []
        ];
        window.allFolderNames = [
            []
        ];
        window.nestedDirs = [''];
        cancelMove();
        updateUI();
        return;
    }

    const data = new FormData();
    data.append('mode', buildMode);
    data.append('path', path);

    sendHttp('POST', 'listFolder', data, request => {
        if (request.status === 200) {
            window.loadingDir = false;
            const allContents = JSON.parse(request.responseText);
            window.allProjectNames[index] = allContents['files'];
            window.allFolderNames[index] = allContents['dirs'];
        }
        updateNavBar();
    });

    window.loadingDir = true;
}

function moveHere_(path, buildMode, successFunc) {
    if (!signedIn()) {
        sweetAlert('Oops!', 'You must sign in before moving.', 'error');
        cancelMove();
        return;
    }

    if (!window.move) {
        sweetAlert('Oops!', 'You must first select something to move.',
            'error');
        cancelMove();
        return;
    }

    const data = new FormData();
    data.append('mode', buildMode);
    data.append('moveTo', path);
    data.append('moveFrom', window.move.path);
    if (window.move.file) {
        data.append('isFile', 'true');
        data.append('name', window.move.file);
    } else {
        if (path.startsWith(window.move.path)) {
            sweetAlert('Oops!',
                'You cannot move a path to a location inside itself.',
                'error');
            cancelMove();
            return;
        }
        data.append('isFile', 'false');
    }

    sendHttp('POST', 'moveProject', data, request => {
        if (request.status !== 200) {
            sweetAlert('Oops',
                'Could not move your project! Please try again.',
                'error');
            cancelMove();
            return;
        }
        successFunc();
    });
}

function cancelMove() {
    window.move = null;
    updateUI();
}

function warnIfUnsaved(action, showAnother) {
    if (isEditorClean()) {
        action();
    } else {
        const msg = 'There are unsaved changes to your project. ' +
            'Continue and throw away your changes?';
        sweetAlert({
            title: Alert.title('Warning'),
            text: msg,
            type: 'warning',
            showCancelButton: true,
            confirmButtonColor: '#DD6B55',
            confirmButtonText: 'Yes, discard my changes!',
            closeOnConfirm: !showAnother
        }).then(result => {
            if (result.value) action();
        });
    }
}

function saveProjectAs() {
    if (!signedIn()) {
        sweetAlert('Oops!', 'You must sign in to save files.', 'error');
        updateUI();
        return;
    }

    let text;
    if (window.nestedDirs.length > 1) {
        text = `Enter a name for your project in folder <b>${ 
            $('<div>').text(window.nestedDirs.slice(1).join('/')).html().replace(/ /g,
                '&nbsp;') 
        }:`;
    } else {
        text = 'Enter a name for your project:';
    }

    let defaultName;
    if (window.openProjectName) {
        defaultName = window.openProjectName;
    } else {
        defaultName = '';
    }

    sweetAlert({
        title: Alert.title('Save As', 'mdi-cloud-upload'),
        html: text,
        input: 'text',
        inputValue: defaultName,
        confirmButtonText: 'Save',
        showCancelButton: true,
        closeOnConfirm: false
    }).then(result => {
        if (result.value) {
            saveProjectBase(window.nestedDirs.slice(1).join('/'), result.value);
        }
    });
}

function saveProject() {
    if (!signedIn()) {
        sweetAlert('Oops!', 'You must sign in to save files.', 'error');
        updateUI();
        return;
    }

    if (window.openProjectName) {
        saveProjectBase(window.nestedDirs.slice(1).join('/'), window.openProjectName);
    } else {
        saveProjectAs();
    }
}

function saveProjectBase_(path, projectName, mode, successFunc) {
    if (projectName === null || projectName === '') return;

    if (!signedIn()) {
        sweetAlert('Oops!', 'You must sign in to save files.', 'error');
        updateUI();
        return;
    }

    function go() {
        sweetAlert({
            title: Alert.title(`Saving ${projectName} ...`),
            text: 'Saving your project.  Please wait.',
            showConfirmButton: false,
            showCancelButton: false,
            showCloseButton: false,
            allowOutsideClick: false,
            allowEscapeKey: false,
            allowEnterKey: false
        });

        const project = getCurrentProject();
        project['name'] = projectName;

        const data = new FormData();
        data.append('project', JSON.stringify(project));
        data.append('mode', mode);
        data.append('path', path);

        sendHttp('POST', 'saveProject', data, request => {
            sweetAlert.close();
            if (request.status !== 200) {
                sweetAlert('Oops!',
                    'Could not save your project!!!  Please try again.',
                    'error');
                return;
            }

            successFunc();
            cancelMove();
            updateUI();

            if (window.allProjectNames[window.allProjectNames.length - 1].indexOf(projectName) === -1) {
                discoverProjects(path, window.allProjectNames.length - 1);
            }
        });
    }

    if (window.allProjectNames[window.allProjectNames.length - 1].indexOf(projectName) === -1 || projectName === window.openProjectName) {
        go();
    } else {
        const msg = `${'Are you sure you want to save over another project?\n\n' +
            'The previous contents of '}${projectName 
        } will be permanently destroyed!`;
        sweetAlert({
            title: Alert.title('Warning'),
            text: msg,
            type: 'warning',
            showCancelButton: true,
            confirmButtonColor: '#DD6B55',
            confirmButtonText: 'Yes, overwrite it!'
        }).then(result => {
            if (result.value) go();
        });
    }
}

function deleteProject_(path, buildMode, successFunc) {
    if (!window.openProjectName) return;

    if (!signedIn()) {
        sweetAlert('Oops', 'You must sign in to delete a project.', 'error');
        updateUI();
        return;
    }

    const msg =
        'Deleting a project will throw away all work, and cannot be undone. ' +
        'Are you sure?';

    sweetAlert({
        title: Alert.title('Warning'),
        text: msg,
        type: 'warning',
        showCancelButton: true,
        confirmButtonColor: '#DD6B55',
        confirmButtonText: 'Yes, delete it!'
    }).then(result => {
        if (!result.value) {
            return;
        }

        const data = new FormData();
        data.append('name', window.openProjectName);
        data.append('mode', buildMode);
        data.append('path', path);

        sendHttp('POST', 'deleteProject', data, request => {
            if (request.status === 200) {
                successFunc();
                discoverProjects(path, window.allProjectNames.length -
                    1);
            }
        });
    });
}

function deleteFolder_(path, buildMode, successFunc) {
    if (path === '' || window.openProjectName !== null) {
        return;
    }
    if (!signedIn()) {
        sweetAlert('Oops', 'You must sign in to delete a folder.', 'error');
        updateUI();
        return;
    }

    const msg =
        'Deleting a folder will throw away all of its content, cannot be undone. ' +
        'Are you sure?';

    sweetAlert({
        title: Alert.title('Warning'),
        text: msg,
        type: 'warning',
        showCancelButton: true,
        confirmButtonColor: '#DD6B55',
        confirmButtonText: 'Yes, delete it!'
    }).then(result => {
        if (!result.value) {
            return;
        }

        const data = new FormData();
        data.append('mode', buildMode);
        data.append('path', path);

        sendHttp('POST', 'deleteFolder', data, request => {
            if (request.status === 200) {
                successFunc();
                window.nestedDirs.pop();
                window.allProjectNames.pop();
                window.allFolderNames.pop();
                discoverProjects(window.nestedDirs.slice(1).join('/'), window.allProjectNames.length - 1);
            }
        });
    });
}

function createFolder(path, buildMode, successFunc) {
    warnIfUnsaved(() => {
        if (!signedIn()) {
            sweetAlert('Oops!', 'You must sign in to create a folder.',
                'error');
            updateUI();
            return;
        }

        sweetAlert({
            title: Alert.title('Create Folder',
                'mdi-folder-plus'),
            text: 'Enter a name for your folder:',
            input: 'text',
            inputValue: '',
            confirmButtonText: 'Create',
            showCancelButton: true,
            closeOnConfirm: false
        }).then(result => {
            if (!result.value) {
                return;
            }

            sweetAlert.close();
            const data = new FormData();
            data.append('mode', buildMode);
            if (path === '') {
                data.append('path', result.value);
            } else {
                data.append('path', `${path}/${result.value}`);
            }

            sendHttp('POST', 'createFolder', data, request => {
                if (request.status !== 200) {
                    sweetAlert('Oops',
                        'Could not create your directory! Please try again.',
                        'error');
                    return;
                }

                window.allFolderNames[window.allFolderNames.length - 1].push(result.value);
                window.nestedDirs.push(result.value);
                window.allFolderNames.push([]);
                window.allProjectNames.push([]);
                successFunc();
                updateNavBar();
            });
        });
    }, true);
}

function loadProject_(index, name, buildMode, successFunc) {

    warnIfUnsaved(() => {
        if (!signedIn()) {
            sweetAlert('Oops!', 'You must sign in to open projects.',
                'error');
            updateUI();
            return;
        }

        const data = new FormData();
        data.append('name', name);
        data.append('mode', buildMode);
        data.append('path', window.nestedDirs.slice(1, index + 1).join('/'));

        sendHttp('POST', 'loadProject', data, request => {
            if (request.status === 200) {
                const project = JSON.parse(request.responseText);

                successFunc(project);
                window.nestedDirs = window.nestedDirs.slice(0, index + 1);
                window.allProjectNames = window.allProjectNames.slice(0, index + 1);
                window.allFolderNames = window.allFolderNames.slice(0, index + 1);
                cancelMove();
                updateUI();
            }
        });
    }, false);
}

function share() {
    let offerSource = true;

    function go() {
        let url;
        let msg;
        let showConfirm;
        let confirmText;

        if (!window.deployHash) {
            url = window.location.href;
            msg = 'Copy this link to share your program and code with others!';
            showConfirm = false;
        } else if (offerSource) {
            url = window.location.href;
            msg = 'Copy this link to share your program and code with others!';
            showConfirm = true;
            confirmText = 'Share Without Code';
        } else {
            const a = document.createElement('a');
            a.href = window.location.href;
            a.hash = '';
            a.pathname = '/run.html';
            a.search = `?mode=${window.buildMode}&dhash=${window.deployHash}`;

            url = a.href;
            msg =
                'Copy this link to share your program (but not code) with others!';
            showConfirm = true;
            confirmText = 'Share With Code';
        }

        sweetAlert({
            title: Alert.title('Share', 'mdi-share'),
            html: msg,
            input: 'text',
            inputValue: url,
            showConfirmButton: showConfirm,
            confirmButtonText: confirmText,
            closeOnConfirm: false,
            showCancelButton: true,
            cancelButtonText: 'Done',
            animation: 'slide-from-bottom'
        }).then(result => {
            if (result.value) {
                offerSource = !offerSource;
                go();
            }
        });
    }

    if (window.runningGeneration) {
        if (!window.codeworldEditor.getDoc().isClean(window.runningGeneration)) {
            sweetAlert({
                type: 'warning',
                text: 'You have changed your code since running the program. ' +
                    ' Rebuild so that you can share your latest code?',
                confirmButtonText: 'Yes, Rebuild',
                cancelButtonText: 'No, Share Old Program',
                showConfirmButton: true,
                showCancelButton: true
            }).then(result => {
                if (result.value) {
                    compile();
                } else {
                    go();
                }
            });
            return;
        }
    }

    go();
}

function inspect() {
    document.getElementById('runner').contentWindow.toggleDebugMode();
    cancelMove();
    updateUI();
}

function shareFolder_(mode) {
    if (!signedIn()) {
        sweetAlert('Oops!', 'You must sign in to share your folder.', 'error');
        updateUI();
        return;
    }
    if (window.nestedDirs.length === 1 || (window.openProjectName !== null && window.openProjectName !== '')) {
        sweetAlert('Oops!', 'YOu must select a folder to share!', 'error');
        updateUI();
        return;
    }

    const data = new FormData();
    data.append('mode', mode);
    data.append('path', window.nestedDirs.slice(1).join('/'));

    sendHttp('POST', 'shareFolder', data, request => {
        if (request.status !== 200) {
            sweetAlert('Oops!',
                'Could not share your folder! Please try again.',
                'error');
            return;
        }

        const shareHash = request.responseText;
        const a = document.createElement('a');
        a.href = window.location.href;
        a.hash = `#${shareHash}`;
        const url = a.href;
        sweetAlert({
            title: Alert.title('Share Folder', 'mdi-folder-outline'),
            html: 'Copy this link to share your folder with others!',
            input: 'text',
            inputValue: url,
            showConfirmButton: false,
            showCancelButton: true,
            cancelButtonText: 'Done',
            animation: 'slide-from-bottom'
        });
    });
}

function preFormatMessage(msg) {
    while (msg.match(/(\r\n|[^\x08]|)\x08/)) {
        msg = msg.replace(/(\r\n|[^\x08])\x08/g, '');
    }

    msg = msg
        .replace(/&/g, '&amp;')
        .replace(/</g, '&lt;')
        .replace(/>/g, '&gt;')
        .replace(/program\.hs:(\d+):((\d+)(-\d+)?)/g,
            '<a href="#" onclick="goto($1, $3);">Line $1, Column $2</a>')
        .replace(/program\.hs:\((\d+),(\d+)\)-\((\d+),(\d+)\)/g,
            '<a href="#" onclick="goto($1, $2);">Line $1-$3, Column $2-$4</a>');
    return msg;
}

function printMessage(type, message) {
    const outputDiv = document.getElementById('message');

    let box = outputDiv.lastChild;
    let messageContent;
    if (box && type === 'log' && box.classList.contains('log')) {
        box.rawMessage += message;
        messageContent = box.lastChild;
    } else {
        box = document.createElement('div');
        box.classList.add('message-box');
        box.classList.add(type);

        const messageGutter = document.createElement('div');
        messageGutter.classList.add('message-gutter');

        messageContent = document.createElement('div');
        messageContent.classList.add('message-content');

        box.appendChild(messageGutter);
        box.appendChild(messageContent);

        box.rawMessage = message;
    }

    const formatted = preFormatMessage(box.rawMessage);
    const lines = formatted.trim().split('\n');

    messageContent.innerHTML = '';

    if (lines.length < 2) {
        const singleLineMsg = document.createElement('div');
        singleLineMsg.innerHTML = formatted;
        messageContent.appendChild(singleLineMsg);
    } else {
        const summary = document.createElement('summary');
        summary.innerHTML = lines[0];

        const details = document.createElement('details');
        details.setAttribute('open', '');
        details.innerHTML = lines.slice(1).join('\n');

        details.insertBefore(summary, details.firstChild);
        messageContent.appendChild(details);
    }

    outputDiv.appendChild(box);
    outputDiv.scrollTop = outputDiv.scrollHeight;
}

function clearMessages() {
    const outputDiv = document.getElementById('message');
    outputDiv.innerHTML = '';
    outputDiv.classList.remove('error');
}

function markFailed() {
    const outputDiv = document.getElementById('message');
    outputDiv.classList.add('error');
}
