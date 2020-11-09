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

import { handleIncomingMessage as passMessageToCodeExplorer } from './codeExplorer.js';
import {
  createFolder,
  definePanelExtension,
  deleteFolder_,
  deleteProject_,
  getNearestDirectory,
  initDirectoryTree,
  initializeLayoutContainer,
  LAYOUT_CONTAINER_CLASSNAME,
  loadProject,
  loadSample,
  loadTreeNodes,
  markFailed,
  onHover,
  parseCompileErrors,
  parseSymbolsFromCurrentCode,
  printMessage,
  registerStandardHints,
  renderDeclaration,
  run,
  saveProjectBase,
  saveProjectAsBase,
  share,
  shareFolder_,
  toggleObsoleteCodeAlert,
  updateDocumentTitle,
  updateProjectChangeMark,
  updateTreeOnNewProjectCreation,
  warnIfUnsaved,
} from './codeworld_shared.js';

import * as Alert from './utils/alert.js';
import * as Auth from './utils/auth.js';
import * as DirTree from './utils/directoryTree.js';
import { sendHttp } from './utils/network.js';
import { onObjectPropertyChange } from './utils/object.js';

init();

function attachEventListeners() {
  $('#signout').on('click', () => {
    Auth.signOut(isEditorClean, clearWorkspace);
  });
  $('#signin').on('click', Auth.signIn);

  $('#navButton').on('click', () => {
    $(LAYOUT_CONTAINER_CLASSNAME).layout().toggle('west');
  });

  $('#newButton').on('click', newProject);
  $('#newFolderButton').on('click', newFolder);
  $('#deleteButton').on('click', deleteProject);
  $('#saveButton').on('click', saveProject);
  $('#saveAsButton').on('click', saveProjectAs);
  $('#downloadButton').on('click', downloadProject);
  $('#docButton').on('click', help);
  $('#toggleThemeButton').on('click', toggleTheme);
  $('#shareFolderButton').on('click', shareFolder);

  $('#startRecButton').on('click', captureStart);
  $('#stopRecButton').on('click', stopRecording);
  $('#shareButton').on('click', share);
  $('#inspectButton').on('click', inspect);

  $('#runButton').on('click', compile);
  $('#stopButton').on('click', stopRun);

  $('#runner').on('load', toggleObsoleteCodeAlert);
}

function attachCustomEventListeners() {
  $('#directoryTree').on(DirTree.events.SELECTION_CLEARED, () => {
    $('#deleteButton').hide();
    $('#saveButton').hide();
    $('#shareFolderButton').hide();
    $('#downloadButton').hide();
  });

  const $inspectButton = $('#inspectButton');

  onObjectPropertyChange(window, 'debugAvailable', () => {
    if (window.debugAvailable) {
      $inspectButton.show();
    } else {
      $inspectButton.hide();
    }
  });

  onObjectPropertyChange(window, 'debugActive', () => {
    const activeClass = 'inspect-button__active';

    if (window.debugActive) {
      $inspectButton.addClass(activeClass);
    } else {
      $inspectButton.removeClass(activeClass);
    }
  });

  onObjectPropertyChange(window, 'savedGeneration', () => {
    updateDocumentTitle();
    updateProjectChangeMark();
  });
}

function initializeLayout() {
  initializeLayoutContainer({
    default: {},
    west: {
      initHidden: true,
      minSize: 50,
      enableCursorHotkey: false,
    },
    east: {
      initHidden: true,
      resizable: true,
      size: 510,
      enableCursorHotkey: false,
      onresize: () => {
        const $runner = $('#runner');
        const runnerWidth = $runner.width();

        $runner.css({
          height: `${runnerWidth}px`,
        });
      },
    },
  });

  let wasDraggingEast = false;
  new MutationObserver(() => {
    const isDraggingEast =
      document.getElementsByClassName('ui-layout-resizer-east-dragging')
        .length > 0;
    const runner = $('#runner');
    if (!wasDraggingEast && isDraggingEast) {
      runner.css({
        'pointer-events': 'none',
      });
    } else if (wasDraggingEast && !isDraggingEast) {
      runner.css({
        'pointer-events': 'auto',
      });
      runner.focus();
    }
    wasDraggingEast = isDraggingEast;
  }).observe(document.querySelector('.ui-layout-container'), {
    childList: true,
  });
}

/*
 * Initializes the programming environment.  This is called after the
 * entire document body and other JavaScript has loaded.
 */
async function init() {
  await Alert.init();

  await Auth.init(() => {
    const autohelpEnabled = location.hash.length <= 2;
    let isFirstSignin = true;

    window.auth2.currentUser.listen(() => {
      if (isFirstSignin && !Auth.signedIn() && autohelpEnabled) {
        help();
      }

      isFirstSignin = false;
    });

    window.auth2.isSignedIn.listen(() => {
      const layoutHandler = $(LAYOUT_CONTAINER_CLASSNAME).layout();

      if (Auth.signedIn()) {
        loadTreeNodes(DirTree.getRootNode());

        $('#signin').hide();
        $('#signout, #navButton').show();
        layoutHandler.show('west');
      } else {
        $('#signin').show();
        $(
          '#signout, #saveButton, #navButton, #deleteButton, #shareFolderButton'
        ).hide();
        layoutHandler.hide('west');
      }
    });
  });

  attachEventListeners();
  attachCustomEventListeners();

  initializeLayout();

  // Keep the base bundle preloaded by retrying regularly.
  function preloadBaseBundle() {
    const request = new XMLHttpRequest();
    request.open('GET', '/runBaseJS', true);
    request.setRequestHeader('Cache-control', 'max-stale');
    request.send();
  }
  preloadBaseBundle();
  window.setInterval(preloadBaseBundle, 1000 * 60 * 60);

  function loadProjectHandler(name, path) {
    function successCallback(project) {
      setCode(project.source, project.history);
    }
    loadProject(name, path, window.buildMode, successCallback);
  }
  initDirectoryTree(isEditorClean, loadProjectHandler, () => setCode(''));

  window.savedGeneration = null;
  window.runningGeneration = null;
  window.debugAvailable = false;
  window.debugActive = false;

  if (window.location.pathname === '/haskell') {
    window.buildMode = 'haskell';
    window.projectEnv = 'haskell';
  } else {
    window.buildMode = 'codeworld';
    window.projectEnv = 'codeworld';
  }
  document.documentElement.classList.add(window.buildMode);

  window.cancelCompile = () => {};
  window.clipboard = '';

  definePanelExtension();

  initCodeworld();
  registerStandardHints(() => {
    setMode(true);
    parseSymbolsFromCurrentCode();
  });

  let hash = location.hash.slice(1);
  if (hash.length > 0) {
    if (hash.slice(-2) === '==') {
      hash = hash.slice(0, -2);
    }
    if (hash[0] === 'F') {
      sweetAlert({
        title: Alert.title('Save As', 'mdi-cloud-upload'),
        html: 'Enter a name for the shared folder:',
        input: 'text',
        confirmButtonText: 'Save',
        showCancelButton: false,
      }).then((result) => {
        if (!result || !result.value) {
          return;
        }

        const data = new FormData();
        data.append('mode', window.buildMode);
        data.append('shash', hash);
        data.append('name', result.value);

        sendHttp('POST', 'shareContent', data, (request) => {
          window.location.hash = '';
          if (request.status === 200) {
            sweetAlert(
              'Success!',
              'The shared folder has been copied to your root directory.',
              'success'
            );
          } else {
            sweetAlert(
              'Oops!',
              'Could not load the shared directory. Please try again.',
              'error'
            );
          }
          loadTreeNodes(DirTree.getRootNode());
        });
      });
    }
  }

  if (hash.length > 0) {
    if (hash.slice(-2) === '==') {
      hash = hash.slice(0, -2);
    }
    if (hash[0] === 'P') {
      sendHttp(
        'GET',
        `loadSource?hash=${hash}&mode=${window.buildMode}`,
        null,
        (request) => {
          if (request.status === 200) {
            setCode(request.responseText, null, true);
          }
        }
      );
    } else if (hash[0] !== 'F') {
      setCode('');
    }
  } else {
    setCode('');
  }
}

function initCodeworld() {
  const editor = document.getElementById('editor');
  const darkMode = window.localStorage.getItem('darkMode') === 'true';

  window.codeworldKeywords = {};

  window.codeworldEditor = CodeMirror.fromTextArea(editor, {
    mode: {
      name: 'codeworld',
      overrideKeywords: window.codeworldKeywords,
    },
    theme: darkMode ? 'ambiance' : 'default',

    undoDepth: 50,
    lineNumbers: true,
    autofocus: true,
    matchBrackets: true,
    styleActiveLine: !WURFL || !WURFL.is_mobile,
    showTrailingSpace: true,
    indentWithTabs: false,
    indentUnit: 2,
    autoClearEmptyLines: true,
    highlightSelectionMatches: {
      showToken: /\w/,
      annotateScrollbar: true,
    },
    rulers: [
      {
        column: 80,
        color: '#bbb',
        lineStyle: 'dashed',
      },
    ],
    extraKeys: {
      'Ctrl-Space': 'autocomplete',
      'Shift-Space': 'autocomplete',
      Tab: 'indentMore',
      'Shift-Tab': 'indentLess',
      'Ctrl-Enter': compile,
      'Ctrl-Up': changeFontSize(1),
      'Ctrl-Down': changeFontSize(-1),
      Backspace: backspace,
    },
    textHover: window.buildMode === 'codeworld' ? onHover : null,
    gutters: ['CodeMirror-lint-markers'],
    lint: {
      getAnnotations: (text, callback) => {
        if (text.trim() === '') {
          callback([]);
          return;
        }

        let request = null;

        function cancelLintRequest() {
          if (window.codeworldEditor) {
            window.codeworldEditor.off('change', cancelLintRequest);
          }
          if (request) request.abort();
        }

        const data = new FormData();
        data.append('source', text);
        data.append('mode', window.buildMode);
        request = sendHttp('POST', 'errorCheck', data, (request) => {
          if (window.codeworldEditor) {
            window.codeworldEditor.off('change', cancelLintRequest);
          }

          if (request.status === 400 || request.status === 200) {
            const messages = parseCompileErrors(request.responseText);
            callback(messages);
          } else if (request.status === 0) {
            // Request was cancelled because of a later change.  Do nothing.
          } else {
            console.log(
              'Not expected behavior: don\'t know how to ' +
                'handle request with status ',
              request.status,
              request
            );
          }
        });
        if (window.codeworldEditor) {
          window.codeworldEditor.on('change', cancelLintRequest);
        }
      },
      async: true,
    },
    buttons: [
      {
        class: 'cw-toolbar-button mdi mdi-file-outline',
        label: '',
        title: 'New',
        callback: (cm) => newProject(),
      },
      {
        class: 'cw-toolbar-button mdi mdi-content-save',
        label: '',
        title: 'Save',
        callback: (cm) => saveProject(),
      },
      {
        class: 'cw-toolbar-button mdi mdi-file-find',
        label: '',
        title: 'Search',
        callback: (cm) => cm.execCommand('find'),
      },
      {
        class: 'cw-toolbar-button mdi mdi-file-replace',
        label: '',
        title: 'Replace',
        callback: (cm) => cm.execCommand('replace'),
      },
      {
        class: 'cw-toolbar-button mdi mdi-undo',
        label: '',
        title: 'Undo',
        callback: (cm) => cm.undo(),
      },
      {
        class: 'cw-toolbar-button mdi mdi-redo',
        label: '',
        title: 'Redo',
        callback: (cm) => cm.redo(),
      },
      {
        class: 'cw-toolbar-button mdi mdi-format-indent-increase',
        label: '',
        title: 'Indent',
        callback: (cm) => cm.execCommand('indentMore'),
      },
      {
        class: 'cw-toolbar-button mdi mdi-format-indent-decrease',
        label: '',
        title: 'Outdent',
        callback: (cm) => cm.execCommand('indentLess'),
      },
      {
        class: 'cw-toolbar-button mdi mdi-magnify-plus',
        label: '',
        title: 'Zoom in',
        callback: (cm) => changeFontSize(1)(),
      },
      {
        class: 'cw-toolbar-button mdi mdi-magnify-minus',
        label: '',
        title: 'Zoom out',
        callback: (cm) => changeFontSize(-1)(),
      },
      {
        class: 'cw-toolbar-button mdi mdi-content-cut',
        label: '',
        title: 'Cut',
        callback: (cm) => {
          if (cm.getSelection()) {
            window.clipboard = cm.getSelection();
            document.execCommand('copy');
            cm.replaceSelection('');
          }
        },
      },
      {
        class: 'cw-toolbar-button mdi mdi-content-copy',
        label: '',
        title: 'Copy',
        callback: (cm) => {
          if (cm.getSelection()) {
            window.clipboard = cm.getSelection();
            document.execCommand('copy');
          }
        },
      },
      {
        class: 'cw-toolbar-button mdi mdi-content-paste',
        label: '',
        title: 'Paste',
        callback: (cm) => cm.replaceSelection(window.clipboard),
      },
      {
        class: 'cw-toolbar-button mdi mdi-stop',
        label: '',
        title: 'Stop',
        callback: (cm) => stopRun(),
      },
      {
        class: 'cw-toolbar-button mdi mdi-play',
        label: '',
        title: 'Run',
        callback: (cm) => compile(),
      },
      {
        class: 'cw-toolbar-button mdi mdi-auto-fix',
        label: '',
        title: 'Autocomplete',
        callback: (cm) => cm.execCommand('autocomplete'),
      },
    ],
  });
  CodeMirror.commands.indentMore = (cm) => changeIndent('add');
  CodeMirror.commands.indentLess = (cm) => changeIndent('subtract');
  window.codeworldEditor.refresh();
  window.codeworldEditor.on('cursorActivity', updateArgHelp);
  window.codeworldEditor.on('refresh', updateArgHelp);

  if (window.localStorage.getItem('darkMode') === 'true') toggleTheme();

  CodeMirror.commands.save = (cm) => {
    saveProject();
  };
  document.onkeydown = (e) => {
    if (e.ctrlKey && e.keyCode === 83) {
      // Ctrl+S
      saveProject();
      return false;
    }
    if (e.ctrlKey && e.keyCode === 73) {
      // Ctrl+I
      formatSource();
      return false;
    }
  };

  window.reparseTimeoutId = null;
  window.codeworldEditor.on('changes', ({ doc }, changes) => {
    if (window.reparseTimeoutId) {
      clearTimeout(window.reparseTimeoutId);
    }
    window.reparseTimeoutId = setTimeout(parseSymbolsFromCurrentCode, 1500);

    const shareButton = document.getElementById('shareButton');

    if (doc.getValue()) {
      shareButton.classList.remove('cw-button--disabled');
    } else {
      shareButton.classList.add('cw-button--disabled');
    }

    updateDocumentTitle(isEditorClean);
    updateProjectChangeMark(isEditorClean);

    toggleObsoleteCodeAlert();
  });

  if (window.buildMode === 'codeworld') {
    window.codeworldEditor.on('cursorActivity', () => {
      if (window.codeworldEditor.getDoc().somethingSelected()) {
        window.codeworldEditor.setOption('electricChars', false);
        window.codeworldEditor.setOption('electricIndentLine', -1);
        return;
      }

      const active = window.codeworldEditor.getOption('electricChars');
      const oldLine =
        window.codeworldEditor.getOption('electricIndentLine') || 0;
      const line = window.codeworldEditor.getDoc().getCursor().line;

      if (active || line !== oldLine) {
        const { prev, smart } = getIndentsAt(line, 'smart');
        window.codeworldEditor.setOption('electricChars', prev === smart);
        window.codeworldEditor.setOption('electricIndentLine', line);
      }
    });
  }

  window.onbeforeunload = (event) => {
    if (!isEditorClean()) {
      const msg =
        'There are unsaved changes to your project. ' +
        'If you continue, they will be lost!';
      if (event) event.returnValue = msg;
      return msg;
    }
  };

  window.onresize = () => {
    if (window.resizeTimeoutId) clearTimeout(window.resizeTimeoutId);
    window.resizeTimeoutId = setTimeout(() => {
      window.codeworldEditor.setSize();
    }, 1000);
  };
}

function setCode(code, history, autostart) {
  if (!window.codeworldEditor) {
    return;
  }

  const doc = window.codeworldEditor.getDoc();
  doc.setValue(code);

  const nextDocGeneration = doc.changeGeneration(true);
  window.savedGeneration = nextDocGeneration;

  if (history) {
    doc.setHistory(history);
  } else {
    doc.clearHistory();
  }

  window.codeworldEditor.focus();
  parseSymbolsFromCurrentCode();
  if (autostart) {
    compile();
  } else {
    stopRun();
  }
}

function isEditorClean() {
  const doc = window.codeworldEditor.getDoc();

  return window.savedGeneration
    ? doc.isClean(window.savedGeneration)
    : Boolean(!doc.getValue());
}

function backspace() {
  if (
    window.buildMode === 'codeworld' &&
    !window.codeworldEditor.somethingSelected()
  ) {
    const cursor = window.codeworldEditor.getCursor();
    const line = window.codeworldEditor.getDoc().getLine(cursor.line);
    const prevIndent = /^[\s]*/.exec(line)[0].length;
    if (cursor.ch > 0 && cursor.ch === prevIndent) {
      // We ask the question: if this line were one space less
      // indented, would this be the next indent point?  If so,
      // then backspace should delete the indent, rather than a
      // single character.
      const startToken = window.codeworldEditor.getTokenAt({
        line: cursor.line,
        ch: 0,
      });
      const smartIndent = getSmartIndent(
        startToken.state,
        line.slice(prevIndent),
        cursor.ch - 1,
        'add'
      );
      if (cursor.ch === smartIndent) {
        window.codeworldEditor.execCommand('indentLess');
        return;
      }
    }
  }
  window.codeworldEditor.execCommand('delCharBefore');
}

function changeIndent(how) {
  if (window.buildMode === 'codeworld') {
    const range = window.codeworldEditor.listSelections()[0];
    const lineNum = Math.min(range.anchor.line, range.head.line);
    const { prev, smart } = getIndentsAt(lineNum, how);

    if (smart >= 0) {
      window.codeworldEditor.indentSelection(smart - prev);
      return;
    }
  }
  window.codeworldEditor.indentSelection(how);
}

function getIndentsAt(lineNum, how) {
  const lineStart = window.codeworldEditor.getTokenAt({
    line: lineNum,
    ch: 0,
  });
  if (!lineStart) return -1;

  const line = window.codeworldEditor.getDoc().getLine(lineNum);
  const prevIndent = /^[\s]*/.exec(line)[0].length;
  const textAfter = line.slice(prevIndent);
  return {
    prev: prevIndent,
    smart: getSmartIndent(lineStart.state, textAfter, prevIndent, how),
  };
}

function getSmartIndent(state, textAfter, prevIndent, how) {
  let dir;
  switch (how) {
  case 'add':
    dir = {
      start: 0,
      end: state.contexts.length + 1,
      inc: 1,
      predicate: (n) => n > prevIndent,
    };
    break;

  case 'subtract':
    dir = {
      start: state.contexts.length,
      end: -1,
      inc: -1,
      predicate: (n) => n < prevIndent,
    };
    break;

  case 'smart':
    dir = {
      start: state.contexts.length,
      end: state.contexts.length + 1,
      inc: 1,
      predicate: (n) => n >= 0,
    };
    break;

  default:
    return -1;
  }

  const mode = window.codeworldEditor.getDoc().getMode();

  for (let i = dir.start; i !== dir.end; i += dir.inc) {
    const stateCopy = mode.copyState(state);
    stateCopy.contexts = stateCopy.contexts.slice(0, i);
    const detected = mode.indent(stateCopy, textAfter);
    if (dir.predicate(detected)) return detected;
  }

  return -1;
}

function updateArgHelp() {
  if (window.buildMode !== 'codeworld') {
    return;
  }

  const prevDiv = document.getElementById('function-details');
  if (prevDiv) prevDiv.remove();

  const cursor = window.codeworldEditor.getCursor();
  const currentToken = window.codeworldEditor.getTokenAt(cursor);
  const functions = currentToken.state.contexts.filter(
    (ctx) => ctx.functionName
  );

  if (!functions.length) return;

  const { functionName, argIndex, ln, ch } = functions.pop();
  const keywordData = window.codeWorldSymbols[functionName];

  // don't show tooltip if function details or argument types are not known
  if (!keywordData || keywordData.declaration === functionName) return;

  const topDiv = document.createElement('div');

  topDiv.title = functionName;
  topDiv.id = 'function-details';

  const docDiv = document.createElement('div');
  docDiv.classList.add('function-tooltip-styling');

  const annotation = document.createElement('div');
  const returnedVal = renderDeclaration(
    annotation,
    keywordData,
    9999,
    argIndex
  );
  //TODO: Remove the if block once a better function parser is integrated.
  if (returnedVal === null) {
    annotation.remove();
    topDiv.remove();
    return;
  }
  annotation.className = 'hover-decl';
  docDiv.appendChild(annotation);
  topDiv.appendChild(docDiv);

  const widgetPos = {
    line: ln > 1 ? ln : cursor.line,
    ch: ch - functionName.length,
  };
  window.codeworldEditor.addWidget(widgetPos, topDiv, false, 'above', 'near');
}

function captureStart() {
  document.getElementById('runner').contentWindow.postMessage(
    {
      type: 'startRecord',
    },
    '*'
  );

  document.querySelector('#recordIcon').style.display = '';
  document.querySelector('#startRecButton').style.display = 'none';
  document.querySelector('#stopRecButton').style.display = '';
}

function stopRecording() {
  document.getElementById('runner').contentWindow.postMessage(
    {
      type: 'stopRecord',
    },
    '*'
  );

  document.querySelector('#recordIcon').style.display = 'none';
  document.querySelector('#startRecButton').style.display = '';
  document.querySelector('#stopRecButton').style.display = 'none';
}

function setMode(force) {
  if (window.buildMode === 'haskell') {
    if (force || window.codeworldEditor.getMode().name !== 'haskell') {
      window.codeworldEditor.setOption('mode', 'haskell');
    }
  } else {
    if (force || window.codeworldEditor.getMode().name !== 'codeworld') {
      window.codeworldEditor.setOption('mode', {
        name: 'codeworld',
        overrideKeywords: window.codeworldKeywords,
      });
    }
  }
}

function toggleTheme() {
  document.body.classList.toggle('dark-theme');
  const dark = document.body.classList.contains('dark-theme');
  window.codeworldEditor.setOption('theme', dark ? 'ambiance' : 'default');
  window.localStorage.setItem('darkMode', dark);
}

function changeFontSize(incr) {
  return () => {
    const elem = window.codeworldEditor.getWrapperElement();
    const style = window.getComputedStyle(elem);
    const fontParts = style['font-size'].match(/^([0-9]+)(.*)$/);
    let fontSize = 12;
    let fontUnit = 'px';
    if (fontParts.length >= 3) {
      fontSize = parseInt(fontParts[1]);
      fontUnit = fontParts[2];
    }
    fontSize += incr;
    if (fontSize < 8) fontSize = 8;
    elem.style.fontSize = fontSize + fontUnit;
    elem.parentElement.style.fontSize = (4 / 3) * fontSize + fontUnit;
    window.codeworldEditor.refresh();
  };
}

function help() {
  let url = `doc.html?shelf=help/${window.buildMode}.shelf`;
  let customClass = 'helpdoc';

  if (window.localStorage.getItem('darkMode') === 'true') {
    url += '&theme=dark-theme';
    customClass += ' dark-theme';
  }

  sweetAlert({
    html: `<iframe id="doc" style="width: 100%; height: 100%" class="dropbox" src="${url}"></iframe>`,
    customClass: customClass,
    allowEscapeKey: true,
    allowOutsideClick: true,
    showConfirmButton: false,
  }).then(() => {
    const docIframe = document.getElementById('doc');
    docIframe.contentWindow.savePosition();
  });
}

function newProject() {
  warnIfUnsaved(isEditorClean, () => {
    updateTreeOnNewProjectCreation();

    setCode('');

    document.title = '(new) - CodeWorld';
  });
}

function newFolder() {
  function successCallback() {
    setCode('');
  }
  createFolder(
    isEditorClean,
    getNearestDirectory(),
    window.buildMode,
    successCallback
  );
}

function formatSource() {
  if (window.buildMode === 'codeworld') {
    reformatCodeWorld();
  } else {
    reformatHaskell();
  }
}

function reformatCodeWorld() {
  const { codeworldEditor } = window;
  const doc = codeworldEditor.getDoc();
  const getLevel = (lineNum) => {
    const lineText = doc.getLine(lineNum);
    const pos = {
      line: lineNum,
      ch: Math.min(lineText.length, /^[\s]*/.exec(lineText)[0].length + 1),
    };
    const token = codeworldEditor.getTokenAt(pos, true);
    if (token.type === 'comment') return -1;

    const isItem = (token.type || '').split(' ').indexOf('layout') >= 0;
    if (isItem) {
      return token.state.contexts.length;
    } else {
      return token.state.contexts.length + 0.5;
    }
  };

  const oldLevel = [];
  for (let line = 0; line < doc.lineCount(); ++line) {
    oldLevel.push(getLevel(line));
  }
  for (let line = 0; line < doc.lineCount(); ++line) {
    if (oldLevel[line] === -1) continue;

    getLevel(line); // Forces an update to the token state.
    codeworldEditor.indentLine(line);
    while (getLevel(line) > oldLevel[line]) {
      const { prev, smart } = getIndentsAt(line, 'subtract');
      if (prev === 0) {
        break;
      } else if (smart >= 0 && smart !== prev) {
        codeworldEditor.indentLine(line, smart - prev);
      } else {
        codeworldEditor.indentLine(line, 'subtract');
      }
    }
    while (getLevel(line) < oldLevel[line]) {
      const { prev, smart } = getIndentsAt(line, 'add');
      if (smart >= 0 && smart !== prev) {
        codeworldEditor.indentLine(line, smart - prev);
      } else {
        codeworldEditor.indentLine(line, 'add');
      }
    }
  }
}

function reformatHaskell() {
  const { codeworldEditor, buildMode } = window;
  const doc = codeworldEditor.getDoc();
  const src = doc.getValue();
  const data = new FormData();
  data.append('source', src);
  data.append('mode', buildMode);

  sendHttp('POST', 'indent', data, (request) => {
    if (request.status === 200) {
      const reformattedSrc = request.responseText;

      if (reformattedSrc !== src) {
        let index = 0;
        const diffs = Diff.diffChars(src, reformattedSrc);
        for (let i = 0; i < diffs.length; i++) {
          const { value, count, added, removed } = diffs[i];
          if (added) {
            const pos = doc.posFromIndex(index);
            doc.replaceRange(value, pos, null, '+format');
            index += count;
          } else if (removed) {
            const fromPos = doc.posFromIndex(index);
            const toPos = doc.posFromIndex(index + count);
            doc.replaceRange('', fromPos, toPos, '+format');
          } else {
            index += count;
          }
        }
      }
    } else if (request.status === 500) {
      sweetAlert(
        'Oops!',
        'Could not format your code.  It may contains errors.',
        'error'
      );
    }
  });
}

window.addEventListener('message', (event) => {
  const { data } = event;

  passMessageToCodeExplorer(data);

  switch (data.type) {
  case 'loadSample':
    loadSample(isEditorClean, setCode, data.code);
    break;
  case 'programStarted':
    if (window.lastRunMessage) {
      const msg = window.lastRunMessage;
      window.lastRunMessage = null;
      setTimeout(() => {
        showRequiredChecksInDialog(msg);
      }, 500);
    }

    sweetAlert.close();
    break;
  case 'showGraphics': {
    const runner = document.getElementById('runner');
    runner.style.display = '';
    runner.focus();
    runner.contentWindow.focus();
    runner.contentWindow.postMessage(
      {
        type: 'graphicsShown',
      },
      '*'
    );
    break;
  }
  case 'consoleOut':
    if (data.str) {
      printMessage(data.msgType, data.str);
    }
    if (data.msgType === 'error') {
      markFailed();
    }
    break;
  case 'debugReady':
    window.debugAvailable = true;
    break;
  case 'debugActive':
    window.debugActive = true;
    break;
  case 'debugFinished':
    window.debugActive = false;
    break;
  default:
    break;
  }
});

function inspect() {
  if (window.debugActive) {
    document.getElementById('runner').contentWindow.postMessage(
      {
        type: 'stopDebug',
      },
      '*'
    );
  } else {
    document.getElementById('runner').contentWindow.postMessage(
      {
        type: 'startDebug',
      },
      '*'
    );
  }
}

function showRequiredChecksInDialog(msg) {
  const outputDiv = document.getElementById('message');
  if (outputDiv.classList.contains('error')) return;
  const matches = msg.match(
    /:: REQUIREMENTS ::((?:.|[\r\n])*):: END REQUIREMENTS ::/
  );
  if (!matches) {
    return;
  }
  const reqs = matches[1].split(/[\r\n]+/);
  const items = [];
  for (let i = 0; i < reqs.length; ++i) {
    const req = reqs[i];
    if (!req) continue;
    const bullet = req.slice(0, 4).toUpperCase();
    const rest = req.slice(4);
    if (bullet === '[Y] ') {
      // Successful requirement
      items.push([true, htmlEscapeString(rest)]);
    } else if (bullet === '[N] ') {
      // Unsuccessful requirement
      items.push([false, htmlEscapeString(rest)]);
    } else if (bullet === '[?] ') {
      // Indeterminate (usually a parse error in the requirement)
      items.push([undefined, htmlEscapeString(rest)]);
    } else if (items.length > 0) {
      // Detail message for the previous requirement.
      items[items.length - 1].push(req);
    }
  }
  const itemsHtml = items.map((item) => {
    const head = item[1];
    const rest = item.slice(2).join('<br>');
    const details = rest ? `<br><span class="req-details">${rest}</span>` : '';
    const itemclass =
      item[0] === undefined ? 'req-indet' : item[0] ? 'req-yes' : 'req-no';
    return `<li class="${itemclass}">${head}${details}</li>`;
  });
  sweetAlert({
    title: Alert.title('Requirements'),
    html: `<ul class="req-list">${itemsHtml.join('')}</ul>`,
    confirmButtonText: 'Dismiss',
    showCancelButton: false,
    closeOnConfirm: true,
  }).then(() => {
    const runner = document.getElementById('runner');
    if (!runner) return;
    if (runner.style.display === 'none') return;

    setTimeout(() => {
      runner.focus();
      runner.contentWindow.focus();
    }, 0);
  });
}

const htmlEscapeString = (() => {
  const el = document.createElement('div');
  return (str) => {
    el.textContent = str;
    return el.innerHTML;
  };
})();

function stopRun() {
  if (window.debugActive) {
    $('.code-explorer').hide();
    $('#message').show();
  }
  window.cancelCompile();

  run('', '', '', false, null);
}

function compile() {
  stopRun();

  const src = window.codeworldEditor.getValue();
  const compileGeneration = window.codeworldEditor
    .getDoc()
    .changeGeneration(true);

  const data = new FormData();
  data.append('source', src);
  data.append('mode', window.buildMode);

  let compileFinished = false;

  window.cancelCompile = () => {
    compileFinished = true;
    window.cancelCompile = () => {};
  };

  sweetAlert({
    title: Alert.title('Compiling'),
    text: 'Your code is compiling.  Please wait...',
    onOpen: () => {
      sweetAlert.showLoading();
      sweetAlert.getCancelButton().disabled = false;
    },
    showConfirmButton: false,
    showCancelButton: true,
    showCloseButton: false,
    allowOutsideClick: false,
    allowEscapeKey: false,
    allowEnterKey: false,
  }).then(() => {
    stopRun();
  });

  sendHttp('POST', 'compile', data, (request) => {
    if (compileFinished) return;

    const { status, responseText } = request;

    window.cancelCompile();

    const success = status === 200;

    let hash, dhash;
    if (status < 500) {
      if (responseText.length === 23) {
        hash = responseText;
        dhash = null;
      } else {
        try {
          const obj = JSON.parse(responseText);
          hash = obj.hash;
          dhash = obj.dhash;
        } catch (e) {
          hash = '';
        }
      }
    }

    if (!hash) {
      sweetAlert({
        title: Alert.title('Could not compile'),
        text: 'The compiler is unavailable.  Please try again later.',
        type: 'error',
      });
      return;
    }

    const data = new FormData();
    data.append('hash', hash);
    data.append('mode', window.buildMode);

    sendHttp('POST', 'runMsg', data, (request) => {
      const { status, responseText } = request;
      let msg = '';

      if (status === 200) {
        msg = responseText.replace(/^[\r\n]+|[\r\n]+$/g, '');
      } else if (status >= 400) {
        msg = 'Sorry!  Your program couldn\'t be run right now.';
      }
      if (msg !== '') msg += '\n\n';

      if (success) {
        run(hash, dhash, msg, false, compileGeneration);
      } else {
        sweetAlert.close();
        run(hash, '', msg, true, compileGeneration);
      }
    });
  });
}

function getCurrentProject() {
  const doc = window.codeworldEditor.getDoc();
  const selectedNode = DirTree.getSelectedNode();

  return {
    name: selectedNode ? selectedNode.name : 'Untitled',
    source: doc.getValue(),
    history: doc.getHistory(),
  };
}

function saveProjectCallback() {
  window.savedGeneration = window.codeworldEditor
    .getDoc()
    .changeGeneration(true);
  window.codeworldEditor.focus();

  document.title = document.title.replace('* ', '');
  $('.unsaved-changes').hide();
}

function saveProject() {
  const selectedNode = DirTree.getSelectedNode();

  if (selectedNode) {
    saveProjectBase(
      getNearestDirectory(),
      selectedNode.name,
      window.projectEnv,
      saveProjectCallback,
      getCurrentProject()
    );
  } else {
    saveProjectAs();
  }
}

function saveProjectAs() {
  saveProjectAsBase(saveProjectCallback, getCurrentProject());
}

function clearWorkspace() {
  DirTree.clearSelectedNode();

  setCode('');
}

function deleteFolder() {
  const path = getNearestDirectory();

  if (path === '') {
    return;
  }

  deleteFolder_(path, window.projectEnv, () => {
    window.savedGeneration = codeworldEditor.getDoc().changeGeneration(true);

    clearWorkspace();
  });
}

function deleteProject() {
  const selectedNode = DirTree.getSelectedNode();

  if (selectedNode && DirTree.isDirectory(selectedNode)) {
    deleteFolder();
    return;
  }

  const path = getNearestDirectory();
  deleteProject_(path, window.projectEnv, () => {
    window.savedGeneration = codeworldEditor.getDoc().changeGeneration(true);
    setCode('');
  });
}

function shareFolder() {
  shareFolder_(window.buildMode);
}

function downloadProject() {
  const blob = new Blob([window.codeworldEditor.getDoc().getValue()], {
    type: 'text/plain',
    endings: 'native',
  });
  const selectedNode = DirTree.getSelectedNode();
  const filename = `${selectedNode ? selectedNode.name : 'untitled'}.hs`;

  if (window.navigator.msSaveBlob) {
    window.navigator.msSaveBlob(blob, filename);
  } else {
    const elem = window.document.createElement('a');
    elem.href = window.URL.createObjectURL(blob);
    elem.download = filename;
    document.body.appendChild(elem);
    elem.click();
    document.body.removeChild(elem);
  }
}
