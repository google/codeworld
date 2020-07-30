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

import * as Alert from './utils/alert.js';
import * as Auth from './utils/auth.js';
import * as DirTree from './utils/directoryTree.js';
import * as Html from './utils/html.js';
import { sendHttp } from './utils/network.js';

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
  'collaborationOf',
  'simulationOf',
  'interactionOf',
  'debugInteractionOf',
  'debugSimulationOf',
  'cyan',
  'magenta',
  'azure',
  'chartreuse',
  'aquamarine',
  'violet',
  'rose',
  'hue',
  'saturation',
  'luminosity',
  'alpha',
];

const VAR_OR_CON = /^[a-zA-Z_][A-Za-z_0-9']*$/;
const QUALIFIER = /^[A-Z][A-Za-z_0-9']*[.]$/;

function definePanelExtension() {
  CodeMirror.defineExtension('addPanel', function (node) {
    const originWrapper = this.getWrapperElement();
    const wrapper = document.createElement('div');
    originWrapper.parentNode.insertBefore(wrapper, originWrapper);
    wrapper.appendChild(originWrapper);
    wrapper.insertBefore(node, wrapper.firstChild);
  });
}

// codeWorldSymbols is a variable containing annotations and documentation
// of builtin and user-defined variables.
// Expected format:
// codeWorldSymbols = {
//   codeWorldLogo: {
//     declaration: "codeWorldLogo :: Picture",
//     symbolStart: 0,
//     symbolEnd: 13,
//     insertText: "codeWorldLogo",
//     doc: "The CodeWorld logo."
//   }
// }
window.codeWorldSymbols = {};
window.codeWorldModules = {
  Prelude: {},
};
window.codeWorldBuiltins = {
  program: {
    declaration: 'program :: Program',
    doc: 'Your program.',
    symbolStart: 0,
    symbolEnd: 7,
    insertText: 'program',
  },
};

window.alreadyReportedErrors = new Set();

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

  const imports = [];

  lines.forEach((line) => {
    lineIndex++;

    const importExp = /^import\s+(qualified)?\s*([A-Z][A-Za-z0-9.']*)(\s+(as)\s+([A-Z][A-Za-z0-9.']*))?(\s+(hiding))?\s*([(]([^()]*|([(][^()]*[)])*)[)])?\s*$/;
    if (importExp.test(line)) {
      const match = importExp.exec(line);
      const qualified = Boolean(match[1]);
      const module = match[2];
      const asName = match[5] !== undefined ? match[5] : module;
      const hiding = Boolean(match[7]);
      const importList =
        match[9] &&
        match[9]
          .split(',')
          .map((s) => s.trim())
          .map((s) => (/[(].*[)]/.test(s) ? s.substr(1, s.length - 2) : s));
      imports.push({
        module: module,
        asName: asName,
        qualified: qualified,
        hiding: hiding,
        importList: importList,
      });
      return;
    }

    const docString = `Defined in your code on line ${lineIndex}.`;

    if (/^\w+\(.*/.test(line)) {
      // f(x, y) =
      const word = line.split('(')[0].trim();
      if (parseResults[word]) return;
      parseResults[word] = {
        declaration: word,
        insertText: word,
        doc: docString,
      };
    } else if (/^\S+\s*=/.test(line)) {
      // foo =
      const word = line.split('=')[0].trim();
      if (parseResults[word]) return;
      parseResults[word] = {
        declaration: word,
        insertText: word,
        doc: docString,
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
        insertText: word,
        doc: docString,
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
        insertText: word,
        doc: docString,
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
        insertText: word,
        doc: docString,
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
        insertText: word,
        doc: docString,
      };
    }
  });

  if (!imports.find((i) => i.module === 'Prelude')) {
    imports.push({
      module: 'Prelude',
      asName: 'Prelude',
      qualified: false,
      hiding: false,
      importList: undefined,
    });
  }

  if (window.buildMode === 'codeworld') {
    const symbols = Object.assign({}, window.codeWorldBuiltins);
    for (const i of imports) {
      if (i.module in window.codeWorldModules) {
        for (const symbol in window.codeWorldModules[i.module]) {
          if (i.importList) {
            if (i.hiding && i.importList.includes(symbol)) continue;
            if (!i.hiding && !i.importList.includes(symbol)) continue;
          }
          symbols[`${i.asName}.${symbol}`] =
            window.codeWorldModules[i.module][symbol];
          if (!i.qualified) {
            symbols[symbol] = window.codeWorldModules[i.module][symbol];
          }
        }
        symbols[i.asName] = {
          declaration: `module ${i.asName}`,
          symbolStart: 7,
          symbolEnd: 7 + i.asName.length,
          insertText: `${i.asName}.`,
          module: true,
          doc: null,
        };
      }
    }
    window.codeWorldSymbols = Object.assign(symbols, parseResults);
  } else {
    window.codeWorldSymbols = Object.assign({}, parseResults);
  }
}

function renderDeclaration(decl, keywordData, maxLen, argIndex = -1) {
  let column = 0;

  function addSegment(text, isWord, isBold) {
    function addSpan(content, wrappable) {
      const span = document.createElement('span');
      if (isWord) span.className = 'hint-word';
      if (isBold) span.style.fontWeight = 'bold';
      if (!wrappable) span.style.whiteSpace = 'nowrap';
      span.appendChild(document.createTextNode(content));
      decl.appendChild(span);
      column += content.length;
    }

    function trimFromTail(excess) {
      const tailLen = decl.lastChild.textContent.length;
      if (tailLen <= excess) {
        decl.removeChild(decl.lastChild);
        trimFromTail(excess - tailLen);
      } else {
        decl.lastChild.textContent = decl.lastChild.textContent.slice(
          0,
          tailLen - excess
        );
      }
    }

    const SYM = /^([:!#$%&*+./<=>?@\\^|~-]+)[^:!#$%&*+./<=>?@\\^|~-].*/;
    const NONSYM = /^([^:!#$%&*+./<=>?@\\^|~-]+)[:!#$%&*+./<=>?@\\^|~-].*/;
    while (text.length > 0) {
      const sym = SYM.exec(text);
      const split = sym || NONSYM.exec(text) || [text, text];

      addSpan(split[1], !sym);
      text = text.slice(split[1].length);
    }

    if (column > maxLen) {
      trimFromTail(column - maxLen + 3);
      addSpan('...', false);
    }
  }

  if (keywordData.symbolStart > 0) {
    addSegment(keywordData.declaration.slice(0, keywordData.symbolStart));
  }

  addSegment(
    keywordData.declaration.slice(
      keywordData.symbolStart,
      keywordData.symbolEnd
    ),
    true,
    false
  );

  if (keywordData.symbolEnd < keywordData.declaration.length) {
    const leftover = keywordData.declaration
      .slice(keywordData.symbolEnd)
      .replace(/\s+/g, ' ');
    if (argIndex >= 0) {
      // TODO: use a more sophisticated parser to fetch arguments,
      // and remove unnecessary subsequent checks.
      const parsedFunction = /^(\s*::\s*[(]?)([\w,\s]*)([)]?\s*->.*)$/.exec(
        leftover
      );
      if (!parsedFunction || parsedFunction.length <= 1) return null;

      const [head, args, tail] = parsedFunction.slice(1);
      const tokens = args.split(',');
      argIndex = Math.min(argIndex, tokens.length - 1);

      addSegment(head, false, false);
      for (let i = 0; i < tokens.length; i++) {
        if (i > 0) addSegment(',', false, false);
        addSegment(tokens[i], false, argIndex === i);
      }
      addSegment(tail, false, false);
    } else {
      addSegment(leftover, false, false);
    }
  }
  return decl;
}

function renderHover(keywordData, isTermReplaced) {
  if (!keywordData) return;

  const $wrapper = $('<div>');
  const $documentationContainer = $('<div>');
  const $fadeDiv = $('<div>');
  $fadeDiv.addClass('fade');
  const $annotation = $('<div>');
  renderDeclaration($annotation[0], keywordData, 9999);
  $annotation.addClass('hover-decl');

  $documentationContainer.append($annotation);

  if (keywordData.doc) {
    const $description = $('<div>');
    $description.html(keywordData.doc);
    $description.addClass('hover-doc');
    $documentationContainer.append($description);

    if (isTermReplaced) {
      const $noteAboutReplacement = $('<p>');
      $noteAboutReplacement.addClass('hint-description-replacement-note');
      $noteAboutReplacement.text('REPLACEMENT NOTE');
      $description.append($noteAboutReplacement);
    }
  }

  $wrapper.append($documentationContainer);
  $wrapper.append($fadeDiv);

  return $wrapper[0];
}

function onHover(cm, data, node) {
  if (data && data.token && data.token.string) {
    const prefix = getQualifierPrefix(
      cm,
      CodeMirror.Pos(data.token.state.line, data.token.start)
    );
    const token_name = data.token.string;
    if (hintBlacklist.indexOf(token_name) === -1) {
      const info = window.codeWorldSymbols[prefix + token_name];
      return renderHover(info);
    }
  }
}

function getQualifierPrefix(cm, pos) {
  let prefix = '';
  let start = pos.ch;
  while (start > 1) {
    let qtoken = cm.getTokenAt(CodeMirror.Pos(pos.line, start));
    let qual = qtoken.string;
    if (qtoken.string === '.') {
      qtoken = cm.getTokenAt(CodeMirror.Pos(pos.line, qtoken.start));
      qual = `${qtoken.string}.`;
    }
    if (!QUALIFIER.test(qual)) break;

    prefix = qual + prefix;
    start = qtoken.start;
  }
  return prefix;
}

function substitutionCost(a, b, fixedLen, isTermReplaced) {
  const insertCost = 1;
  const deleteCost = 1.5;
  const transCost = 1;
  const substCost = 1.5;
  const caseCost = 0.1;
  const redirectPenalty = 5;

  const d = Array(b.length + 1)
    .fill()
    .map(() => Array(a.length + 1));

  function scale(i) {
    return i >= fixedLen ? 10 : 100;
  }

  for (let i = 0; i <= a.length; i += 1) {
    for (let j = 0; j <= b.length; j += 1) {
      if (i === 0 && j === 0) {
        d[j][i] = 0;
        continue;
      } else if (i === 0) {
        d[j][i] = d[j - 1][i] + insertCost * scale(i);
      } else if (j === 0) {
        d[j][i] = d[j][i - 1] + deleteCost * scale(i - 1);
      } else {
        const replaceCost =
          a[i - 1] === b[j - 1]
            ? 0
            : a[i - 1].toLowerCase() === b[j - 1].toLowerCase()
              ? caseCost
              : substCost;

        d[j][i] = Math.min(
          d[j][i - 1] + deleteCost * scale(i - 1),
          d[j - 1][i] + insertCost * scale(i),
          d[j - 1][i - 1] + replaceCost * scale(i - 1)
        );
        if (i > 1 && j > 1 && a[i - 1] === b[j - 2] && a[i - 2] === b[j - 1]) {
          d[j][i] = Math.min(
            d[j][i],
            d[j - 2][i - 2] + transCost * scale(i - 2)
          );
        }
      }
    }
  }

  return (
    d[b.length][a.length] +
    scale(fixedLen) * (a.length - b.length) +
    (isTermReplaced ? redirectPenalty : 0)
  );
}

// Hints and hover tooltips
function registerStandardHints(successFunc) {
  let replacementTerms;

  CodeMirror.registerHelper('hint', 'codeworld', async (cm) => {
    if (!replacementTerms) {
      const blob = await fetch('./replacement_terms.json');
      replacementTerms = await blob.json();
    }
    const deleteOldHintDocs = () => {
      $('.hint-description').remove();
    };

    deleteOldHintDocs();

    const cur = cm.getCursor();
    const token = cm.getTokenAt(cur);

    // If the current token is whitespace, it can be split.
    let term = token.string.substr(0, cur.ch - token.start);
    let from = CodeMirror.Pos(cur.line, token.start);

    if (!VAR_OR_CON.test(term)) {
      term = '';
      from = cur;
    }

    const prefix = getQualifierPrefix(cm, from);

    // The found collection is organized into three tiers:
    //
    // 1. Exact match for the current token.
    // 2. Current token is a case-sensitive prefix.
    // 3. Others, to be presented as fuzzy matches.
    const found = [[], [], []];

    const hints = Object.keys(window.codeWorldSymbols);
    for (let i = 0; i < hints.length; i++) {
      const hint = hints[i];
      const parts = hint.split(/\.(?=[^.]+$)/);
      const hintPrefix = parts.length < 2 ? '' : `${parts[0]}.`;
      const hintIdent = parts.length < 2 ? hint : parts[1];
      if (!VAR_OR_CON.test(hintIdent)) continue;
      if (window.codeWorldSymbols[hint].module) {
        if (hint.startsWith(prefix)) {
          const candidate = {
            text: window.codeWorldSymbols[hint].insertText.substr(
              prefix.length
            ),
            details: window.codeWorldSymbols[hint],
            render: (elem) => {
              renderDeclaration(elem, window.codeWorldSymbols[hint], 50);
            },
          };
          if (hint === prefix + token.string) {
            found[0].push(candidate);
          } else if (hint.startsWith(prefix + term)) {
            found[1].push(candidate);
          } else {
            found[2].push(candidate);
          }
        }
      } else if (hintPrefix === prefix) {
        const candidate = {
          text: window.codeWorldSymbols[hint].insertText,
          details: window.codeWorldSymbols[hint],
          render: (elem) => {
            renderDeclaration(elem, window.codeWorldSymbols[hint], 50);
          },
        };
        if (hintIdent === token.string) {
          found[0].push(candidate);
        } else if (hintIdent.startsWith(term)) {
          found[1].push(candidate);
        } else {
          found[2].push(candidate);
        }
      }
    }

    // If there's a chance to find an exact match, clear out the fuzzy matches
    // so that the exact match is chosen.
    if (found[0].length + found[1].length === 1) {
      found[2] = [];
    }

    for (const [lookedUpTerm, replacementTerm] of Object.entries(
      replacementTerms
    )) {
      if (window.codeWorldSymbols[replacementTerm]) {
        found[2].push({
          text: window.codeWorldSymbols[replacementTerm].insertText,
          details: window.codeWorldSymbols[replacementTerm],
          render: (elem) => {
            renderDeclaration(
              elem,
              window.codeWorldSymbols[replacementTerm],
              50
            );
          },
          isTermReplaced: true,
          originalTerm: lookedUpTerm,
        });
      }
    }

    const options = found[0].concat(found[1], found[2]);
    for (const candidate of options) {
      const { isTermReplaced, originalTerm, text } = candidate;
      candidate.cost = substitutionCost(
        token.string,
        isTermReplaced ? originalTerm : text,
        term.length,
        isTermReplaced
      );
    }

    if (options.length > 0) {
      options.sort((a, b) => {
        if (a.cost < b.cost) return -1;
        if (a.cost > b.cost) return 1;
        return a.text.toLowerCase() < b.text.toLowerCase() ? -1 : 1;
      });

      let numGood;
      for (numGood = 1; numGood < options.length; numGood++) {
        if (numGood >= 16 && options[numGood].cost > 2 * options[0].cost + 50) {
          break;
        }
      }

      const goodOptions = options
        .slice(0, numGood)
        .reduce((result, current) => {
          const duplicate = result.find(({ text }) => text === current.text);

          return duplicate ? result : result.concat(current);
        }, []);

      const data = {
        list: goodOptions,
        from,
        to: VAR_OR_CON.test(term) ? CodeMirror.Pos(cur.line, token.end) : cur,
      };

      CodeMirror.on(data, 'close', deleteOldHintDocs);
      CodeMirror.on(data, 'pick', deleteOldHintDocs);
      CodeMirror.on(data, 'pick', (completion) => {
        if (completion.details.module) cm.showHint();
      });

      // Tracking of hint selection
      CodeMirror.on(data, 'select', (selection, elem) => {
        const hintsWidgetRect = elem.parentElement.getBoundingClientRect();
        const doc = document.createElement('div');
        deleteOldHintDocs();
        const hover = renderHover(selection.details, selection.isTermReplaced);
        if (hover) {
          doc.className += 'hint-description';
          doc.style.top = `${hintsWidgetRect.top}px`;
          doc.style.left = `${hintsWidgetRect.right}px`;
          doc.appendChild(hover);
          document.body.appendChild(doc);
        }
      });
      return data;
    }
  });

  sendHttp('GET', 'codeworld-base.txt', null, (request) => {
    let lines = [];
    if (request.status !== 200) {
      console.log('Failed to load autocomplete word list.');
    } else {
      lines = request.responseText.split('\n');
    }

    // Special case for "program", since it is morally a built-in name.
    window.codeworldKeywords['program'] = 'builtin';

    window.codeWorldModules = {};
    let module = null;
    let doc = '';
    lines.forEach((line) => {
      if (line.startsWith('module ')) {
        module = line.substr(7);
        if (!window.codeWorldModules[module]) {
          window.codeWorldModules[module] = {};
        }
        doc = '';
        return;
      }

      if (!module) {
        // Ignore anything outside of a module.
        doc = '';
        return;
      }

      if (module === 'Prelude' && line.startsWith('type Program')) {
        // We must intervene to hide the IO type.
        line = 'data Program';
      } else if (module === 'Prelude' && line.startsWith('type Truth')) {
        line = 'data Truth';
      } else if (module === 'Prelude' && line.startsWith('True ::')) {
        line = 'True :: Truth';
      } else if (module === 'Prelude' && line.startsWith('False ::')) {
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

        if (line[wordStart] === '(' && line[wordEnd - 1] === ')') {
          wordStart++;
          wordEnd--;
        }

        const word = line.substr(wordStart, wordEnd - wordStart);
        let isBlacklisted = false;
        if (module === 'Prelude') {
          if (hintBlacklist.indexOf(word) >= 0) isBlacklisted = true;
        } else {
          if (['RGB', 'HSL', 'RGBA'].indexOf(word) >= 0) isBlacklisted = true;
        }
        if (!isBlacklisted) {
          window.codeWorldModules[module][word] = {
            declaration: line,
            symbolStart: wordStart,
            symbolEnd: wordEnd,
            insertText: word,
            definingModule: module,
          };
          if (doc) {
            window.codeWorldModules[module][word].doc = doc;
          }
        }

        if (module === 'Prelude') {
          if (hintBlacklist.indexOf(word) >= 0) {
            window.codeworldKeywords[word] = 'deprecated';
          } else if (/^[A-Z:]/.test(word)) {
            window.codeworldKeywords[word] = 'builtin-2';
          } else {
            window.codeworldKeywords[word] = 'builtin';
          }
        }

        doc = '';
      }
    });

    successFunc();
  });
}

function loadTreeNodes(atNode) {
  const data = new FormData();
  data.append('mode', window.projectEnv);
  data.append('path', getNearestDirectory(atNode));

  showLoadingAnimation(atNode);

  sendHttp('POST', 'listFolders', data, (request) => {
    if (request.status === 200) {
      const treeNodes = JSON.parse(request.responseText);

      treeNodes.forEach((node) => {
        if (!node.id) {
          node.id = DirTree.createNodeId(node.type, node.name);
        }
      });

      $('#directoryTree').tree(
        'loadData',
        treeNodes.sort((a, b) => a.index > b.index),
        atNode
      );

      $('#directoryTree').tree('openNode', atNode);
    }

    hideLoadingAnimation();
  });
}

function moveDirTreeNode(moveFrom, moveTo, node, successFunc) {
  if (!Auth.signedIn()) {
    sweetAlert('Oops!', 'You must sign in before moving.', 'error');
    return;
  }
  const data = new FormData();
  data.append('mode', window.projectEnv);
  data.append('moveTo', moveTo);
  data.append('moveFrom', moveFrom);

  if (DirTree.isProject(node)) {
    data.append('isFile', 'true');
    data.append('name', node.name);
  } else {
    data.append('isFile', 'false');
  }

  sendHttp('POST', 'moveProject', data, (request) => {
    if (request.status !== 200) {
      sweetAlert(
        'Oops',
        'Could not move your project! Please try again.',
        'error'
      );
      return;
    }
    successFunc();
  });
}

function loadSample(isEditorClean, action, code) {
  if (isEditorClean()) {
    sweetAlert.close();
  }

  warnIfUnsaved(isEditorClean, () => {
    action(code);
  });
}

function updateDocumentTitle(isEditorClean) {
  const selectedNode = DirTree.getSelectedNode();
  let title = `${selectedNode ? selectedNode.name : '(new)'} - Codeworld`;

  if (isEditorClean && !isEditorClean()) {
    title = `* ${title}`;
  }

  document.title = title;
}

function updateProjectChangeMark(isEditorClean) {
  const selectedNode = DirTree.getSelectedNode();

  if (
    isEditorClean &&
    !isEditorClean() &&
    selectedNode &&
    DirTree.isProject(selectedNode)
  ) {
    const asterisk = selectedNode.element.getElementsByClassName(
      'unsaved-changes'
    )[0];

    if (asterisk) {
      asterisk.style.display = '';
    }
  } else {
    $('.unsaved-changes').hide();
  }
}

function warnIfUnsaved(isEditorClean, action) {
  if (isEditorClean()) {
    action();
  } else {
    sweetAlert({
      title: Alert.title('Warning'),
      text:
        'There are unsaved changes to your project. Continue and throw away your changes?',
      type: 'warning',
      showCancelButton: true,
      confirmButtonColor: '#DD6B55',
      confirmButtonText: 'Yes, discard my changes!',
    }).then((result) => {
      if (result && result.value) action();
    });
  }
}

function deleteProject_(path, buildMode, successFunc) {
  const selectedNode = DirTree.getSelectedNode();
  if (!selectedNode) return;

  if (!Auth.signedIn()) {
    sweetAlert('Oops', 'You must sign in to delete a project.', 'error');
    return;
  }

  const currentProjectName = selectedNode.name;
  const msg =
    'Deleting a project will throw away all work, and cannot be undone. ' +
    `Are you sure you want to delete ${currentProjectName}?`;

  sweetAlert({
    title: Alert.title('Warning'),
    text: msg,
    type: 'warning',
    showCancelButton: true,
    confirmButtonColor: '#DD6B55',
    confirmButtonText: 'Yes, delete it!',
  }).then((result) => {
    if (
      result.dismiss === sweetAlert.DismissReason.cancel ||
      result.dismiss === sweetAlert.DismissReason.backdrop
    ) {
      return;
    }

    const data = new FormData();
    data.append('name', currentProjectName);
    data.append('mode', buildMode);
    data.append('path', path);

    sendHttp('POST', 'deleteProject', data, (request) => {
      if (request.status === 200) {
        successFunc();

        $('#directoryTree').tree('removeNode', selectedNode);

        DirTree.clearSelectedNode();
      }
    });
  });
}

function saveProjectAsBase(successFunc, currentProject) {
  if (!Auth.signedIn()) {
    sweetAlert('Oops!', 'You must sign in to save files.', 'error');
    return;
  }

  const selectedNode = DirTree.getSelectedNode();
  const isDirectoryNode = DirTree.isDirectory(selectedNode);

  sweetAlert({
    title: Alert.title('Save As', 'mdi-cloud-upload'),
    html:
      selectedNode && isDirectoryNode
        ? `Enter a name for your project in folder <b>${$('<div>')
          .text(getNearestDirectory())
          .html()
          .replace(/ /g, '&nbsp;')}:`
        : 'Enter a name for your project:',
    input: 'text',
    inputValue: selectedNode && !isDirectoryNode ? selectedNode.name : '',
    confirmButtonText: 'Save',
    showCancelButton: true,
    closeOnConfirm: false,
  }).then((result) => {
    const parent = getNearestDirectory_();

    function localSuccessFunc() {
      const matches = parent.children.filter(
        (node) => node.name === result.value && DirTree.isProject(node)
      );
      let node;
      const type = DirTree.nodeTypes.PROJECT;
      const name = result.value;

      if (matches.length === 0) {
        node = $('#directoryTree').tree(
          'appendNode',
          {
            id: DirTree.createNodeId(type, name),
            name,
            type,
            data: JSON.stringify(currentProject),
          },
          parent
        );
      } else {
        node = matches[0];
      }

      $('#directoryTree').tree('selectNode', node);

      updateChildrenIndexes(parent);

      successFunc(result.value);
    }
    if (result && result.value) {
      saveProjectBase(
        getNearestDirectory(),
        result.value,
        window.projectEnv,
        localSuccessFunc,
        currentProject
      );
    }
  });
}

function saveProjectBase(path, projectName, mode, successFunc, currentProject) {
  if (!Auth.signedIn()) {
    sweetAlert('Oops!', 'You must sign in to save files.', 'error');
    return;
  }

  if (!projectName) return;

  function go() {
    sweetAlert({
      title: Alert.title(`Saving ${projectName} ...`),
      text: 'Saving your project. Please wait.',
      showConfirmButton: false,
      showCancelButton: false,
      showCloseButton: false,
      allowOutsideClick: false,
      allowEscapeKey: false,
      allowEnterKey: false,
    });

    const project = { ...currentProject };
    project['name'] = projectName;

    const data = new FormData();
    data.append(DirTree.nodeTypes.PROJECT, JSON.stringify(project));
    data.append('mode', mode);
    data.append('path', path);

    sendHttp('POST', 'saveProject', data, (request) => {
      sweetAlert.close();

      if (request.status !== 200) {
        sweetAlert(
          'Oops!',
          'Could not save your project!!!  Please try again.',
          'error'
        );
        return;
      }

      successFunc();
    });
  }

  const selectedNode = DirTree.getSelectedNode();

  if (
    (selectedNode && projectName === selectedNode.name) ||
    getNearestDirectory_().children.filter(
      (node) => node.name === projectName && DirTree.isProject(node)
    ).length === 0
  ) {
    go();
  } else {
    const msg = `${
      'Are you sure you want to save over another project?\n\n' +
      'The previous contents of '
    }${projectName} will be permanently destroyed!`;
    sweetAlert({
      title: Alert.title('Warning'),
      text: msg,
      type: 'warning',
      showCancelButton: true,
      confirmButtonColor: '#DD6B55',
      confirmButtonText: 'Yes, overwrite it!',
    }).then((result) => {
      if (result && result.value) {
        go();
      }
    });
  }
}

function deleteFolder_(path, buildMode, successFunc) {
  if (!Auth.signedIn()) {
    sweetAlert('Oops', 'You must sign in to delete a folder.', 'error');
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
    confirmButtonText: 'Yes, delete it!',
  }).then((result) => {
    if (
      result.dismiss === sweetAlert.DismissReason.cancel ||
      result.dismiss === sweetAlert.DismissReason.backdrop
    ) {
      return;
    }

    const data = new FormData();
    data.append('mode', buildMode);
    data.append('path', path);

    sendHttp('POST', 'deleteFolder', data, (request) => {
      if (request.status === 200) {
        const selectedNode = DirTree.getSelectedNode();
        $('#directoryTree').tree('removeNode', selectedNode);
        successFunc();
      }
    });
  });
}

function createFolder(isEditorClean, path, buildMode, successFunc) {
  warnIfUnsaved(isEditorClean, () => {
    if (!Auth.signedIn()) {
      sweetAlert('Oops!', 'You must sign in to create a folder.', 'error');
      return;
    }

    sweetAlert({
      title: Alert.title('Create Folder', 'mdi-folder-plus'),
      text: 'Enter a name for your folder:',
      input: 'text',
      inputValue: '',
      confirmButtonText: 'Create',
      showCancelButton: true,
    }).then((result) => {
      if (!result.value) {
        return;
      }

      sweetAlert.close();
      const data = new FormData();
      data.append('mode', buildMode);

      if (!path) {
        data.append('path', result.value);
      } else {
        data.append('path', `${path}/${result.value}`);
      }

      sendHttp('POST', 'createFolder', data, (request) => {
        if (request.status !== 200) {
          sweetAlert(
            'Oops',
            'Could not create your directory! Please try again.',
            'error'
          );
          return;
        }

        successFunc();

        let selectedNode = DirTree.getSelectedNode();
        const type = DirTree.nodeTypes.DIRECTORY;
        const name = result.value;

        if (!selectedNode) {
          selectedNode = $('#directoryTree').tree('getTree');
        }
        if (selectedNode && !DirTree.isDirectory(selectedNode)) {
          selectedNode = selectedNode.parent;
        }

        $('#directoryTree').tree(
          'appendNode',
          {
            id: DirTree.createNodeId(type, name),
            name,
            type,
            children: [],
          },
          selectedNode
        );

        updateChildrenIndexes(selectedNode);
      });
    });
  });
}

function loadProject(name, path, buildMode, successFunc) {
  if (!Auth.signedIn()) {
    sweetAlert('Oops!', 'You must sign in to open projects.', 'error');
    return;
  }

  sweetAlert({
    title: Alert.title(`Loading ${name} ...`),
    text: 'Please wait.',
    showConfirmButton: false,
    showCancelButton: false,
    showCloseButton: false,
    allowOutsideClick: false,
    allowEscapeKey: false,
    allowEnterKey: false,
  });

  const data = new FormData();
  data.append('name', name);
  data.append('mode', buildMode);
  data.append('path', path);

  sendHttp('POST', 'loadProject', data, (request) => {
    sweetAlert.close();
    if (request.status === 200) {
      const project = JSON.parse(request.responseText);
      successFunc(project);
    } else {
      sweetAlert(
        'Oops!',
        'Could not load the project!!!  Please try again.',
        'error'
      );
      return;
    }
  });
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
      a.href = `run.html?mode=${window.buildMode}&dhash=${window.deployHash}`;

      url = a.href;
      msg = 'Copy this link to share your program (but not code) with others!';
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
      animation: 'slide-from-bottom',
    }).then((result) => {
      if (result && result.value) {
        offerSource = !offerSource;
        go();
      }
    });
  }

  if (window.runningGeneration) {
    if (!window.codeworldEditor.getDoc().isClean(window.runningGeneration)) {
      sweetAlert({
        type: 'warning',
        text:
          'You have changed your code since running the program. ' +
          ' Rebuild so that you can share your latest code?',
        confirmButtonText: 'Yes, Rebuild',
        cancelButtonText: 'No, Share Old Program',
        showConfirmButton: true,
        showCancelButton: true,
      }).then((result) => {
        if (result && result.value) {
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

function shareFolder_(mode) {
  if (!Auth.signedIn()) {
    sweetAlert('Oops!', 'You must sign in to share your folder.', 'error');
    return;
  }

  const selectedNode = DirTree.getSelectedNode();

  if (!getNearestDirectory() || !(selectedNode && selectedNode.name)) {
    sweetAlert('Oops!', 'You must select a folder to share!', 'error');
    return;
  }

  const folderName = Html.encode(getNearestDirectory_().name);

  const data = new FormData();
  data.append('mode', mode);
  data.append('path', getNearestDirectory());

  sendHttp('POST', 'shareFolder', data, (request) => {
    if (request.status !== 200) {
      sweetAlert(
        'Oops!',
        'Could not share your folder! Please try again.',
        'error'
      );
      return;
    }

    const baseURL = window.location.origin + window.location.pathname;
    const shareHash = request.responseText;
    let gallery = false;

    function go() {
      let title;
      let url;
      let msg;
      let confirmText;

      if (gallery) {
        title = Alert.title('Share Gallery', 'mdi-presentation-play');
        msg = `Copy this link to make a gallery out of ${folderName}!`;
        url = new URL(
          `/gallery.html?path=/gallery/${shareHash}?mode=${mode}`,
          baseURL
        ).toString();
        confirmText = 'Share as Folder';
      } else {
        (title = Alert.title('Share Folder', 'mdi-folder-account-outline')),
        (msg = `Copy this link to share code in ${folderName} with others!`);
        url = `${baseURL}#${shareHash}`;
        confirmText = 'Share as Gallery';
      }

      sweetAlert({
        title: title,
        html: msg,
        input: 'text',
        inputValue: url,
        showConfirmButton: true,
        confirmButtonText: confirmText,
        closeOnConfirm: false,
        showCancelButton: true,
        cancelButtonText: 'Done',
        animation: 'slide-from-bottom',
      }).then((result) => {
        if (result && result.value) {
          gallery = !gallery;
          go();
        }
      });
    }

    go();
  });
}

function goto(line, col) {
  codeworldEditor.getDoc().setCursor(line - 1, col - 1);
  codeworldEditor.scrollIntoView(null, 100);
  codeworldEditor.focus();
}

// Expose this method globally - click handlers required it in preFormatMessage().
window.goto = goto;

function preFormatMessage(msg) {
  while (msg.match(/(\r\n|[^\x08]|)\x08/)) {
    msg = msg.replace(/(\r\n|[^\x08])\x08/g, '');
  }

  msg = Html.encode(msg)
    .replace(
      /program\.hs:(\d+):((\d+)(-\d+)?)/g,
      '<a href="#" onclick="goto($1, $3); return false;">Line $1, Column $2</a>'
    )
    .replace(
      /program\.hs:\((\d+),(\d+)\)-\((\d+),(\d+)\)/g,
      '<a href="#" onclick="goto($1, $2); return false;">Line $1-$3, Column $2-$4</a>'
    )
    .replace(
      /[A-Za-z0-9_-]*\.hs:(\d+):((\d+)(-\d+)?)/g,
      'In an imported module'
    )
    .replace(
      /program\.hs:\((\d+),(\d+)\)-\((\d+),(\d+)\)/g,
      'In an imported module'
    );
  return msg;
}

function printMessage(type, message) {
  const $outputBlock = $('#message');
  const $lastOutputBlock = $outputBlock.children().last();

  const formattedMessage = preFormatMessage(message);

  // Combine sequential log messages.
  if (
    type === 'log' &&
    $lastOutputBlock.length &&
    $lastOutputBlock.attr('class').includes('log')
  ) {
    const $lastOutputBlockMessageContent = $lastOutputBlock.find(
      '.message-content'
    );
    $lastOutputBlockMessageContent.append(formattedMessage);
  } else {
    const lines = formattedMessage.trim().split('\n');
    const $box = $('<div>');
    $box.addClass(`message-box ${type}`);

    const $messageGutter = $('<div>');
    $messageGutter.addClass('message-gutter');

    const $messageContent = $('<div>');
    $messageContent.addClass('message-wrapper');

    $box.append($messageGutter, $messageContent);
    $outputBlock.append($box);

    if (lines.length < 2 || type === 'log') {
      const $singleLineMsg = $('<div>');
      $singleLineMsg.addClass('message-content');
      $singleLineMsg.html(formattedMessage);

      $messageContent.append($singleLineMsg);
    } else {
      const formattedMessageFirstLine = lines[0];
      const formattedMessageWithoutFirstLine = lines.slice(1).join('\n');

      const $summary = $('<summary>');
      $summary.addClass('message-summary');
      $summary.html(formattedMessageFirstLine);

      const $details = $('<details>');
      $details.addClass('message-content');
      $details.attr('open', '');
      $details.append($summary, formattedMessageWithoutFirstLine);

      $messageContent.append($details);
    }
  }

  if (type === 'error' || type === 'warning') {
    if (!window.alreadyReportedErrors.has(scrubError(message))) {
      const $reportLink = $('<a>');
      $reportLink.attr('href', '#');
      $reportLink.addClass('report-unhelpful');
      $reportLink.on('click', (event) =>
        sendUnhelpfulReport(event, message, $reportLink)
      );
      $reportLink.text('Not helpful?');

      const $lastOutputBlockMessageContent = $outputBlock
        .children()
        .last()
        .find('.message-content');

      if ($lastOutputBlockMessageContent.children().length) {
        $lastOutputBlockMessageContent
          .find('.message-summary')
          .append($reportLink);
      } else {
        $lastOutputBlockMessageContent.append($reportLink);
      }
    }
  }

  $outputBlock.scrollTop($outputBlock.prop('scrollHeight'));
}

function sendUnhelpfulReport(event, message, $reportLink) {
  if (window.alreadyReportedErrors.has(scrubError(message))) {
    sweetAlert({
      type: 'info',
      text:
        'You have already reported this message.  Thank you for your feedback.',
    });
    $reportLink.hide();
    return;
  }
  sweetAlert({
    title: Alert.title('Report unhelpful message:', 'mdi-flag-variant'),
    text: 'The report will include your code.',
    input: 'textarea',
    inputPlaceholder: 'Anything else to add?',
    showConfirmButton: true,
    showCancelButton: true,
  }).then((result) => {
    if (!result || result.dismiss !== sweetAlert.DismissReason.confirm) return;

    const data = new FormData();
    data.append('title', 'User-reported unhelpful error message');
    data.append('label', 'error-message');

    let report = `**Program:** ${window.location.href}`;
    if (result.value) report += `\n\n**Comment:**\n\n${result.value}`;
    report += `\n\n**Message:**\n\n${message.replace(/^/gm, '    ')}`;
    data.append('message', report);
    sendHttp('POST', 'log', data);
    sweetAlert({
      type: 'success',
      text: 'Thank you for your feedback.',
    });

    $reportLink.hide();
    window.alreadyReportedErrors.add(scrubError(message));
  });
  event.preventDefault();
}

function scrubError(msg) {
  return msg
    .replace(/[a-zA-Z0-9_-]+\.hs:(\d+):((\d+)(-\d+)?)/g, '(loc)')
    .replace(/[a-zA-Z0-9_-]+\.hs:\((\d+),(\d+)\)-\((\d+),(\d+)\)/g, '(loc)');
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

// Get path to root dir in format root/sub1/sub2/etc
// starting from parent.
function pathToRootDir(nodeInit) {
  let node = Object.assign(nodeInit);
  const path = [];
  while (node.parent && node.parent.name !== '') {
    node = node.parent;
    path.push(node.name);
  }
  path.reverse();
  return path.join('/');
}

function initDirectoryTree(isEditorClean, loadProjectHandler, clearEditor) {
  const treeStateStorageKey = 'directoryTree';

  $('#directoryTree').tree({
    data: [],
    autoOpen: false,
    saveState: treeStateStorageKey,
    dragAndDrop: true,
    keyboardSupport: false,
    onCanSelectNode: (node) => {
      if (node.type === 'loadNotification') return false;
      return true;
    },
    onCanMove: (node) => {
      if (node.type === 'loadNotification') return false;
      return true;
    },
    onCanMoveTo: (moving_node, target_node, position) => {
      // Forbid move inside project node,
      // but allow to move before and after
      if (DirTree.isProject(target_node) && position === 'inside') {
        return false;
      }
      if (target_node.type === 'loadNotification') return false;
      return true;
    },
    closedIcon: $('<i class="mdi mdi-18px mdi-chevron-right"></i>'),
    openedIcon: $('<i class="mdi mdi-18px mdi-chevron-down"></i>'),
    onCreateLi: function (node, $li) {
      const titleElem = $li.find('.jqtree-element .jqtree-title');

      if (DirTree.isDirectory(node)) {
        if (node.is_open) {
          titleElem.before($('<i class="mdi mdi-18px mdi-folder-open"></i>'));
        } else {
          titleElem.before($('<i class="mdi mdi-18px mdi-folder"></i>'));
        }
      } else if (node.type === 'loadNotification') {
        titleElem.before($('<div style="float: left" class="loader"></div>'));
      } else if (DirTree.isProject(node)) {
        const asterisk = $('<i class="unsaved-changes"></i>');
        asterisk.css('display', 'none');
        titleElem.before($('<i class="mdi mdi-18px mdi-cube"></i>'));
        titleElem.after(asterisk);
      }
    },
  });

  if (localStorage.getItem(treeStateStorageKey)) {
    localStorage.removeItem(treeStateStorageKey);
  }

  $('#directoryTree').on('tree.move', (event) => {
    event.preventDefault();
    warnIfUnsaved(isEditorClean, () => {
      if (!Auth.signedIn()) {
        sweetAlert(
          'Oops!',
          'You must sign in to move this project or folder.',
          'error'
        );
        return;
      }
      const movedNode = event.move_info.moved_node;
      const isFile = DirTree.isProject(movedNode);
      let fromPath, name;
      fromPath = pathToRootDir(movedNode);
      if (isFile) {
        name = movedNode.name;
      } else if (fromPath) {
        fromPath = [fromPath, movedNode.name].join('/');
      } else {
        fromPath = movedNode.name;
      }
      const haveChildWithSameNameAndType = (movedNode, toNode) => {
        // check if target node have child node
        // which have same name and type as moving node
        // and not equals to moving node
        return (
          toNode.children.filter((ch) => {
            return ch.type === movedNode.type && ch.name === movedNode.name;
          }).length !== 0
        );
      };
      let toNode = event.move_info.target_node;
      const position = event.move_info.position;
      if (position === 'before' || position === 'after') {
        toNode = toNode.parent;
      }
      if (event.move_info.previous_parent === toNode) {
        // Reordering in same directory
        event.move_info.do_move();
        updateChildrenIndexes(toNode);
        return;
      }

      let toPath = pathToRootDir(toNode);
      if (toPath) {
        toPath = `${toPath}/${toNode.name}`;
      } else {
        toPath = toNode.name;
      }
      if (haveChildWithSameNameAndType(movedNode, toNode)) {
        // Replacement of existing project
        let msg, confirmText;
        if (DirTree.isProject(movedNode)) {
          msg = `${
            'Are you sure you want to save over another project?\n\n' +
            'The previous contents of '
          }${name} will be permanently destroyed!`;
          confirmText = 'Yes, overwrite it!';
        } else {
          msg = 'Are you sure you want to merge content of these directories?';
          confirmText = 'Yes, merge them!';
        }

        sweetAlert({
          title: Alert.title('Warning'),
          text: msg,
          type: 'warning',
          showCancelButton: true,
          confirmButtonColor: '#DD6B55',
          confirmButtonText: confirmText,
        }).then((result) => {
          if (result && result.value) {
            moveDirTreeNode(fromPath, toPath, movedNode, () => {
              toNode.children = toNode.children.filter((n) => {
                return (
                  movedNode === n ||
                  n.name !== movedNode.name ||
                  n.type !== movedNode.type
                );
              });
              event.move_info.do_move();
              updateChildrenIndexes(toNode);

              if (DirTree.isDirectory(movedNode)) {
                loadTreeNodes(movedNode);
                clearEditor();
              }
            });
          }
        });
      } else {
        // Regular moving
        moveDirTreeNode(fromPath, toPath, movedNode, () => {
          event.move_info.do_move();
          updateChildrenIndexes(toNode);
        });
      }
    });
  });
  $('#directoryTree').on('tree.open', (event) => {
    const { node: openedNode } = event;

    const folderIcon = openedNode.element.getElementsByClassName(
      'mdi-folder'
    )[0];
    if (folderIcon) {
      folderIcon.classList.replace('mdi-folder', 'mdi-folder-open');
    }

    loadTreeNodes(openedNode);

    const selectedNode = DirTree.getSelectedNode();

    if (selectedNode) {
      for (
        let parent = selectedNode.parent;
        parent !== null;
        parent = parent.parent
      ) {
        if (parent.id === openedNode.id) {
          DirTree.clearSelectedNode();
          updateDocumentTitle();
          clearEditor();

          break;
        }
      }
    }
  });
  $('#directoryTree').on('tree.close', (event) => {
    const folderIcon = event.node.element.getElementsByClassName(
      'mdi-folder-open'
    )[0];
    if (folderIcon) {
      folderIcon.classList.replace('mdi-folder-open', 'mdi-folder');
    }

    if ($('#directoryTree').tree('isNodeSelected', event.node)) {
      DirTree.clearSelectedNode();
      updateDocumentTitle();
    }
  });
  $('#directoryTree').on('tree.click', (event) => {
    event.preventDefault();

    const { node } = event;
    const isProjectNode = DirTree.isProject(node);

    // Deselection of selected project. Cancel it and do nothing.
    if (isProjectNode && $('#directoryTree').tree('isNodeSelected', node)) {
      return;
    }
    warnIfUnsaved(isEditorClean, () => {
      if (isProjectNode) {
        loadProjectHandler(node.name, pathToRootDir(node));
      } else {
        clearEditor();
      }

      $('#directoryTree').tree('selectNode', node);
    });
  });
  $('#directoryTree').on('tree.select', (event) => {
    const selectedNode = DirTree.getSelectedNode();

    const $deleteButton = $('#deleteButton');
    const $saveButton = $('#saveButton');
    const $downloadButton = $('#downloadButton');
    const $shareFolderButton = $('#shareFolderButton');

    $deleteButton.show();

    if (DirTree.isProject(selectedNode)) {
      $saveButton.show();
      $downloadButton.show();

      $shareFolderButton.hide();
    } else {
      $saveButton.hide();
      $downloadButton.hide();

      $shareFolderButton.show();
    }

    updateDocumentTitle();
  });
}

// Get directory nearest to selected node, or root if there is no selection
function getNearestDirectory_(node) {
  if (node) {
    const isDir = DirTree.isDirectory(node);
    const hasParent = Boolean(node.parent);

    if (isDir) {
      return node;
    } else if (hasParent) {
      return node.parent;
    }
    // root node
    return node;
  }

  const selectedNode = DirTree.getSelectedNode();

  if (!selectedNode) {
    // nearest directory is root
    return $('#directoryTree').tree('getTree');
  } else if (selectedNode && DirTree.isProject(selectedNode)) {
    return selectedNode.parent;
  } else if (selectedNode && DirTree.isDirectory(selectedNode)) {
    return selectedNode;
  }
}

function getNearestDirectory(node) {
  const selected = getNearestDirectory_(node);
  const path = pathToRootDir(selected);
  if (DirTree.isDirectory(selected)) {
    return path ? `${path}/${selected.name}` : selected.name;
  }
  return path;
}

function showLoadingAnimation(node) {
  if (!node) {
    node = $('#directoryTree').tree('getTree');
  }
  if (node === $('#directoryTree').tree('getTree')) {
    $('#directoryTree').tree(
      'appendNode',
      {
        name: 'Loading...',
        type: 'loadNotification',
      },
      node
    );
  } else {
    const target = node.element.getElementsByClassName(
      'jqtree-title jqtree_common'
    )[0];
    const elem = document.createElement('div');
    elem.classList.add('loader'); // float left
    elem.style.marginLeft = '5px';
    target.after(elem);
  }
}

function hideLoadingAnimation(node) {
  if (!node) {
    node = $('#directoryTree').tree('getTree');
  }
  if (node === $('#directoryTree').tree('getTree')) {
    node.children
      .filter((c) => {
        return c.type === 'loadNotification';
      })
      .forEach((c) => {
        $('#directoryTree').tree('removeNode', c);
      });
  } else {
    $('.loader').remove();
  }
}

function recalcChildrenIndexes(node) {
  let index = 0;
  node.children.forEach((n) => {
    n.index = index;
    index++;
  });
}

function updateChildrenIndexes(node) {
  if (Auth.signedIn() && node && node.children) {
    recalcChildrenIndexes(node);
    const repacked = [];
    for (let i = 0; i < node.children.length; i++) {
      repacked.push({
        type: node.children[i].type,
        name: node.children[i].name,
        index: node.children[i].index,
      });
    }
    const data = new FormData();
    data.append('mode', window.projectEnv);
    data.append('path', getNearestDirectory(node));
    data.append('entries', JSON.stringify(repacked));
    sendHttp('POST', 'updateChildrenIndexes', data, () => {});
  }
}

function updateTreeOnNewProjectCreation() {
  const selectedNode = DirTree.getSelectedNode();

  if (selectedNode && DirTree.isProject(selectedNode)) {
    DirTree.clearSelectedNode();

    // Select parent folder (if any) as new project's location is
    // determined by the selection in the directory tree.
    if (pathToRootDir(selectedNode)) {
      DirTree.selectNode(selectedNode.parent);
    }
  }
}

function run(hash, dhash, msg, error, generation) {
  window.runningGeneration = generation;
  window.debugAvailable = false;
  window.debugActive = false;
  window.lastRunMessage = msg;

  const runner = document.getElementById('runner');

  // Stop canvas recording if the recorder is active
  document.getElementById('runner').contentWindow.postMessage(
    {
      type: 'stopRecord',
    },
    '*'
  );

  if (hash) {
    window.location.hash = `#${hash}`;
    document.getElementById('shareButton').style.display = '';
  } else {
    window.location.hash = '';
    document.getElementById('shareButton').style.display = 'none';
  }

  if (dhash) {
    const loc = `run.html?dhash=${dhash}&mode=${window.buildMode}`;
    runner.contentWindow.location.replace(loc);
    if (
      Boolean(navigator.mediaDevices) &&
      Boolean(navigator.mediaDevices.getUserMedia)
    ) {
      document.getElementById('startRecButton').style.display = '';
    }
  } else {
    runner.contentWindow.location.replace('about:blank');
    document.getElementById('runner').style.display = 'none';
    document.getElementById('startRecButton').style.display = 'none';
  }

  const $shareFolderButton = $('#shareFolderButton');

  if (hash || msg) {
    $shareFolderButton.hide();

    window.mainLayout.show('east');
    window.mainLayout.open('east');
  } else {
    if ($shareFolderButton.css('display') !== 'none') {
      $shareFolderButton.show();
    }

    window.mainLayout.hide('east');
  }

  clearMessages();

  parseCompileErrors(msg).forEach((cmError) => {
    printMessage(cmError.severity, cmError.fullText);
  });

  if (error) markFailed();

  window.deployHash = dhash;
}

function toggleObsoleteCodeAlert() {
  const isRunning = $('#runner').css('display') !== 'none';
  // If true, current code isn't equal to previously compiled code.
  const isObsolete = window.codeworldEditor
    ? !window.codeworldEditor.getDoc().isClean(window.runningGeneration)
    : false;
  const $obsoleteAlert = $('#obsolete-code-alert');

  if (isRunning && isObsolete) {
    $obsoleteAlert.addClass('obsolete-code-alert-fadein');
    $obsoleteAlert.removeClass('obsolete-code-alert-fadeout');
  } else {
    $obsoleteAlert.addClass('obsolete-code-alert-fadeout');
    $obsoleteAlert.removeClass('obsolete-code-alert-fadein');
  }
}

function parseCompileErrors(rawErrors) {
  const errors = [];
  rawErrors = rawErrors.split('\n\n');
  rawErrors.forEach((err) => {
    const lines = err.trim().split('\n');
    const firstLine = lines[0].trim();
    const otherLines = lines
      .slice(1)
      .map((ln) => ln.trim())
      .join('\n');
    const re1 = /^program\.hs:(\d+):((\d+)-?(\d+)?): (\w+):(.*)/;
    const re2 = /^program\.hs:\((\d+),(\d+)\)-\((\d+),(\d+)\): (\w+):(.*)/;

    if (err.trim() === '') {
      // Ignore empty messages.
    } else if (re1.test(firstLine)) {
      const match = re1.exec(firstLine);

      const line = Number(match[1]) - 1;
      let startCol = Number(match[3]) - 1;
      let endCol;
      if (match[4]) {
        endCol = Number(match[4]);
      } else {
        const token = window.codeworldEditor
          .getLineTokens(line)
          .find((t) => t.start === startCol);
        if (token) {
          endCol = token.end;
        } else if (
          startCol >= window.codeworldEditor.getDoc().getLine(line).length
        ) {
          endCol = startCol;
          --startCol;
        } else {
          endCol = startCol + 1;
        }
      }

      const message = ((match[6] ? `${match[6].trim()}\n` : '') + otherLines)
        .replace(/program\.hs:(\d+):((\d+)(-\d+)?)/g, 'Line $1, Column $2')
        .replace(
          /program\.hs:\((\d+),(\d+)\)-\((\d+),(\d+)\)/g,
          'Line $1-$3, Column $2-$4'
        );

      errors.push({
        from: CodeMirror.Pos(line, startCol),
        to: CodeMirror.Pos(line, endCol),
        severity: match[5],
        fullText: err,
        message: message,
      });
    } else if (re2.test(firstLine)) {
      const match = re2.exec(firstLine);

      const startLine = Number(match[1]) - 1;
      const startCol = Number(match[2]) - 1;
      const endLine = Number(match[3]) - 1;
      const endCol = Number(match[4]);

      errors.push({
        from: CodeMirror.Pos(startLine, startCol),
        to: CodeMirror.Pos(endLine, endCol),
        severity: match[5],
        fullText: err,
        message: (match[6] ? `${match[6].trim()}\n` : '') + otherLines,
      });
    } else {
      console.log('Can not parse error header:', firstLine);
    }
  });
  return errors;
}

export {
  clearMessages,
  createFolder,
  definePanelExtension,
  deleteFolder_,
  deleteProject_,
  getNearestDirectory,
  initDirectoryTree,
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
};
