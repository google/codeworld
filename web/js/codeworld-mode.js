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

// This module is based on the Haskell mode from CodeMirror.
//
// CodeMirror is copyright (c) by Marijn Haverbeke and others
// Distributed under an MIT license: http://codemirror.net/LICENSE

'use strict';

CodeMirror.defineMode('codeworld', (_config, modeConfig) => {
    // This is a regular expression used in multiple contexts.
    const MULTICHAR_ESCAPE_REGEX =
        '\\\\NUL|\\\\SOH|\\\\STX|\\\\ETX|\\\\EOT|\\\\ENQ|\\\\ACK|\\\\BEL|\\\\BS|' +
        '\\\\HT|\\\\LF|\\\\VT|\\\\FF|\\\\CR|\\\\SO|\\\\SI|\\\\DLE|\\\\DC1|\\\\DC2|' +
        '\\\\DC3|\\\\DC4|\\\\NAK|\\\\SYN|\\\\ETB|\\\\CAN|\\\\EM|\\\\SUB|\\\\ESC|' +
        '\\\\FS|\\\\GS|\\\\RS|\\\\US|\\\\SP|\\\\DEL';

    const RE_WHITESPACE = /[ \v\t\f]+/;
    const RE_STARTMETA = /{-#/;
    const RE_STARTCOMMENT = /{-/;
    const RE_DASHES = /--+(?=$|[^:!#$%&*+./<=>?@\\^|~-]+)/;
    const RE_QUAL =
        /[A-Z][A-Za-z_0-9']*\.(?=[A-Za-z_:!#$%&*+./<=>?@\\^|~]|-[^-])/;
    const RE_VARID = /[a-z_][A-Za-z_0-9']*/;
    const RE_CONID = /[A-Z][A-Za-z_0-9']*/;
    const RE_VARSYM = /[!#$%&*+./<=>?@\\^|~-][:!#$%&*+./<=>?@\\^|~-]*/;
    const RE_CONSYM = /:[:!#$%&*+./<=>?@\\^|~-]*/;
    const RE_NUMBER =
        /[0-9]+(?:\.[0-9]+)?(?:[eE][+-]?[0-9]+)?|0[oO][0-7]+|0[xX][0-9a-fA-F]+/;
    const RE_CHAR = new RegExp(
        `'(?:[^\\\\']|\\\\[abfnrtv\\\\"']|\\\\^[A-Z@[\\\\\\]^_]|${ 
            MULTICHAR_ESCAPE_REGEX})'`);
    const RE_STRING = new RegExp(
        `"(?:[^\\\\"]|\\\\[abfnrtv\\\\"'&]|\\\\^[A-Z@[\\\\\\]^_]|${ 
            MULTICHAR_ESCAPE_REGEX})*"`);
    const RE_OPENBRACKET = /[([{]/;
    const RE_CLOSEBRACKET = /[)\]}]/;
    const RE_INCOMMENT = /(?:[^{-]|-(?=$|[^}])|\{(?=$|[^-]))*/;
    const RE_ENDCOMMENT = /-}/;

    const RE_OF = /of/;
    const RE_LET = /let/;
    const RE_IN = /in/;
    const RE_WHERE = /where/;
    const RE_DO = /do/;
    const RE_CASE = /case/;

    // Next non-comment or non-whitespace token
    // is start of layout context
    const TRIGGERED = 0;
    // Parsing is not layout sensetive
    const NORMAL = 1;

    function opening(bracket) {
        if (bracket === ')') return '(';
        if (bracket === ']') return '[';
        if (bracket === '}') return '{';
        return bracket;
    }

    // Start new layout context.
    function checkContextStarter(stream, state) {
        let match = null;
        if (stream.match(RE_LET)   ||
            stream.match(RE_WHERE) ||
            stream.match(RE_DO)    ||
            stream.match(RE_CASE)  ||
            stream.match(RE_OF)
           ) {
            let nextChar = stream.peek();
            // Check if it word like 'lettering' or 'offer'
            if (nextChar === null || nextChar === undefined || /\s/.exec(nextChar)){
                state.scanState = TRIGGERED;
                return true;
            }
        }
        return false;
    }

    // The state has the following properties:
    //
    // func:          The function to tokenize the remaining stream.
    // commentLevel:  Number of levels of block comments.
    // layoutContext: Explicit brackets + implicit layout contexts.
    // scanState:     When TRIGGERED - next token is start of layout context.

    function normal(stream, state) {
        let spanStyles = [];

        if (stream.match(RE_WHITESPACE)) return [];

        if (stream.match(RE_STARTMETA)) {
            state.func = blockComment('meta');
            ++state.commentLevel;
            return state.func(stream, state);
        }

        if (stream.match(RE_STARTCOMMENT)) {
            state.func = blockComment('comment');
            ++state.commentLevel;
            return state.func(stream, state);
        }

        if (stream.match(RE_DASHES)) {
            stream.skipToEnd();
            return ['comment'];
        }

        if (checkContextStarter(stream, state)) {
            return ['keyword'];
        }

        if (state.scanState === NORMAL && stream.match(RE_IN)) {
            // Cancel previous layout 'let' context
            state.layoutContext.pop();
            return ['keyword'];
        } else if (state.scanState === NORMAL) {
            let lastContext = state.layoutContext[state.layoutContext.length - 1];
            lastContext = lastContext === '{' ? 0 : lastContext;
            if (stream.sol() && stream.indentation() < lastContext) {
                // Close previous context
                state.layoutContext.pop();
            }
            if (stream.column() == lastContext || stream.column() == 0) {
                spanStyles.push('layout');
            }
        } else if (state.scanState === TRIGGERED && stream.match('{')) {
            // Cancel previous layout context because of next char is '{'
            state.layoutContext.pop();
            state.scanState = NORMAL;
        } else if (state.scanState === TRIGGERED) {
            state.layoutContext.push(stream.column());
            spanStyles.push('layout');
            state.scanState = NORMAL;
        }

        if (stream.match(RE_QUAL)) {
            spanStyles.push('qualifier');
            return spanStyles;
        };
        if (stream.match(RE_VARID) || stream.match(RE_VARSYM)) {
            spanStyles.push('variable');
            return spanStyles;
        };
        if (stream.match(RE_CONID) || stream.match(RE_CONSYM)) {
            spanStyles.push('variable-2');
            return spanStyles;
        };
        if (stream.match(RE_NUMBER)) {
            spanStyles.push('number');
            return spanStyles;
        };
        if (stream.match(RE_CHAR) || stream.match(RE_STRING)) {
            spanStyles.push('string');
            return spanStyles;
        };

        if (stream.match(RE_OPENBRACKET)) {
            state.layoutContext.push(stream.current());
            spanStyles.push(
                `bracket${state.layoutContext.length <= 7 ? `-${
                state.layoutContext.length - 1}` : ''}`);
            return spanStyles;
        }

        if (stream.match(RE_CLOSEBRACKET)) {
            const i = state.layoutContext.lastIndexOf(opening(stream.current()));
            if (i < 0) {
                spanStyles.push('bracket');
                return spanStyles;
            } else {
                while (state.layoutContext.length > i) state.layoutContext.pop();
                spanStyles.push(
                    `bracket${state.layoutContext.length <= 6 ? `-${
                    state.layoutContext.length}` : ''}`);
                return spanStyles;
            }
        }

        if (stream.eat(',')) return [];

        stream.next();
        return ['error'];
    }

    function blockComment(tokenType) {
        return (stream, state) => {
            if (state.commentLevel === 0) {
                state.func = normal;
                return [tokenType];
            }

            stream.match(RE_INCOMMENT);
            if (stream.match(RE_STARTCOMMENT)) {
                ++state.commentLevel;
                return state.func(stream, state);
            }
            if (stream.match(RE_ENDCOMMENT)) {
                --state.commentLevel;
                return state.func(stream, state);
            }
            return [tokenType];
        };
    }

    const wellKnownWords = (() => {
        const result = {};

        const keywords = [
            'case', 'class', 'data', 'default', 'deriving',
            'do', 'else', 'foreign',
            'if', 'import', 'in', 'infix', 'infixl',
            'infixr', 'instance', 'let',
            'module', 'newtype', 'of', 'then', 'type',
            'where', '_', '..', ':',
            '=', '::', '\\', '<-', '->', '@', '~', '=>',
            '|'
        ];

        for (let i = 0; i < keywords.length; ++i) {
            result[
                keywords[i]] = 'keyword';
        }

        const override = modeConfig.overrideKeywords;
        if (override) {
            for (const word in override) {
                if (override.hasOwnProperty(word)) {
                    result[word] = override[word];
                }
            }
        }

        return result;
    })();

    return {
        startState: () => {
            return {
                func: normal,
                commentLevel: 0,
                layoutContext: [],
                scanState: NORMAL
            };
        },

        token: (stream, state) => {
            const t = state.func(stream, state);
            const w = stream.current();
            if (wellKnownWords.hasOwnProperty(w)) {
                if (t.indexOf('variable') !== -1) t.pop(t.indexOf('variable'));
                if (t.indexOf('variable-2') !== -1) t.pop(t.indexOf('variable-2'));
                t.push(wellKnownWords[w]);
            };
            return t.join(' ');
        },

        blockCommentStart: '{-',
        blockCommentEnd: '-}',
        lineComment: '--',
        indent: null
    };
});
