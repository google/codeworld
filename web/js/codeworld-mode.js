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

    // The state has the following properties:
    //
    // func:         The function to tokenize the remaining stream.
    // commentLevel: Number of levels of block comments.
    // contexts:     Grouping contexts, from outermost to innermost.
    //               Array of objects
    //               {
    //                 value: '{'. '(', '[', 'let', or 'other'
    //                 column: integer,
    //               }
    // lastTokens:   Array of last up-to-two tokens encountered.

    function isBracket(context) {
        return context.value.length === 1;
    }

    function normal(stream, column, state) {
        if (stream.match(RE_WHITESPACE)) return null;

        if (stream.match(RE_STARTMETA)) {
            state.func = blockComment('meta');
            ++state.commentLevel;
            return state.func(stream, column, state);
        }

        if (stream.match(RE_STARTCOMMENT)) {
            state.func = blockComment('comment');
            ++state.commentLevel;
            return state.func(stream, column, state);
        }

        if (stream.match(RE_DASHES)) {
            stream.skipToEnd();
            return 'comment';
        }

        if (stream.match(RE_QUAL)) return 'qualifier';
        if (stream.match(RE_VARID) || stream.match(RE_VARSYM)) return 'variable';
        if (stream.match(RE_CONID) || stream.match(RE_CONSYM)) return 'variable-2';
        if (stream.match(RE_NUMBER)) return 'number';
        if (stream.match(RE_CHAR) || stream.match(RE_STRING)) return 'string';
        if (stream.match(RE_OPENBRACKET) || stream.match(RE_CLOSEBRACKET)) return 'bracket';
        if (stream.eat(',')) return 'comma';

        stream.next();
        return 'error';
    }

    function blockComment(tokenType) {
        return (stream, column, state) => {
            if (state.commentLevel === 0) {
                state.func = normal;
                return tokenType;
            }

            stream.match(RE_INCOMMENT);
            if (stream.match(RE_STARTCOMMENT)) {
                ++state.commentLevel;
                return state.func(stream, column, state);
            }
            if (stream.match(RE_ENDCOMMENT)) {
                --state.commentLevel;
                return state.func(stream, column, state);
            }
            return tokenType;
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

    function parseToken(stream, column, state) {
        const t = state.func(stream, column, state);
        if (['variable', 'variable-2'].indexOf(t) !== -1) {
            const w = stream.current();
            if (wellKnownWords.hasOwnProperty(w)) {
                return wellKnownWords[w];
            }
        }
        return t;
    }

    function updateLayout(token, column, style, state) {
        function opening(bracket) {
            if (bracket === ')') return '(';
            if (bracket === ']') return '[';
            if (bracket === '}') return '{';
            if (bracket === 'in') return 'let';
        }

        // Close any implicit contexts when there are tokens in columns to
        // the left.
        const toClose = state.contexts.findIndex(ctx => !isBracket(ctx) && ctx.column > column);
        let foundLet = false;
        if (toClose >= 0) {
            foundLet = state.contexts[toClose].value === 'let';
            while (state.contexts.length > toClose) state.contexts.pop();
        }

        // Create any new implicit contexts called for by layout rules.
        if (state.lastTokens.length === 0) {
            if (token !== 'module' && token !== '{') {
                state.contexts.push({
                    value: 'where', // There's an implied "module Main where"
                    column: column
                });
            }
        } else {
            const triggered = state.lastTokens.slice(-1).join(' ') === 'where' ||
                state.lastTokens.slice(-1).join(' ') === 'of' ||
                state.lastTokens.slice(-1).join(' ') === 'do' ||
                state.lastTokens.slice(-1).join(' ') === 'let' ||
                state.lastTokens.slice(-2).join(' ') === '\\ case';

            if (triggered && token !== '{') {
                state.contexts.push({
                    value: state.lastTokens.slice(-1)[0],
                    column: column
                });
            }
        }

        // Update lastTokens so that layout rules can be applied next time.
        state.lastTokens = state.lastTokens.slice(-1);
        state.lastTokens.push(token);

        // Open new contexts for brackets.  These should be inside the
        // implicit contexts created by layout.
        if (RE_OPENBRACKET.test(token)) {
            const level = state.contexts.filter(isBracket).length;
            if (level <= 6) style = `${style}-${level}`;

            let functionName = null;
            if (token === '(' && state.lastTokens.length === 2) {
                if (RE_VARID.test(state.lastTokens[0]) || RE_CONID.test(state.lastTokens[0])) {
                    functionName = state.lastTokens[0];
                }
            }

            state.contexts.push({
                value: token,
                column: column,
                functionName,
                argIndex: 0
            });
        }

        // Close implicit contexts on comma.
        if (token === ',') {
            while(true) {
                const topContext = state.contexts.pop();
                if (!state.contexts.length || isBracket(topContext)) {
                    if (topContext.hasOwnProperty("argIndex")) topContext.argIndex++;
                    state.contexts.push(topContext);
                    break;
                }
            }
        }

        // Close contexts when syntax demands that we do so.
        if (RE_CLOSEBRACKET.test(token) || (!foundLet && token === 'in')) {
            state.contexts.reverse();
            const reverseIndex = state.contexts.findIndex(ctx => ctx.value === opening(token));
            state.contexts.reverse();
            if (reverseIndex >= 0) {
                const index = state.contexts.length - reverseIndex - 1;
                while (state.contexts.length > index) state.contexts.pop();
                if (token !== 'in') {
                    const level = state.contexts.filter(isBracket).length;
                    if (level <= 6) style = `${style}-${level}`;
                }
            }
        }

        const ctx = state.contexts.find(ctx => ctx.column === column);
        const isLayout = ctx && !isBracket(ctx);
        return isLayout ? `${style} layout` : style;
    }

    return {
        startState: () => {
            return {
                func: normal,
                commentLevel: 0,
                contexts: [],
                lastTokens: []
            };
        },
        token: (stream, state) => {
            const column = stream.column();
            const style = parseToken(stream, column, state);

            // Ignore whitespaces and comments for layout purposes.
            if (style === null || style === 'comment' || style === 'meta') {
                return style;
            }

            return updateLayout(stream.current(), column, style, state);
        },
        blockCommentStart: '{-',
        blockCommentEnd: '-}',
        lineComment: '--',
        indent: null
    };
});
