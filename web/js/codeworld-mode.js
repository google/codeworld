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
    const RE_CLOSEBRACKET = /[)\]}]/;
    const RE_INCOMMENT = /(?:[^{-]|-(?=$|[^}])|\{(?=$|[^-]))*/;
    const RE_ENDCOMMENT = /-}/;

    const RE_MODULE = /\bmodule\b/;
    const RE_OF = /\bof\b/;
    const RE_LET = /\blet\b/;
    const RE_IN = /\bin\b/;
    const RE_WHERE = /\bwhere\b/;
    const RE_DO = /\bdo\b/;
    const RE_LCASE = /\\case\b/; // Lambda case

    // Start of module. If next token is 'module'
    // or '{' then parse happens in NORMAL mode
    // otherwise in TRIGGERED
    const START = 0;
    // Next non-comment or non-whitespace token
    // is start of layout context
    const TRIGGERED = 1;
    // Parsing inside last layout
    const NORMAL = 2;

    // Possible values for context objects
    const BRACE = '{';
    const BRACKET = '(';
    const SQUARE = '[';
    const WHERE = 'where';
    const LET = 'let';
    const OF = 'of';
    const LCASE = 'lcase';
    const DO = 'do';
    const ROOT = 'ROOT';

    function rmRightContexts(column, state) {
        const ctx = state.layoutContext.pop();
        if (!ctx) return null;
        if (ctx.type === 'explicit') {
            let result = rmRightContexts(column, state);
            // put bracket back
            state.layoutContext.push(ctx);
            return result;
        }
        if (ctx.column > column) {
            // Don't push righter context back (close context)
            // search context to the left from current,
            // close others on the way
            return rmRightContexts(column, state);
        } else {
            // This context on same level as passed column or to left
            // put it back and return
            state.layoutContext.push(ctx);
            return ctx;
        };
    };

    function getExplicitCtxNesting(state) {
        const ctx = state.layoutContext.pop();
        if (ctx && ctx.type === 'explicit') {
            state.layoutContext.push(ctx);
            return ctx.nesting + 1;
        }
        return 0;
    }

    function rmMatchingExplicitContext(closing, state) {
        const ctx = state.layoutContext.pop();
        if (ctx === undefined) {
            return 0;
        }
        if (ctx.type === 'explicit' && ctx.value === opening(closing)) {
            return ctx.nesting + 1;
        } else if (ctx) {
            let result = rmMatchingExplicitContext(closing, state);
            state.layoutContext.push(ctx);
            return result;
        }
        return 0;
    }

    function opening(bracket) {
        if (bracket === ')') return '(';
        if (bracket === ']') return '[';
        if (bracket === '}') return '{';
        return bracket;
    }

    // Check if there start of new layout context.
    // Returns {style: <style of current token>, context:<layout context>}
    function checkContextStarter(stream, state) {
        // if (state.layoutScanState === START && stream.match(RE_MODULE)) {
        //     state.layoutScanState = NORMAL;
        //     return {
        //         style:'keyword',
        //         contextDraft: {
        //             type: 'implicit'
        //         }
        //     };
        // } else
        if (state.layoutScanState === START && stream.match(BRACE)) {
            state.layoutScanState = TRIGGERED;
            return {
                style: 'bracket-0',
                contextDraft: {
                    type: 'explicit',
                    value: BRACE,
                    nesting: 0
                }
            };
        } else if (state.layoutScanState === START) {
            state.layoutScanState = TRIGGERED;
            return {
                style: null,
                contextDraft: {
                    type: 'implicit',
                    value: ROOT,
                    nesting: 0
                }};
        }

        if (stream.match(BRACE)) {
            state.layoutScanState = TRIGGERED;
            return {
                style: 'bracket',
                contextDraft: {
                    type: 'explicit',
                    value: BRACE
                }
            };
        };

        if (stream.match(SQUARE)) {
            state.layoutScanState = TRIGGERED;
            return {
                style: 'bracket',
                contextDraft: {
                    type: 'explicit',
                    value: SQUARE
                }
            };
        };

        if (stream.match(BRACKET)) {
            state.layoutScanState = TRIGGERED;
            return {
                style: 'bracket',
                contextDraft: {
                    type: 'explicit',
                    value: BRACKET
                }
            };
        };

        if (stream.match(RE_LET)){
            state.layoutScanState = TRIGGERED;
            return {
                style: 'keyword',
                contextDraft: {
                    type: 'implicit',
                    value: LET,
                    line: stream.lineOracle.line
                }
            };
        };
        if (stream.match(RE_WHERE)) {
            state.layoutScanState = TRIGGERED;
            return {
                style: 'keyword',
                contextDraft: {
                    type: 'implicit',
                    value: WHERE
                }
            };
        };
        if (stream.match(RE_DO)) {
            state.layoutScanState = TRIGGERED;
            return {
                style: 'keyword',
                contextDraft: {
                    type: 'implicit',
                    value: DO
                }
            };
        };

        if (stream.match(RE_LCASE)) {
            state.layoutScanState = TRIGGERED;
            return {
                style: 'keyword',
                contextDraft: {
                    type: 'implicit',
                    value: LCASE
                }
            };
        };

        if (stream.match(RE_OF)) {
            state.layoutScanState = TRIGGERED;
            return {
                style: 'keyword',
                contextDraft: {
                    type: 'implicit',
                    value: OF
                }
            };
        };
        return null;
    }

    // The state has the following properties:
    //
    // func:            The function to tokenize the remaining stream.
    // commentLevel:    Number of levels of block comments.
    // layoutContext:   Explicit brackets and implicit layout contexts. Array of objects
    //                   { type: explicit | implicit,
    //                     value: { | ( | [ | where | let | of | lambdaCase | do | root,
    //                     column: integer,
    //                     line: integer, // required for detecting one line let expressions
    //                     nesting: integer, // required for brackets styling
    //                   }
    // layoutScanState: When TRIGGERED - next token is start of layout context.

    function normal(stream, state) {
        const spanStyles = [];

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

        const contextStarter = checkContextStarter(stream, state);
        if (contextStarter) {
            const ctxDraft = contextStarter.contextDraft;
            if (ctxDraft.type === 'explicit' && state.layoutScanState === TRIGGERED) {
                ctxDraft.nesting = getExplicitCtxNesting(state);
                let bracketStyleIndex = ctxDraft.nesting;
                if (bracketStyleIndex !== 7) {
                    bracketStyleIndex = bracketStyleIndex % 7;
                };
                contextStarter.style = 'bracket-' + bracketStyleIndex;
                state.layoutScanState = NORMAL;
            };
            state.layoutContext.push(ctxDraft);
            return [contextStarter.style];
        }

        let currentContext = state.layoutContext.pop();

        // Update current implicit context
        if (currentContext && currentContext.type === 'implicit' && state.layoutScanState == TRIGGERED) {
            currentContext.column = stream.column();
            const prevImplContext = rmRightContexts(stream.column(), state);
            if (prevImplContext) {
                // Implicit contexts have pass-through nesting
                currentContext.nesting = prevImplContext.nesting + 1;
            } else {
                currentContext.nesting = 0;
            }
            state.layoutScanStxate = NORMAL;
        }

        if (currentContext) {
            state.layoutContext.push(currentContext);
        }

        currentContext = rmRightContexts(stream.column(), state);

        if (currentContext && currentContext.type === 'implicit'
            && stream.column() === currentContext.column) {
            spanStyles.push('layout');
        }


        if (stream.match(RE_IN)) {
            // Cancel previous layout 'let' context
            // TODO close all contexts from this to prev let
            // TODO handle one line let expressions
            state.layoutContext.pop();
            return ['keyword'];
        }

        if (stream.match(RE_QUAL)) {
            spanStyles.push('qualifier');
            return spanStyles;
        }
        if (stream.match(RE_VARID) || stream.match(RE_VARSYM)) {
            spanStyles.push('variable');
            return spanStyles;
        }
        if (stream.match(RE_CONID) || stream.match(RE_CONSYM)) {
            spanStyles.push('variable-2');
            return spanStyles;
        }
        if (stream.match(RE_NUMBER)) {
            spanStyles.push('number');
            return spanStyles;
        }
        if (stream.match(RE_CHAR) || stream.match(RE_STRING)) {
            spanStyles.push('string');
            return spanStyles;
        }

        if (stream.match(RE_CLOSEBRACKET)) {
            let style = rmMatchingExplicitContext(stream.current(), state);
            if (style !== 7) {
                style = style % 7;
            };
            spanStyles.push('bracket-' + style);
            return spanStyles;
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
                layoutScanState: START,
                implicitContextNesting: 0
            };
        },

        token: (stream, state) => {
            const t = state.func(stream, state);
            const w = stream.current();
            if (wellKnownWords.hasOwnProperty(w)) {
                if (t.indexOf('variable') !== -1) t.splice(t.indexOf('variable'), 1);
                if (t.indexOf('variable-2') !== -1) t.splice(t.indexOf('variable-2'), 1);
                t.push(wellKnownWords[w]);
            }
            return t.join(' ');
        },
        blockCommentStart: '{-',
        blockCommentEnd: '-}',
        lineComment: '--',
        indent: null
    };
});
