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

// This module is based on the Haskell mode from CodeMirror.
//
// CodeMirror is copyright (c) by Marijn Haverbeke and others
// Distributed under an MIT license: http://codemirror.net/LICENSE

'use strict';

CodeMirror.defineMode('codeworld', (config, modeConfig) => {
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
        `'(?:[^\\\\']|\\\\[abfnrtv\\\\"']|\\\\^[A-Z@[\\\\\\]^_]|${MULTICHAR_ESCAPE_REGEX})'`);
    const RE_STRING = new RegExp(
        `"(?:[^\\\\"]|\\\\[abfnrtv\\\\"'&]|\\\\^[A-Z@[\\\\\\]^_]|${MULTICHAR_ESCAPE_REGEX})*"`);
    const RE_OPENBRACKET = /[([{]/;
    const RE_CLOSEBRACKET = /[)\]}]/;
    const RE_INCOMMENT = /(?:[^{-]|-(?=$|[^}])|\{(?=$|[^-]))*/;
    const RE_ENDCOMMENT = /-}/;
    const RE_ELECTRIC_START = /^\s*(?:[:!#$%&*+./<=>?@^|~,)\]}-]+|where\b|in\b|of\b|then\b|else\b|deriving\b)/;
    const RE_ELECTRIC_INPUT = /^\s*(?:[:!#$%&*+./<=>?@^|~,)\]}-]+|where|in|of|then|else|deriving).?$/;
    const RE_NEGATIVE_NUM = /^\s*[-]($|[^!#$%&*+./<=>?@\\^|~-])/;

    // The state has the following properties:
    //
    // func:         The function to tokenize the remaining stream.
    // commentLevel: Number of levels of block comments.
    // continued:    The last token cannot end an expression or layout statement.
    // contexts:     Grouping contexts, from outermost to innermost.
    //               Array of objects
    //               {
    //                 value: '{'. '(', '[', 'let', or 'other'
    //                 column: base indent column for the context,
    //                 ln: line at which the context started
    //                 ch: column at which the context started
    //                 fresh: for brackets, indicates alignment is undecided
    //                 guardCol: column to align '|' characters for guards, or -1
    //                 guardAlign: column to align guards after '|' characters, or -1
    //                 eqLine: Line number for the latest equal sign
    //                 rhsAlign: column to indent equation right-hand side, or -1
    //                 functionName: string (optional)
    //                 argIndex: integer (optional)
    //               }
    // lastTokens:   Array of last up-to-two tokens encountered.
    // line:         Current line number

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

        state.continued = false;

        if (stream.match(RE_QUAL)) return 'qualifier';
        if (stream.match(RE_CONID)) return 'variable-2';
        if (stream.match(RE_NUMBER)) return 'number';
        if (stream.match(RE_CHAR) || stream.match(RE_STRING)) return 'string';
        if (stream.match(RE_CLOSEBRACKET)) return 'bracket';
        if (stream.match(RE_OPENBRACKET)) {
            state.continued = true;
            return 'bracket';
        }

        if (stream.match(RE_VARID)) {
            if (['case', 'of', 'class', 'data', 'instance', 'deriving',
                'do', 'if', 'then', 'else', 'import', 'infix', 'infixl',
                'infixr', 'instance', 'let', 'in', 'module', 'newtype',
                'type', 'where'
            ].indexOf(stream.current()) >= 0) {
                state.continued = true;
            }
            return 'variable';
        }

        if (stream.match(RE_VARSYM)) {
            state.continued = true;
            return 'variable';
        }

        if (stream.match(RE_CONSYM)) {
            state.continued = true;
            return 'variable-2';
        }

        if (stream.eat(',')) {
            // Set continued to false, so next item is aligned.
            state.continued = false;
            return 'comma';
        }

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

    function parseToken(stream, state) {
        const t = state.func(stream, stream.column(), state);
        if (['variable', 'variable-2'].indexOf(t) !== -1) {
            const w = stream.current();
            if (wellKnownWords.hasOwnProperty(w)) {
                return wellKnownWords[w];
            }
            if (w === 'qualified' &&
                state.lastTokens.slice(-1).join(' ') === 'import') {
                return 'keyword';
            }
        }
        return t;
    }

    function opening(bracket) {
        if (bracket === ')') return '(';
        if (bracket === ']') return '[';
        if (bracket === '}') return '{';
        if (bracket === 'in') return 'let';
    }

    function updateLayout(token, column, style, state) {
        // Close any implicit contexts when there are tokens in columns to
        // the left.
        let foundLet = false;
        for (let i = 0; i < state.contexts.length; ++i) {
            const ctx = state.contexts[i];

            if (ctx.column > column && !isBracket(ctx)) {
                foundLet = ctx.value === 'let';
                state.contexts = state.contexts.slice(0, i);
                break;
            }
        }

        // Update alignment columns for the innermost context.
        if (state.contexts.length > 0 && token !== ',') {
            const openBracket = RE_OPENBRACKET.test(token);
            const ctx = state.contexts[state.contexts.length - 1];

            if (ctx.fresh) {
                const sameLine = state.line === ctx.ln;
                if (!sameLine || !openBracket) {
                    ctx.column = column;
                    ctx.fresh = false;
                }
            } else {
                ctx.column = Math.min(ctx.column, column);
            }

            if (ctx.guardAlign === -1 && ctx.guardCol >= 0) {
                let target = column;
                for (let i = state.contexts.length - 1; i >= 0; --i) {
                    if (ctx.guardAlign === -1) ctx.guardAlign = target;
                    target = Math.min(target, ctx.ch, ctx.column);
                }
            }

            if (ctx.guardCol === -1 && token === '|') {
                ctx.guardCol = column;
            }

            if (ctx.rhsAlign === -1 && ctx.eqLine >= 0) {
                const sameLine = ctx.eqLine === state.line;
                if (!sameLine || !openBracket) {
                    // We must update this and all parent alignments, because some
                    // parents may be deferred by an open bracket.
                    let target = column;
                    for (let i = state.contexts.length - 1; i >= 0; --i) {
                        if (ctx.rhsAlign === -1) ctx.rhsAlign = target;
                        target = Math.min(target, ctx.ch);
                        if (!ctx.fresh) target = Math.min(target, ctx.column);
                    }
                }
            }

            if (ctx.rhsAlign === -1 && token === '=') {
                ctx.eqLine = state.line;
            }

            if (token === '|' || token === 'where') {
                ctx.rhsAlign = -1;
                ctx.eqLine = -1;
            }

            for (let i = 0; i < state.contexts.length; ++i) {
                if (column < state.contexts[i].rhsAlign) {
                    state.contexts[i].rhsAlign = -1;
                    state.contexts[i].eqLine = -1;
                }
                if (column < state.contexts[i].guardCol) {
                    state.contexts[i].guardCol = -1;
                    state.contexts[i].guardAlign = -1;
                }
            }
        }

        // Create any new implicit contexts called for by layout rules.
        if (state.lastTokens.length === 0) {
            if (token !== 'module' && token !== '{') {
                state.contexts.push({
                    value: 'where', // There's an implied "module Main where"
                    column: column,
                    ln: state.line,
                    ch: column,
                    guardCol: -1,
                    guardAlign: -1,
                    rhsAlign: -1,
                    eqLine: -1
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
                    column: column,
                    ln: state.line,
                    ch: column,
                    guardCol: -1,
                    guardAlign: -1,
                    rhsAlign: -1,
                    eqLine: -1
                });
            }
        }

        // Update lastTokens so that layout rules can be applied next time.
        state.lastTokens = state.lastTokens.slice(-1);
        state.lastTokens.push(token);

        const topContext = () => state.contexts[state.contexts.length - 1];

        // Close contexts when syntax demands that we do so.
        if (token === ',') {
            // Close implicit contexts on comma.
            while (state.contexts.length) {
                const topContext = state.contexts.pop();
                if (!state.contexts.length || isBracket(topContext)) {
                    if (topContext.hasOwnProperty('argIndex')) topContext.argIndex++;
                    state.contexts.push(topContext);
                    break;
                }
            }
        } else if (RE_CLOSEBRACKET.test(token) || (!foundLet && token === 'in')) {
            // Close contexts inside brackets when the brackets are closed.
            // Note that let/in counts as a bracket pair for this purposes.
            state.contexts.reverse();
            const reverseIndex = state.contexts.findIndex(ctx => ctx.value === opening(token));
            state.contexts.reverse();
            if (reverseIndex >= 0) {
                const index = state.contexts.length - reverseIndex - 1;
                while (state.contexts.length > index) {
                    const child = topContext();
                    state.contexts.pop();
                    const parent = topContext();
                    if (isBracket(parent)) {
                        if (parent.fresh) {
                            parent.fresh = false;
                            parent.column = Math.min(child.ch, child.column);
                        } else {
                            parent.column = Math.min(parent.column, child.ch, child.column);
                        }
                    }
                }
                if (token !== 'in') {
                    // Update the style to indicate bracket level.
                    const level = state.contexts.filter(isBracket).length;
                    if (level <= 6) style = `${style}-${level}`;
                }
            }
        } else if (token === 'where') {
            while (state.contexts.length > 0 && ['let', 'of', 'do', 'case'].indexOf(topContext().value) >= 0) {
                state.contexts.pop();
            }
        }

        // Decide if this is a new layout statement, whether it's in a new layout context
        // or an existing one.  If so, then any pre-existing brackets should be closed.
        const layoutCtx = state.contexts.findIndex(ctx => ctx.column === column);
        const isLayout = layoutCtx >= 0 && !isBracket(state.contexts[layoutCtx]);
        if (isLayout) {
            state.contexts = state.contexts.slice(0, layoutCtx + 1);
            state.contexts[state.contexts.length - 1].guardCol = -1;
            state.contexts[state.contexts.length - 1].guardAlign = -1;
            state.contexts[state.contexts.length - 1].rhsAlign = -1;
            state.contexts[state.contexts.length - 1].eqLine = -1;
        }

        // Open new contexts for brackets.  These should be inside the
        // implicit contexts created by layout.
        if (RE_OPENBRACKET.test(token)) {
            const level = state.contexts.filter(isBracket).length;
            if (level <= 6) style = `${style}-${level}`;

            let functionName = null;
            if (token === '(' && state.lastTokens.length > 1) {
                const nextToLast = state.lastTokens[state.lastTokens.length - 2];
                if (RE_VARID.test(nextToLast) || RE_CONID.test(nextToLast)) {
                    functionName = nextToLast;
                }
            }

            let newColumn = 0;
            if (state.contexts.length > 0) {
                const parent = state.contexts[state.contexts.length - 1];
                if (parent.rhsAlign >= 0 && state.line !== parent.eqLine) newColumn = parent.rhsAlign;
                else newColumn = parent.column + config.indentUnit;
            }
            state.contexts.push({
                value: token,
                column: newColumn,
                ln: state.line,
                ch: column,
                functionName,
                argIndex: 0,
                fresh: true,
                guardCol: -1,
                guardAlign: -1,
                rhsAlign: -1,
                eqLine: -1,
            });
        }

        return isLayout ? `${style} layout` : style;
    }

    return {
        startState: () => {
            return {
                func: normal,
                commentLevel: 0,
                continued: false,
                contexts: [],
                lastTokens: [],
                line: 0
            };
        },
        copyState: state => {
            return {
                func: state.func,
                commentLevel: state.commentLevel,
                continued: state.continued,
                contexts: state.contexts.map(ctx => {
                    return {
                        value: ctx.value,
                        column: ctx.column,
                        ln: ctx.ln,
                        ch: ctx.ch,
                        fresh: ctx.fresh,
                        guardCol: ctx.guardCol,
                        guardAlign: ctx.guardAlign,
                        rhsAlign: ctx.rhsAlign,
                        eqLine: ctx.eqLine,
                        functionName: ctx.functionName,
                        argIndex: ctx.argIndex || 0
                    };
                }),
                lastTokens: state.lastTokens.map(t => t),
                line: state.line
            };
        },
        token: (stream, state) => {
            const column = stream.column();
            let style = parseToken(stream, state);

            // Ignore whitespaces and comments for layout purposes.
            if (style !== null && style !== 'comment' && style !== 'meta') {
                style = updateLayout(stream.current(), column, style, state);
            }

            if (stream.eol()) state.line++;
            return style;
        },
        blankLine: state => {
            state.line++;
        },
        indent: (state, textAfter) => {
            if (state.commentLevel > 0) return CodeMirror.Pass;
            if (state.contexts.length < 1) return 0;

            // Find the top context.  If the next token closes a layout context, then this
            // is the context above the one that's closed. Otherwise, it's the top of the stack.
            let topLayout = state.contexts.length - 1;
            const token = textAfter.match(/^(in\b|where\b|[,)\]}])/);

            if (token && token[0] === ',') {
                // By rule 5, commas close all non-bracket contexts unless they occur in a guard.
                while (topLayout > 0 &&
                    !isBracket(state.contexts[topLayout]) &&
                    (state.contexts[topLayout].guardCol === -1 ||
                        state.contexts[topLayout].eqLine !== -1)) {
                    --topLayout;
                }
            } else if (token && token[0] === 'where') {
                // 'where' cannot occur in expression context, so closes a lot.
                while (topLayout > 0 && ['let', 'of', 'do', 'case'].indexOf(state.contexts[topLayout].value) >= 0) {
                    --topLayout;
                }
            } else if (token && state.contexts.findIndex(ctx => ctx.value === opening(token[0])) >= 1) {
                // Brackets close up to and including the top layout that matches.
                while (state.contexts[topLayout].value !== opening(token[0])) {
                    --topLayout;
                }
                if (!isBracket(state.contexts[topLayout])) --topLayout;
            }

            let ctx = state.contexts[topLayout];

            // Compute the rightmost surrounding layout column, which should be respected
            // by indent rules.
            let minIndent;
            if (topLayout > 0) {
                const parent = state.contexts[topLayout - 1];
                minIndent = isBracket(parent) ? parent.column : parent.column + 1;
            } else {
                minIndent = 0;
            }

            let continued = state.continued;
            if (isBracket(ctx) && token && opening(token[0]) === ctx.value) {
                // Close brackets should be aligned to the open bracket if the inside indent
                // is always more than that.  Otherwise, they are indented like a continued
                // line in the parent context.

                if (ctx.column > ctx.ch) return Math.max(minIndent, ctx.ch);
                if (topLayout === 0) return 0;

                ctx = state.contexts[topLayout - 1];
                continued = true;
            } else if (isBracket(ctx) && token && token[0] === ',') {
                // In order to align elements, commas should be placed two columns to the left
                // of the bracket's internal alignment, if possible.

                let gap;
                if (textAfter === ',') gap = 2;
                else gap = /^,\s*/.exec(textAfter)[0].length;

                return Math.max(minIndent, ctx.column - gap);
            } else if (/^(where\b|[|]($|[^:!#$%&*+./<=>?@\\^|~-]+))/.test(textAfter)) {
                if (textAfter.startsWith('|') && ctx.guardCol >= 0) {
                    // Guards should be aligned if there's an alignment set.

                    return Math.max(minIndent, ctx.guardCol);
                } else {
                    // Guards and where clauses are indented a half-indent beyond the parent
                    // context.  This is reasonably common, and helps them stand out from wrapped
                    // expressions.

                    return ctx.column + Math.ceil(config.indentUnit / 2);
                }
            } else if (RE_DASHES.exec(textAfter) && RE_DASHES.exec(textAfter).index === 0) {
                // Comments are aligned at the current level.  Special case to avoid
                // mistaking them for operators.

                return ctx.column;
            }

            const mustContinue =
                (RE_ELECTRIC_START.test(textAfter) && !RE_NEGATIVE_NUM.test(textAfter)) ||
                (state.lastTokens.slice(-1).join('') === ',' && ctx.guardAlign >= 0);
            if (continued || mustContinue) {
                // This is a continuation line, because either the end of the last line or
                // the beginning of this one are not suitable for this to be a new statement.

                if (ctx.rhsAlign >= 0) return ctx.rhsAlign;
                else if (ctx.guardAlign >= 0) return ctx.guardAlign;
                else return ctx.column + config.indentUnit;
            } else {
                return ctx.column;
            }
        },
        electricInput: RE_ELECTRIC_INPUT,
        blockCommentStart: '{-',
        blockCommentEnd: '-}',
        lineComment: '--'
    };
});
