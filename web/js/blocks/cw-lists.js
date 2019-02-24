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

goog.provide('Blockly.Blocks.cwLists');

goog.require('Blockly.Blocks');

const listsHUE = 260;

Blockly.Blocks['lists_comprehension'] = {
    /**
     * Block for creating a list with any number of elements of any type.
     * @this Blockly.Block
     */
    init() {
        this.setColour(listsHUE);
        this.vars_ = ['i', 'j', 'k'];
        this.varTypes_ = [Type.generateTypeVar('lc'), Type.generateTypeVar(
            'lc'), Type.generateTypeVar('lc')];
        this.appendValueInput('DO')
            .appendField(new Blockly.FieldLabel('List Comprehension',
                'blocklyTextEmph'));
        this.appendValueInput('VAR0')
            .setAlign(Blockly.ALIGN_RIGHT)
            .appendField(new Blockly.FieldLocalVar(this.vars_[0], this.getArgType(
                0)))
            .appendField('\u2190');
        this.appendValueInput('VAR1')
            .setAlign(Blockly.ALIGN_RIGHT)
            .appendField(new Blockly.FieldLocalVar(this.vars_[1], this.getArgType(
                1)))
            .appendField('\u2190');
        this.setOutput(true);
        this.setMutator(new Blockly.Mutator(['lists_comp_var',
            'lists_comp_guard'
        ]));
        this.setTooltip(Blockly.Msg.LISTS_CREATE_WITH_TOOLTIP);
        this.varCount_ = 2;
        this.guardCount_ = 0;
        this.resetArrows();
        Blockly.TypeInf.defineFunction('&&&', Type.fromList([Type.Lit(
            'Truth'), Type.Lit('Truth'), Type.Lit('Truth')]));
        Blockly.TypeInf.defineFunction('filtB', Type.fromList([Type.Var('a'),
            Type.Lit('Truth'), Type.Var('a')
        ]));
        Blockly.TypeInf.defineFunction('returnl', Type.fromList([Type.Var(
            'a'), Type.Lit('list', [Type.Var('a')])]));
        Blockly.TypeInf.defineFunction('froml', Type.fromList([Type.Lit(
            'list', [Type.Var('a')]), Type.Var('a')]));
        Blockly.TypeInf.defineFunction('bindl', Type.fromList(
            [Type.Lit('list', [Type.Var('a')]), Type.fromList([Type
                .Var('a'), Type.Lit('list', [Type.Var('b')])
            ]), Type.Lit('list', [Type.Var('b')])]));

    },

    foldr1(fn, xs) {
        let result = xs[xs.length - 1];
        for (let i = xs.length - 2; i > -1; i--) {
            result = fn(xs[i], result);
        }
        return result;
    },

    getExpr() {

        // Do main exp
        let mainExp = Exp.Var('undef');
        if (this.getInput('DO').connection.isConnected()) {
            mainExp = this.getInput('DO').connection.targetBlock().getExpr();
        }

        mainExp.tag = this.getInput('DO').connection;
        mainExp = Exp.App(Exp.Var('returnl'), mainExp);

        // Do Guards
        const guardExps = [];
        for (let i = 0; i < this.guardCount_; i++) {
            const inp = this.getInput(`GUARD${i}`);
            if (inp.connection && inp.connection.isConnected()) {
                const exp = inp.connection.targetBlock().getExpr();
                exp.tag = inp.connection;
                guardExps.push(exp);
            } else {
                const exp = Exp.Var('undef');
                exp.tag = inp.connection;
                guardExps.push(exp);
            }
        }

        const boolComb = (a, b) => Exp.AppFunc([a, b], Exp.Var('&&&'));
        let guardExp;
        if (guardExps.length == 0) {
            guardExp = Exp.Lit('Truth');
        } else if (guardExps.length == 1) {
            const inp = this.getInput('GUARD0');
            if (inp.connection.isConnected()) {
                guardExp = inp.connection.targetBlock().getExpr();
                guardExp.tag = inp.connection;
            } else {
                guardExp = Exp.Lit('Truth');
                guardExp.tag = inp.connection;
            }
        } else {
            guardExp = this.foldr1(boolComb, guardExps);
        }

        // Do variables
        let result = Exp.AppFunc([mainExp, guardExp], Exp.Var('filtB'));
        for (let i = this.varCount_ - 1; i !== -1; i--) {
            const varName = this.vars_[i];
            const inp = this.getInput(`VAR${i}`);
            let exp;
            if (inp && inp.connection.isConnected()) {
                exp = inp.connection.targetBlock().getExpr();
            } else {
                exp = Exp.Var('undef');
            }

            exp.tag = inp.connection;

            const field = inp.fieldRow[0];
            if (!field.typeExpr) {
                throw 'Wrong field !';
            }

            exp = Exp.App(Exp.Var('froml'), exp);
            exp.tag = field;
            exp = Exp.App(Exp.Var('returnl'), exp);

            const letExp = Exp.App(Exp.Var('bindl'), exp);

            result = Exp.App(letExp, Exp.Abs(varName, result));
        }

        result.tag = this.outputConnection;
        return result;
    },

    foldr(fn, ult, xs) {
        let result = ult;
        for (let i = xs.length - 1; i !== -1; i--) {
            result = fn(xs[i], result);
        }
        return result;
    },

    resetArrows() {
        this.arrows = null;
        const tps = [];
        this.varTypes_ = [];

        const a = Type.generateTypeVar('a');

        tps.push(a);
        for (let i = 0; i < this.varCount_; i++) {
            const varTp = Type.generateTypeVar('lc');
            this.varTypes_.push(varTp);
            const t = Type.Lit('list', [varTp]);
            tps.push(t);
        }
        for (let i = 0; i < this.guardCount_; i++) {
            tps.push(Type.Lit('Truth'));
        }

        tps.push(Type.Lit('list', [a]));
        this.arrows = Type.fromList(tps);
        this.initArrows(false);
    },

    assignVars() {
        let i = 0;
        const thisBlock = this;
        this.inputList.forEach(inp => {
            if (inp.name.startsWith('VAR')) {
                for (let f = 0; f < inp.fieldRow.length; f++) {
                    const fieldvar = inp.fieldRow[f];
                    if (fieldvar instanceof Blockly.FieldLocalVar) {
                        const tp = thisBlock.varTypes_[i++];
                        fieldvar.typeExpr = tp;
                        break;
                    }
                }
            }
        });
    },

    getArgType(localId) {
        return this.varTypes_[localId];
    },

    getVars(connection) {
        let i = 0;
        let available = [];
        for (i = 0; i < this.varCount_; i++) {
            if (this.getInput('DO').connection == connection) {
                return this.vars_;
            }

            if (this.getInput(`VAR${i}`) && this.getInput(`VAR${i}`).connection ==
                connection) {
                return available;
            }

            if (this.getInput(`GUARD${i}`) && this.getInput(`GUARD${i}`).connection ==
                connection) {
                return this.vars_;
            }

            available = available.concat(this.vars_[i]);

        }
        return [];
    },
    /**
     * Create XML to represent list inputs.
     * @return {Element} XML storage element.
     * @this Blockly.Block
     */
    mutationToDom() {
        const container = document.createElement('mutation');
        container.setAttribute('guardcount', this.guardCount_);
        container.setAttribute('varcount', this.varCount_);

        for (let i = 0; i < this.varCount_; i++) {
            const parameter = document.createElement('let');
            parameter.setAttribute('name', this.vars_[i]);

            container.appendChild(parameter);
        }

        return container;
    },
    /**
     * Parse XML to restore the list inputs.
     * @param {!Element} xmlElement XML storage element.
     * @this Blockly.Block
     */
    domToMutation(xmlElement) {
        for (let x = 0; x < this.varCount_; x++) {
            this.removeInput(`VAR${x}`);
        }
        for (let x = 0; x < this.guardCount_; x++) {
            this.removeInput(`GUARD${x}`);
        }
        this.vars_ = [];
        this.varTypes_ = [];

        this.varCount_ = parseInt(xmlElement.getAttribute('varcount'), 10);
        this.guardCount_ = parseInt(xmlElement.getAttribute('guardcount'),
            10);

        for (let i = 0, childNode; childNode = xmlElement.childNodes[i]; i++) {
            if (childNode.nodeName.toLowerCase() == 'let') {
                const name = childNode.getAttribute('name');

                this.appendValueInput(`VAR${i}`)
                    .setAlign(Blockly.ALIGN_RIGHT)
                    .appendField(new Blockly.FieldLocalVar(name, this.getArgType(
                        i)))
                    .appendField('\u2190');
                this.vars_.push(name);
            }
        }

        for (let x = 0; x < this.guardCount_; x++) {
            this.appendValueInput(`GUARD${x}`)
                .setAlign(Blockly.ALIGN_RIGHT)
                .appendField('If');
        }
        this.resetArrows();
    },

    /**
     * Populate the mutator's dialog with this block's components.
     * @param {!Blockly.Workspace} workspace Mutator's workspace.
     * @return {!Blockly.Block} Root block in mutator.
     * @this Blockly.Block
     */
    decompose(workspace) {
        const containerBlock =
            workspace.newBlock('lists_create_with_container');
        containerBlock.initSvg();
        let connection = containerBlock.getInput('STACK').connection;

        for (let x = 0; x < this.varCount_; x++) {
            const itemBlock = workspace.newBlock('lists_comp_var');
            itemBlock.setFieldValue(this.vars_[x], 'NAME');
            itemBlock.initSvg();
            connection.connect(itemBlock.previousConnection);
            connection = itemBlock.nextConnection;
        }
        for (let x = 0; x < this.guardCount_; x++) {
            const itemBlock = workspace.newBlock('lists_comp_guard');
            itemBlock.initSvg();
            connection.connect(itemBlock.previousConnection);
            connection = itemBlock.nextConnection;
        }

        return containerBlock;
    },

    /**
     * Reconfigure this block based on the mutator dialog's components.
     * @param {!Blockly.Block} containerBlock Root block in mutator.
     * @this Blockly.Block
     */
    compose(containerBlock) {
        // Disconnect all input blocks and remove all inputs.
        for (let x = this.varCount_ - 1; x >= 0; x--) {
            this.removeInput(`VAR${x}`);
        }
        for (let x = this.guardCount_ - 1; x >= 0; x--) {
            this.removeInput(`GUARD${x}`);
        }
        this.vars_ = [];

        this.varCount_ = 0;
        this.guardCount_ = 0;
        // Rebuild the block's inputs.
        let itemBlock = containerBlock.getInputTargetBlock('STACK');
        while (itemBlock) {
            if (itemBlock.type == 'lists_comp_var') {
                const name = itemBlock.getFieldValue('NAME');
                this.vars_[this.varCount_] = name;
                const input = this.appendValueInput(`VAR${this.varCount_}`)
                    .setAlign(Blockly.ALIGN_RIGHT)
                    .appendField(new Blockly.FieldLocalVar(name, this.getArgType(
                        this.varCount_)))
                    .appendField('\u2190');

                this.vars_.push(name);
                // Reconnect any child blocks.
                if (itemBlock.valueConnection_) {
                    input.connection.connect(itemBlock.valueConnection_);
                }
                this.varCount_++;
            } else if (itemBlock.type == 'lists_comp_guard') {
                const input = this.appendValueInput(`GUARD${this.guardCount_}`)
                    .setAlign(Blockly.ALIGN_RIGHT)
                    .appendField('If');
                if (itemBlock.valueConnection_) {
                    input.connection.connect(itemBlock.valueConnection_);
                }
                this.guardCount_++;
            }

            itemBlock = itemBlock.nextConnection &&
                itemBlock.nextConnection.targetBlock();
        }
        this.renderMoveConnections_();
        this.resetArrows();

        Blockly.TypeInf.inferWorkspace(this.workspace);
    },

    /**
     * Store pointers to any connected child blocks.
     * @param {!Blockly.Block} containerBlock Root block in mutator.
     * @this Blockly.Block
     */
    saveConnections(containerBlock) {
        let itemBlock = containerBlock.getInputTargetBlock('STACK');
        let x = 0;
        while (itemBlock) {
            const input = this.getInput(`VAR${x}`);
            itemBlock.valueConnection_ = input && input.connection.targetConnection;
            x++;
            itemBlock = itemBlock.nextConnection &&
                itemBlock.nextConnection.targetBlock();
        }
    }
};

Blockly.Blocks['lists_comp_var'] = {
    /**
     * Mutator block for procedure argument.
     * @this Blockly.Block
     */
    init() {
        this.appendDummyInput()
            .appendField(new Blockly.FieldTextInput('k'), 'NAME')
            .appendField('\u2190');
        this.setPreviousStatement(true);
        this.setNextStatement(true);
        this.setColour(listsHUE);
        this.setTooltip('Assign a binding to a list');
        this.contextMenu = false;
    }
};

Blockly.Blocks['lists_comp_guard'] = {
    /**
     * Mutator block for adding items.
     * @this Blockly.Block
     */
    init() {
        this.setColour(listsHUE);
        this.appendDummyInput()
            .appendField('Guard');
        this.setPreviousStatement(true);
        this.setNextStatement(true);
        this.setTooltip('Guard against a boolean expression');
        this.contextMenu = false;
    }
};

Blockly.Blocks['lists_numgen'] = {
    init() {
        this.appendValueInput('LEFT')
            .appendField('[');
        this.appendValueInput('RIGHT')
            .setAlign(Blockly.ALIGN_RIGHT)
            .appendField('...');
        this.appendDummyInput()
            .setAlign(Blockly.ALIGN_RIGHT)
            .appendField(']');
        this.setInputsInline(true);
        this.setOutput(true);

        Blockly.TypeInf.defineFunction('[..]', Type.fromList([Type.Lit(
            'Number'), Type.Lit('Number'), Type.Lit('list',
            [Type.Lit('Number')])]));
        this.setAsFunction('[..]');

        this.setColour(listsHUE);
        this.setTooltip(
            'Generates a list of numbers between the first and second inputs'
        );
    }
};

Blockly.Blocks['lists_numgenstep'] = {
    init() {
        this.appendValueInput('LEFT')
            .appendField('[');
        this.appendValueInput('NEXT')
            .setAlign(Blockly.ALIGN_RIGHT)
            .appendField(',');
        this.appendValueInput('RIGHT')
            .setAlign(Blockly.ALIGN_RIGHT)
            .appendField('...');
        this.appendDummyInput()
            .setAlign(Blockly.ALIGN_RIGHT)
            .appendField(']');
        this.setInputsInline(true);
        this.setOutput(true);

        Blockly.TypeInf.defineFunction('[,..]', Type.fromList([Type.Lit(
            'Number'), Type.Lit('Number'), Type.Lit(
            'Number'), Type.Lit('list', [Type.Lit('Number')])]));
        this.setAsFunction('[,..]');

        this.setColour(listsHUE);
        this.setTooltip(
            'Generates a list of numbers between the first and second inputs, with a step'
        );
    }
};

Blockly.Blocks['lists_length'] = {
    init() {
        this.setColour(210);
        this.appendValueInput('LST')
            .appendField(new Blockly.FieldLabel('length', 'blocklyTextEmph'))
            .appendField('(');
        this.appendDummyInput()
            .setAlign(Blockly.ALIGN_RIGHT)
            .appendField(')');
        this.setOutput(true);
        Blockly.TypeInf.defineFunction('length', Type.fromList([Type.Lit(
            'list', [Type.Var('a')]), Type.Lit('Number')]));
        this.setAsFunction('length');
    }
};

Blockly.Blocks['lists_repeating'] = {
    init() {
        this.setColour(listsHUE);
        this.appendValueInput('LST')
            .appendField(new Blockly.FieldLabel('repeating',
                'blocklyTextEmph'))
            .appendField('(');
        this.appendDummyInput()
            .setAlign(Blockly.ALIGN_RIGHT)
            .appendField(')');
        this.setOutput(true);
        const lstType = Type.Lit('list', [Type.Var('a')]);
        Blockly.TypeInf.defineFunction('repeating', Type.fromList([lstType,
            lstType
        ]));
        this.setAsFunction('repeating');
    }
};

Blockly.Blocks['lists_shuffled'] = {
    init() {
        this.setColour(listsHUE);
        this.appendValueInput('LST')
            .appendField(new Blockly.FieldLabel('shuffled',
                'blocklyTextEmph'))
            .appendField('(');
        this.appendValueInput('SEED')
            .setAlign(Blockly.ALIGN_RIGHT)
            .appendField(',');
        this.appendDummyInput()
            .setAlign(Blockly.ALIGN_RIGHT)
            .appendField(')');
        this.setOutput(true);
        const lstType = Type.Lit('list', [Type.Var('a')]);
        Blockly.TypeInf.defineFunction('shuffled', Type.fromList([lstType,
            Type.Lit('Number'), lstType
        ]));
        this.setAsFunction('shuffled');
    }
};

Blockly.Blocks['lists_sorted'] = {
    init() {
        this.setColour(listsHUE);
        this.appendValueInput('LST')
            .appendField(new Blockly.FieldLabel('sorted', 'blocklyTextEmph'))
            .appendField('(');
        this.appendDummyInput()
            .setAlign(Blockly.ALIGN_RIGHT)
            .appendField(')');
        this.setOutput(true);
        const lstType = Type.Lit('list', [Type.Lit('Number')]);
        Blockly.TypeInf.defineFunction('sorted', Type.fromList([lstType,
            lstType
        ]));
        this.setAsFunction('sorted');
    }
};

Blockly.Blocks['lists_reversed'] = {
    init() {
        this.setColour(listsHUE);
        this.appendValueInput('LST')
            .appendField(new Blockly.FieldLabel('reversed',
                'blocklyTextEmph'))
            .appendField('(');
        this.appendDummyInput()
            .setAlign(Blockly.ALIGN_RIGHT)
            .appendField(')');
        this.setOutput(true);
        const lstType = Type.Lit('list', [Type.Var('a')]);
        Blockly.TypeInf.defineFunction('reversed', Type.fromList([lstType,
            lstType
        ]));
        this.setAsFunction('reversed');
    }
};

Blockly.Blocks['lists_first'] = {
    init() {
        this.setColour(listsHUE);
        this.appendValueInput('LST')
            .appendField(new Blockly.FieldLabel('first', 'blocklyTextEmph'))
            .appendField('(');
        this.appendValueInput('COUNT')
            .setAlign(Blockly.ALIGN_RIGHT)
            .appendField(',');
        this.appendDummyInput()
            .setAlign(Blockly.ALIGN_RIGHT)
            .appendField(')');
        this.setOutput(true);
        const lstType = Type.Lit('list', [Type.Var('a')]);
        Blockly.TypeInf.defineFunction('first', Type.fromList([lstType,
            Type.Lit('Number'), lstType
        ]));
        this.setAsFunction('first');
    }
};

Blockly.Blocks['lists_rest'] = {
    init() {
        this.setColour(listsHUE);
        this.appendValueInput('LST')
            .appendField(new Blockly.FieldLabel('rest', 'blocklyTextEmph'))
            .appendField('(');
        this.appendValueInput('COUNT')
            .setAlign(Blockly.ALIGN_RIGHT)
            .appendField(',');
        this.appendDummyInput()
            .setAlign(Blockly.ALIGN_RIGHT)
            .appendField(')');
        this.setOutput(true);
        const lstType = Type.Lit('list', [Type.Var('a')]);
        Blockly.TypeInf.defineFunction('rest', Type.fromList([lstType, Type
            .Lit('Number'), lstType
        ]));
        this.setAsFunction('rest');
    }
};

Blockly.Blocks['lists_at'] = {
    init() {
        this.setColour(180);
        this.appendValueInput('LST');
        this.appendValueInput('POS')
            .setAlign(Blockly.ALIGN_RIGHT)
            .appendField(new Blockly.FieldLabel('#', 'blocklyTextEmph'));
        this.setOutput(true);
        this.setInputsInline(true);

        Blockly.TypeInf.defineFunction('#',
            Type.fromList([Type.Lit('list', [Type.Var('a')]), Type.Lit(
                'Number'), Type.Var('a')]));
        this.setAsFunction('#');
    }
};

Blockly.Blocks['lists_cons'] = {
    init() {
        this.setColour(listsHUE);
        this.appendValueInput('ITEM');
        this.appendValueInput('LST')
            .setAlign(Blockly.ALIGN_RIGHT)
            .appendField(new Blockly.FieldLabel(':', 'blocklyTextEmph'));
        this.setOutput(true);
        this.setInputsInline(true);

        const lst = Type.Lit('list', [Type.Var('a')]);
        Blockly.TypeInf.defineFunction(':', Type.fromList([Type.Var('a'),
            lst, lst
        ]));
        this.setAsFunction(':');

    }
};

Blockly.Blocks['lists_create_with_typed'] = {
    /**
     * Block for creating a list with any number of elements of any type.
     * @this Blockly.Block
     */
    init() {
        this.setColour(260);
        this.appendValueInput('ADD0')
            .appendField(new Blockly.FieldImage(
                'ims/format-list-bulleted.svg', 20, 20))
            .appendField(new Blockly.FieldLabel('List', 'blocklyTextEmph'));
        this.appendValueInput('ADD1');
        this.appendValueInput('ADD2');
        this.setOutput(true);
        this.setMutator(new Blockly.Mutator(['lists_create_with_item']));
        this.setTooltip(Blockly.Msg.LISTS_CREATE_WITH_TOOLTIP);
        this.itemCount_ = 3;
        const tps = [];
        for (let k = 0; k < this.itemCount_; k++) {
            tps.push(Type.Var('a'));
        }
        tps.push(Type.Lit('list', [Type.Var('a')]));
        this.arrows = Type.fromList(tps);

        Blockly.TypeInf.defineFunction(':', Type.fromList([Type.Var('a'),
            Type.Lit('list', [Type.Var('a')]), Type.Lit('list',
                [Type.Var('a')])
        ]));
    },
    getType() {
        return this.outputConnection.typeExpr.children[0];
    },

    foldr(fn, ult, xs) {
        let result = ult;
        for (let i = xs.length - 1; i !== -1; i--) {
            result = fn(xs[i], result);
        }
        return result;
    },

    getExpr() {
        const exps = [];
        this.inputList.forEach(inp => {
            if (inp.connection && inp.connection.isConnected()) {
                const exp = inp.connection.targetBlock().getExpr();
                exp.tag = inp.connection;
                exps.push(exp);
            } else {
                const exp = Exp.Var('undef');
                exp.tag = inp.connection;
                exps.push(exp);
            }
        });
        const func = (a, b) => Exp.AppFunc([a, b], Exp.Var(':'));
        const e = this.foldr(func, Exp.Var('[]'), exps);
        e.tag = this.outputConnection;
        return e;
    },

    /**
     * Create XML to represent list inputs.
     * @return {Element} XML storage element.
     * @this Blockly.Block
     */
    mutationToDom() {
        const container = document.createElement('mutation');
        container.setAttribute('items', this.itemCount_);
        return container;
    },
    /**
     * Parse XML to restore the list inputs.
     * @param {!Element} xmlElement XML storage element.
     * @this Blockly.Block
     */
    domToMutation(xmlElement) {
        for (let x = 0; x < this.itemCount_; x++) {
            this.removeInput(`ADD${x}`);
        }
        this.itemCount_ = parseInt(xmlElement.getAttribute('items'), 10);
        for (let x = 0; x < this.itemCount_; x++) {
            const input = this.appendValueInput(`ADD${x}`);
            if (x == 0) {
                input.appendField(new Blockly.FieldImage(
                    'ims/format-list-bulleted.svg', 20, 20));
                input.appendField('List');
            }
        }
        if (this.itemCount_ == 0) {
            this.appendDummyInput('EMPTY')
                .appendField('[ ]');
        }
        const tps = [];
        for (let k = 0; k < this.itemCount_; k++) {
            tps.push(Type.Var('a'));
        }
        tps.push(Type.Lit('list', [Type.Var('a')]));
        this.arrows = Type.fromList(tps);
        this.initArrows();
    },
    /**
     * Populate the mutator's dialog with this block's components.
     * @param {!Blockly.Workspace} workspace Mutator's workspace.
     * @return {!Blockly.Block} Root block in mutator.
     * @this Blockly.Block
     */
    decompose(workspace) {
        const containerBlock =
            workspace.newBlock('lists_create_with_container');
        containerBlock.initSvg();
        let connection = containerBlock.getInput('STACK').connection;
        for (let x = 0; x < this.itemCount_; x++) {
            const itemBlock = workspace.newBlock('lists_create_with_item');
            itemBlock.initSvg();
            connection.connect(itemBlock.previousConnection);
            connection = itemBlock.nextConnection;
        }
        return containerBlock;
    },
    /**
     * Reconfigure this block based on the mutator dialog's components.
     * @param {!Blockly.Block} containerBlock Root block in mutator.
     * @this Blockly.Block
     */
    compose(containerBlock) {
        // Disconnect all input blocks and remove all inputs.
        if (this.itemCount_ == 0) {
            this.removeInput('EMPTY');
        } else {
            for (let x = this.itemCount_ - 1; x >= 0; x--) {
                this.removeInput(`ADD${x}`);
            }
        }
        this.itemCount_ = 0;
        // Rebuild the block's inputs.
        let itemBlock = containerBlock.getInputTargetBlock('STACK');
        while (itemBlock) {
            const input = this.appendValueInput(`ADD${this.itemCount_}`);
            if (this.itemCount_ == 0) {
                input.appendField(new Blockly.FieldImage(
                    'ims/format-list-bulleted.svg', 20, 20));
                input.appendField(new Blockly.FieldLabel('List',
                    'blocklyTextEmph'));
            }
            // Reconnect any child blocks.
            if (itemBlock.valueConnection_) {
                input.connection.connect(itemBlock.valueConnection_);
            }
            this.itemCount_++;
            itemBlock = itemBlock.nextConnection &&
                itemBlock.nextConnection.targetBlock();
        }
        if (this.itemCount_ == 0) {
            this.appendDummyInput('EMPTY')
                .appendField('[ ]');
        }

        const tps = [];
        for (let k = 0; k < this.itemCount_; k++) {
            tps.push(Type.Var('a'));
        }
        tps.push(Type.Lit('list', [Type.Var('a')]));
        this.arrows = Type.fromList(tps);
        this.initArrows();

        this.renderMoveConnections_();
    },
    /**
     * Store pointers to any connected child blocks.
     * @param {!Blockly.Block} containerBlock Root block in mutator.
     * @this Blockly.Block
     */
    saveConnections(containerBlock) {
        let itemBlock = containerBlock.getInputTargetBlock('STACK');
        let x = 0;
        while (itemBlock) {
            const input = this.getInput(`ADD${x}`);
            itemBlock.valueConnection_ = input && input.connection.targetConnection;
            x++;
            itemBlock = itemBlock.nextConnection &&
                itemBlock.nextConnection.targetBlock();
        }
    }
};
