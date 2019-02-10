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

goog.provide('Blockly.Blocks.cwTuples');

goog.require('Blockly.Blocks');

/**
 * Pairs
 */
Blockly.Blocks['pair_create_typed'] = {
    init() {
        this.setColour(210);
        this.appendValueInput('FIRST')
            .appendField(new Blockly.FieldLabel('(', 'blocklyTextEmph'));
        this.appendValueInput('SECOND')
            .setAlign(Blockly.ALIGN_RIGHT)
            .appendField(new Blockly.FieldLabel(',', 'blocklyTextEmph'))
        this.appendDummyInput()
            .setAlign(Blockly.ALIGN_RIGHT)
            .appendField(new Blockly.FieldLabel(')', 'blocklyTextEmph'));
        this.setOutput(true);
        this.setInputsInline(true);

        let a = Type.Var("a");
        let b = Type.Var("b");
        let res = Type.Lit("pair", [a, b]);
        Blockly.TypeInf.defineFunction(",", Type.fromList([a, b, res]));
        this.setAsFunction(",");
    }
};

Blockly.Blocks['pair_first_typed'] = {
    init() {
        this.setColour(180);
        this.appendValueInput('PAIR')
            .appendField(new Blockly.FieldLabel("firstOfPair", "blocklyTextEmph"))
            .appendField("(");
        this.appendDummyInput()
            .setAlign(Blockly.ALIGN_RIGHT)
            .appendField(")");
        this.setOutput(true);

        Blockly.TypeInf.defineFunction("firstOfPair", Type.fromList([Type.Lit("pair", [Type.Var("a"), Type.Var("b")]), Type.Var("a")]));
        this.setAsFunction("firstOfPair");
    }
};

Blockly.Blocks['pair_second_typed'] = {
    init() {
        this.setColour(180);
        this.appendValueInput('PAIR')
            .appendField(new Blockly.FieldLabel("secondOfPair", "blocklyTextEmph"))
            .appendField("(");
        this.appendDummyInput()
            .setAlign(Blockly.ALIGN_RIGHT)
            .appendField(")");
        this.setOutput(true);

        Blockly.TypeInf.defineFunction("secondOfPair", Type.fromList([Type.Lit("pair", [Type.Var("a"), Type.Var("b")]), Type.Var("b")]));
        this.setAsFunction("secondOfPair");
    }
};