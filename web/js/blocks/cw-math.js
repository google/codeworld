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

goog.provide('Blockly.Blocks.cwMath');

goog.require('Blockly.Blocks');

// Stefan
Blockly.Blocks['numNumber'] = {
    /**
     * Block for numeric value.
     * @this Blockly.Block
     */
    init() {
        this.setHelpUrl(Blockly.Msg.MATH_NUMBER_HELPURL);
        this.setColour(210);
        let field = new Blockly.FieldNumber('0');
        field.setValidator(Blockly.FieldNumber.prototype.basicNumberValidator);
        this.appendDummyInput()
            .appendField(field, 'NUMBER');
        this.setOutput(true);
        this.setTooltip(Blockly.Msg.MATH_NUMBER_TOOLTIP);
        this.setAsLiteral('Number');
    }
};

// TODO, moves these to Haskell
Blockly.Blocks['numNumberPerc'] = {
    /**
     * Block for numeric value.
     * @this Blockly.Block
     */
    init() {
        this.setHelpUrl(Blockly.Msg.MATH_NUMBER_HELPURL);
        this.setColour(210);
        let field = new Blockly.FieldNumber('0');
        field.setValidator(Blockly.FieldNumber.prototype.basicNumberValidator);
        this.appendDummyInput()
            .appendField(field, 'NUMBER')
            .appendField(new Blockly.FieldLabel('%', 'blocklyTextEmph'));
        this.setOutput(true, 'Number');
        // Sorin
        this.setAsLiteral("Number");
        this.setTooltip(Blockly.Msg.MATH_NUMBER_TOOLTIP);
    }
};
