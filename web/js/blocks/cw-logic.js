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

goog.provide('Blockly.Blocks.cwLogic');
goog.require('Blockly.Blocks');

let colorPoly = 180;

Blockly.Blocks['conIf'] = {
    init() {
        this.setColour(colorPoly);
        this.appendValueInput('IF')
            .appendField('if');
        this.appendValueInput('THEN')
            .appendField('then');
        this.appendValueInput('ELSE')
            .appendField('else');
        this.setInputsInline(true);
        this.setOutput(true);
        Blockly.TypeInf.defineFunction("if", Type.fromList([Type.Lit("Truth"), Type.Var("a"), Type.Var("a"), Type.Var("a")]));
        this.setAsFunction("if");
    }
};