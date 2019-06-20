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

goog.provide('Blockly.Blocks.cwProgram');

goog.require('Blockly.Blocks');

Blockly.Blocks['cwActivityOf'] = {
   /**
    * Block for comparison operator.
    * @this Blockly.Block
    */
   init() {
      this.setColour(0);
      this.setOutput(false);
      this.appendValueInput('INITIAL')
          .appendField(new Blockly.FieldLabel('activityOf', 'blocklyTextEmph'))
          .appendField('(');
      this.appendValueInput('EVENT')
          .setAlign(Blockly.ALIGN_RIGHT)
          .appendField(',');
      this.appendValueInput('DRAW')
          .setAlign(Blockly.ALIGN_RIGHT)
          .appendField(',');
      this.appendDummyInput()
          .setAlign(Blockly.ALIGN_RIGHT)
          .appendField(')');
      this.setInputsInline(true);

      const worldTp = Type.Var('a');
      const numTp = Type.Lit('Number');
      const initTp = Type.fromList([Type.Lit('list', [numTp]), worldTp]);
      const eventTp = Type.fromList([worldTp, Type.Lit('Event'), worldTp]);
      const drawTp = Type.fromList([worldTp, Type.Lit('Picture')]);

      Blockly.TypeInf.defineFunction('activityOf', Type.fromList([
         initTp, eventTp, drawTp, Type.Lit('Program')
      ]));
      this.setAsFunction('activityOf');
   }
};

Blockly.Blocks['cwSimulationOf'] = {
    /**
     * Block for comparison operator.
     * @this Blockly.Block
     */
    init() {
        this.setColour(0);
        this.setOutput(false);
        this.appendValueInput('INITIAL')
            .appendField(new Blockly.FieldImage('ims/car-wash.svg', 20, 20))
            .appendField(new Blockly.FieldLabel('simulationOf',
                'blocklyTextEmph'))
            .appendField('(');
        this.appendValueInput('STEP')
            .setAlign(Blockly.ALIGN_RIGHT)
            .appendField(',');
        this.appendValueInput('DRAW')
            .setAlign(Blockly.ALIGN_RIGHT)
            .appendField(',');
        this.appendDummyInput()
            .setAlign(Blockly.ALIGN_RIGHT)
            .appendField(')');
        this.setInputsInline(true);

        const worldTp = Type.Var('a');
        const numTp = Type.Lit('Number');
        const initTp = Type.fromList([Type.Lit('list', [numTp]), worldTp]);
        const stepTp = Type.fromList([worldTp, numTp, worldTp]);
        const drawTp = Type.fromList([worldTp, Type.Lit('Picture')]);

        Blockly.TypeInf.defineFunction('simulationOf', Type.fromList([
            initTp, stepTp, drawTp, Type.Lit('Program')
        ]));
        this.setAsFunction('simulationOf');
    }
};

Blockly.Blocks['cwInteractionOf'] = {
    /**
     * Block for comparison operator.
     * @this Blockly.Block
     */
    init() {
        this.setColour(0);
        this.setOutput(false);
        this.appendValueInput('INITIAL')
            .appendField(new Blockly.FieldImage('ims/human-handsup.svg', 20,
                20))
            .appendField(new Blockly.FieldLabel('interactionOf',
                'blocklyTextEmph'))
            .appendField('(');
        this.appendValueInput('STEP')
            .setAlign(Blockly.ALIGN_RIGHT)
            .appendField(',');
        this.appendValueInput('EVENT')
            .setAlign(Blockly.ALIGN_RIGHT)
            .appendField(',');
        this.appendValueInput('DRAW')
            .setAlign(Blockly.ALIGN_RIGHT)
            .appendField(',');
        this.appendDummyInput()
            .setAlign(Blockly.ALIGN_RIGHT)
            .appendField(')');
        this.setInputsInline(true);

        const worldTp = Type.Var('a');
        const numTp = Type.Lit('Number');
        const initTp = Type.fromList([Type.Lit('list', [numTp]), worldTp]);
        const stepTp = Type.fromList([worldTp, numTp, worldTp]);
        const eventTp = Type.fromList([worldTp, Type.Lit('Event'), worldTp]);
        const drawTp = Type.fromList([worldTp, Type.Lit('Picture')]);

        Blockly.TypeInf.defineFunction('interactionOf', Type.fromList([
            initTp, stepTp, eventTp, drawTp, Type.Lit('Program')
        ]));
        this.setAsFunction('interactionOf');
    }
};

Blockly.Blocks['cwAnimationOf'] = {
    /**
     * Block for comparison operator.
     * @this Blockly.Block
     */
    init() {
        this.setColour(0);
        this.setOutput(false);
        this.appendValueInput('FUNC')
            .appendField(new Blockly.FieldImage('ims/car.svg', 20, 20))
            .appendField(new Blockly.FieldLabel('animationOf',
                'blocklyTextEmph'))
            .appendField('(');
        this.appendDummyInput()
            .setAlign(Blockly.ALIGN_RIGHT)
            .appendField(')');
        this.setInputsInline(true);

        const stepTp = Type.Func(Type.Lit('Number'), Type.Lit('Picture'));

        Blockly.TypeInf.defineFunction('animationOf', Type.Func(stepTp,
            Type.Lit('Program')));
        this.setAsFunction('animationOf');
    }
};
