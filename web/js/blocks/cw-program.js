/*
 * Copyright 2016 The CodeWorld Authors. All rights reserved.
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


Blockly.Blocks['cwSimulationOf'] = {
  /**
   * Block for comparison operator.
   * @this Blockly.Block
   */
  init: function() {
    this.setColour(0);
    this.setOutput(false);
    this.appendDummyInput()
        .appendField(new Blockly.FieldImage('ims/car-wash.svg',20,20))
        .appendField(new Blockly.FieldLabel('simulationOf', 'blocklyTextEmph'));
    
    var world = Blockly.TypeVar.getUnusedTypeVar();
    var number = new Blockly.TypeExpr('Number');
    var listNum = new Blockly.TypeExpr('list', [new Blockly.TypeExpr('Number')]);
    this.appendValueInput('INITIAL')
        .setTypeExpr(new Blockly.TypeExpr('Function_', 
              [new Blockly.TypeExpr('list', [new Blockly.TypeExpr('Number') ] ), world ]  ));

    this.appendValueInput('STEP')
        .setTypeExpr(new Blockly.TypeExpr('Function_', 
              [world, new Blockly.TypeExpr('Number'), world ]  ));
    this.appendValueInput('DRAW')
        .setTypeExpr(new Blockly.TypeExpr('Function_', 
              [world, new Blockly.TypeExpr('Picture') ]  ));
    this.setInputsInline(true);
    this.functionName = "";
  }
};

Blockly.Blocks['cwInteractionOf'] = {
  /**
   * Block for comparison operator.
   * @this Blockly.Block
   */
  init: function() {
    this.setColour(0);
    this.setOutput(false);
    this.appendDummyInput()
        .appendField(new Blockly.FieldImage('ims/human-handsup.svg',20,20))
        .appendField(new Blockly.FieldLabel('interactionOf', 'blocklyTextEmph'));
    
    var world = Blockly.TypeVar.getUnusedTypeVar();
    var number = new Blockly.TypeExpr('Number');
    var listNum = new Blockly.TypeExpr('list', [new Blockly.TypeExpr('Number')]);
    this.appendValueInput('INITIAL')
        .setTypeExpr(new Blockly.TypeExpr('Function_', 
              [new Blockly.TypeExpr('list', [new Blockly.TypeExpr('Number') ] ), world ]  ));

    this.appendValueInput('STEP')
        .setTypeExpr(new Blockly.TypeExpr('Function_', 
              [world, new Blockly.TypeExpr('Number'), world ]  ));

    this.appendValueInput('EVENT')
        .setTypeExpr(new Blockly.TypeExpr('Function_', 
              [world, new Blockly.TypeExpr('Event'), world ]  ));

    this.appendValueInput('DRAW')
        .setTypeExpr(new Blockly.TypeExpr('Function_', 
              [world, new Blockly.TypeExpr('Picture') ]  ));
    this.setInputsInline(true);
    this.functionName = "";
  }
};




Blockly.Blocks['cwAnimationOf'] = {
  /**
   * Block for comparison operator.
   * @this Blockly.Block
   */
  init: function() {
    this.setColour(0);
    this.setOutput(false);

    this.appendDummyInput()
        .appendField(new Blockly.FieldImage('ims/car.svg',20,20))
        .appendField(new Blockly.FieldLabel('animationOf', 'blocklyTextEmph'));
    this.appendValueInput('FUNC')
        .setTypeExpr(new Blockly.TypeExpr('Function_', 
              [new Blockly.TypeExpr('Number'), new Blockly.TypeExpr('Picture') ]  ));
    this.setInputsInline(true);
    this.functionName = "";
  }
};


