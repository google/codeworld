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

goog.provide('Blockly.Blocks.cwText');

goog.require('Blockly.Blocks');


var textHUE = 45;

Blockly.Blocks['text_typed'] = {
  /**
   * Block for text value.
   * @this Blockly.Block
   */
  init: function() {
    this.setHelpUrl(Blockly.Msg.TEXT_TEXT_HELPURL);
    this.setColour(45);
    this.appendDummyInput()
        .appendField(this.newQuote_(true))
        .appendField(new Blockly.FieldTextInput(''), 'TEXT')
        .appendField(this.newQuote_(false));
    this.setOutput(true);
    this.setTooltip("Gives the given text");
    this.setAsLiteral("Text");
  },
  /**
   * Create an image of an open or closed quote.
   * @param {boolean} open True if open quote, false if closed.
   * @return {!Blockly.FieldImage} The field image of the quote.
   * @this Blockly.Block
   * @private
   */
  newQuote_: function(open) {
    if (open == this.RTL) {
      var file = 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAwAAAAKCAQAAAAqJXdxAAAAqUlEQVQI1z3KvUpCcRiA8ef9E4JNHhI0aFEacm1o0BsI0Slx8wa8gLauoDnoBhq7DcfWhggONDmJJgqCPA7neJ7p934EOOKOnM8Q7PDElo/4x4lFb2DmuUjcUzS3URnGib9qaPNbuXvBO3sGPHJDRG6fGVdMSeWDP2q99FQdFrz26Gu5Tq7dFMzUvbXy8KXeAj57cOklgA+u1B5AoslLtGIHQMaCVnwDnADZIFIrXsoXrgAAAABJRU5ErkJggg==';
    } else {
      var file = 'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAwAAAAKCAQAAAAqJXdxAAAAn0lEQVQI1z3OMa5BURSF4f/cQhAKjUQhuQmFNwGJEUi0RKN5rU7FHKhpjEH3TEMtkdBSCY1EIv8r7nFX9e29V7EBAOvu7RPjwmWGH/VuF8CyN9/OAdvqIXYLvtRaNjx9mMTDyo+NjAN1HNcl9ZQ5oQMM3dgDUqDo1l8DzvwmtZN7mnD+PkmLa+4mhrxVA9fRowBWmVBhFy5gYEjKMfz9AylsaRRgGzvZAAAAAElFTkSuQmCC';
    }
    return new Blockly.FieldImage(file, 12, 12, '"');
  }
};

Blockly.Blocks['txtConcat'] = {
  init: function() {
    this.appendValueInput('STR0');
    this.appendValueInput('STR1')
        .appendField(new Blockly.FieldLabel("<>","blocklyTextEmph") );
    this.setColour(textHUE);
    this.setMutator(new Blockly.Mutator(['text_combine_ele']));
    this.setTooltip('Concatenate multiple text');
    this.itemCount_ = 2;
    this.functionName = "Literal";
    this.setOutput(true);
    Blockly.TypeInf.defineFunction("<>", Type.fromList([Type.Lit("Text"),Type.Lit("Text"),Type.Lit("Text")]));
    this.setAsFunction("<>");
  },

  foldr1 : function(fn, xs) {
    var result = xs[xs.length - 1];
      for (var i = xs.length - 2; i > -1; i--) {
        result = fn(xs[i], result);
      }
    return result;
  },

  getExpr: function(){
    var exps = [];
    this.inputList.forEach(function(inp){
      if(inp.connection.isConnected())
        exps.push(inp.connection.targetBlock().getExpr());
      else
        exps.push(Exp.Var('undef'));
    });
    if(exps.length < 2){ // If the block has less than 2 inputs, warn the user
      exps = [];
      exps.push(Exp.Var('undef'));
      exps.push(Exp.Var('undef'));
    } 

    var func = (a,b) => Exp.AppFunc([a,b],Exp.Var("<>"));
    var e = this.foldr1(func,exps);
    return e;
  },

  decompose: function(workspace) {
    var containerBlock =
        workspace.newBlock('text_combine_container');
    containerBlock.initSvg();
    var connection = containerBlock.getInput('STACK').connection;

    for (var x = 0; x < this.itemCount_; x++) {
      var itemBlock = workspace.newBlock('text_combine_ele');
      itemBlock.initSvg();
      connection.connect(itemBlock.previousConnection);
      connection = itemBlock.nextConnection;
    }

    return containerBlock;
  },

  compose: function(containerBlock) {
    
    for (var x = 0; x < this.itemCount_; x++) {
      this.removeInput('STR' + x);
    }

    this.itemCount_ = 0;
    // Rebuild the block's inputs.
    var itemBlock = containerBlock.getInputTargetBlock('STACK');
    var tps = [];
    while (itemBlock) {
      var input = this.appendValueInput('STR' + this.itemCount_);
      tps.push(new Type.Lit("Text"));
                     
      if (this.itemCount_ > 0) {
        input.appendField(new Blockly.FieldLabel("<>","blocklyTextEmph") );
      }
      if (itemBlock.valueConnection_) {
        input.connection.connect(itemBlock.valueConnection_);
      }
      this.itemCount_++;
      itemBlock = itemBlock.nextConnection &&
          itemBlock.nextConnection.targetBlock();
    }
    this.renderMoveConnections_();
    tps.push(new Type.Lit("Text"));
    this.arrows = Type.fromList(tps);
    this.initArrows();

    if(this.itemCount_ < 2){
      this.setWarningText('This block requires at least 2 inputs');
    }
    else{
      this.setWarningText(null)
    }
  },

  mutationToDom: function() {
    var container = document.createElement('mutation');
    container.setAttribute('items', this.itemCount_);
    return container;
  },

  domToMutation: function(xmlElement) {
    this.itemCount_ = parseInt(xmlElement.getAttribute('items'), 10);

    this.inputList = [];
    var tps = [];
    for (var i = 0; i < this.itemCount_; i++){
      var input = this.appendValueInput('STR' + i);
      tps.push(new Type.Lit("Text"));
      if (i > 0) {
        input.appendField(new Blockly.FieldLabel("<>","blocklyTextEmph") );
      }
    };
    tps.push(new Type.Lit("Text"));

    this.arrows = Type.fromList(tps);
    this.initArrows();
  },

  saveConnections: function(containerBlock) {
    var itemBlock = containerBlock.getInputTargetBlock('STACK');
    var x = 0;
    while (itemBlock) {
      var input = this.getInput('STR' + x);
      if(input && input.connection.targetConnection){
        if(input.connection.targetBlock().isShadow_){
          x++;
          itemBlock = itemBlock.nextConnection &&
          itemBlock.nextConnection.targetBlock();
          continue;
        }
      }
      itemBlock.valueConnection_ = input && input.connection.targetConnection;
      x++;
      itemBlock = itemBlock.nextConnection &&
          itemBlock.nextConnection.targetBlock();
    }
  }
};

Blockly.Blocks['text_combine_ele'] = {
  /**
   * Mutator block for procedure argument.
   * @this Blockly.Block
   */
  init: function() {
    this.appendDummyInput()
        .appendField('text');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(textHUE);
    this.setTooltip('Adds a text input');
    this.contextMenu = false;
  },
  getExpr: null
};

Blockly.Blocks['text_combine_container'] = {
  init: function() {
    this.setColour(textHUE);
    this.appendDummyInput()
        .appendField('Text inputs');
    this.appendStatementInput('STACK');
    this.setTooltip('A list of inputs that the combine block should have');
    this.contextMenu = false;
  },
  getExpr: null
};


