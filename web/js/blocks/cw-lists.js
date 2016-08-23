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

goog.provide('Blockly.Blocks.cwLists');

goog.require('Blockly.Blocks');

var listsHUE = 260;


Blockly.Blocks['lists_comprehension'] = {
  /**
   * Block for creating a list with any number of elements of any type.
   * @this Blockly.Block
   */
  init: function() {
    this.setColour(listsHUE);
    this.vars_ = ['i','j','k'];
    this.varTypes_ = [Type.Var('a'), Type.Var('b'), Type.Var('c')];
    this.appendValueInput("DO")
        .appendField(new Blockly.FieldLabel("List Comprehension","blocklyTextEmph"))
    this.appendValueInput('VAR0')
          .setAlign(Blockly.ALIGN_RIGHT)
          .appendField(new Blockly.FieldVarInput(this.vars_[0],this.getArgType, 0))
          .appendField('\u2190');
    this.appendValueInput('VAR1')
          .setAlign(Blockly.ALIGN_RIGHT)
          .appendField(new Blockly.FieldVarInput(this.vars_[1],this.getArgType, 1 ))
          .appendField('\u2190');
    this.setOutput(true);
    this.setMutator(new Blockly.Mutator(['lists_comp_var', 'lists_comp_guard']));
    this.setTooltip(Blockly.Msg.LISTS_CREATE_WITH_TOOLTIP);
    this.varCount_ = 2;
    this.guardCount_ = 0;
    this.resetArrows();
  },
  
  resetArrows: function(){
    this.arrows = null;
    var tps = [];
    var i = 0;

    tps.push(Type.Var("a"));
    for(; i < this.varCount_; i++){
      var c = String.fromCharCode(66 + i);
      var t = Type.Lit("list", [Type.Var(c)]);
      tps.push(t);
    }
    i++;
    tps.push(Type.Var("a"));
    this.arrows = Type.fromList(tps);
    this.initArrows();
  },

  resetVarTypes: function(){
    this.inputList.forEach(function(inp){
      if(inp.name.startsWith('VAR')){
        for (var l = 0; l < inp.fieldRow.length; l++)
        {
          var f = inp.fieldRow[l];
          if(f instanceof Blockly.FieldVarInput){
            f.render_();
          }
        }
      }
    });
  },

  onTypeChange: function(){
    // set variables to input types
    var j = 0;
    var thisBlock = this;
    this.inputList.forEach(function(inp){
      if(inp.name.startsWith('VAR')){
        thisBlock.varTypes_[j] = inp.connection.typeExpr.children[0];
        j++;
      }
    });
    this.resetVarTypes();
  },

  getArgType: function(localId){
    return this.varTypes_[localId];
  },

  getVars: function(connection){
    var i = 0;
    for(i = 0; i < this.varCount_; i++){
      if(this.getInput('DO').connection == connection)
        return this.vars_;
      if(this.getInput('VAR' + i).connection == connection)
        return [this.vars_[i]];
      
    }
    return [];
  },
  /**
   * Create XML to represent list inputs.
   * @return {Element} XML storage element.
   * @this Blockly.Block
   */
  mutationToDom: function() {
    var container = document.createElement('mutation');
    container.setAttribute('guardcount', this.guardCount_);
    container.setAttribute('varcount', this.varCount_);

    for (var i = 0; i < this.varCount_; i++) {
      var parameter = document.createElement('var');
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
  domToMutation: function(xmlElement) {

    for (var x = 0; x < this.varCount_; x++) {
      this.removeInput('VAR' + x);
    }
    for (var x = 0; x < this.guardCount_; x++) {
      this.removeInput('GUARD' + x);
    }
    this.vars_ = [];
    this.varTypes_ = [];

    this.varCount_ = parseInt(xmlElement.getAttribute('varcount'), 10);
    this.guardCount_ = parseInt(xmlElement.getAttribute('guardcount'), 10);

    for (var i = 0, childNode; childNode = xmlElement.childNodes[i]; i++) {
      if (childNode.nodeName.toLowerCase() == 'var') {
        var name = childNode.getAttribute('name');

        var chr = String.fromCharCode(97 + i); 
        this.varTypes_.push(Type.Var(chr));

        var input = this.appendValueInput('VAR' + i)
            .setAlign(Blockly.ALIGN_RIGHT)
            .appendField(new Blockly.FieldVarInput(name, this.getArgType, i))
            .appendField('\u2190');
        this.vars_.push(name);
      }
    }

    for (var x = 0; x < this.guardCount_; x++){
      var input = this.appendValueInput('GUARD' + x)
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
  decompose: function(workspace) {
    var containerBlock =
        workspace.newBlock('lists_create_with_container');
    containerBlock.initSvg();
    var connection = containerBlock.getInput('STACK').connection;

    for (var x = 0; x < this.varCount_; x++) {
      var itemBlock = workspace.newBlock('lists_comp_var');
      itemBlock.setFieldValue(this.vars_[x], 'NAME');
      itemBlock.initSvg();
      connection.connect(itemBlock.previousConnection);
      connection = itemBlock.nextConnection;
    }
    for (var x = 0; x < this.guardCount_; x++) {
      var itemBlock = workspace.newBlock('lists_comp_guard');
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
  compose: function(containerBlock) {
    // Disconnect all input blocks and remove all inputs.
    for (var x = this.varCount_ - 1; x >= 0; x--) {
      this.removeInput('VAR' + x);
    }
    for (var x = this.guardCount_ - 1; x >= 0; x--) {
      this.removeInput('GUARD' + x);
    }
    this.vars_ = [];
    this.varTypes_ = [];

    this.varCount_ = 0;
    this.guardCount_ = 0;
    // Rebuild the block's inputs.
    var itemBlock = containerBlock.getInputTargetBlock('STACK');
    while (itemBlock) {
      if(itemBlock.type == 'lists_comp_var')
      {
        this.varTypes_.push(Blockly.TypeVar.getUnusedTypeVar());

        var name = itemBlock.getFieldValue('NAME');
        this.vars_[this.varCount_] = name;
        var input = this.appendValueInput('VAR' + this.varCount_)
            .setAlign(Blockly.ALIGN_RIGHT)
            .appendField(new Blockly.FieldVarInput(name, this.getArgType, this.varCount_))
            .appendField('\u2190');

        this.vars_.push(name);
        // Reconnect any child blocks.
        if (itemBlock.valueConnection_) {
          input.connection.connect(itemBlock.valueConnection_);
        }
        this.varCount_++;
      }
      else if(itemBlock.type == 'lists_comp_guard')
      {
          var input = this.appendValueInput('GUARD' + this.guardCount_)
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
  },
  /**
   * Store pointers to any connected child blocks.
   * @param {!Blockly.Block} containerBlock Root block in mutator.
   * @this Blockly.Block
   */
  saveConnections: function(containerBlock) {
    var itemBlock = containerBlock.getInputTargetBlock('STACK');
    var x = 0;
    while (itemBlock) {
      var input = this.getInput('VAR' + x);
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
  init: function() {
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
   * Mutator bolck for adding items.
   * @this Blockly.Block
   */
  init: function() {
    this.setColour(listsHUE);
    this.appendDummyInput()
        .appendField("Guard");
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setTooltip("Guard against a boolean expression");
    this.contextMenu = false;
  }
};

Blockly.Blocks['lists_numgen'] = {
  init: function() {
    this.appendDummyInput()
        .appendField("[");
    this.appendValueInput("LEFT");
    this.appendValueInput("RIGHT")
        .appendField("...")
    this.appendDummyInput()
        .appendField("]");
    this.setInputsInline(true);
    this.setOutput(true);
    this.setAsLiteralT(Type.Lit("lit", [Type.Lit("Number")]));

    this.setColour(listsHUE);
    this.setTooltip('Generates a list of numbers between the first and second inputs');
  }
};

Blockly.Blocks['lists_length'] = {
  init: function() {
    var a = Blockly.TypeVar.getUnusedTypeVar();
    this.setColour(210);
    this.appendValueInput('LST')
        .appendField(new Blockly.FieldLabel("length","blocklyTextEmph") );
    this.setOutput(true);
    Blockly.TypeInf.defineFunction("length", Type.fromList([Type.Lit("list", [Type.Var("a")]), Type.Lit("Number") ]));
    this.setAsFunction("length");
  }
};

Blockly.Blocks['lists_at'] = {
  init: function() {
    var a = Blockly.TypeVar.getUnusedTypeVar();
    this.setColour(210);
    this.appendValueInput('LST');
    this.appendValueInput('POS')
        .appendField(new Blockly.FieldLabel("at","blocklyTextEmph") );
    this.setOutput(true);
    this.setInputsInline(true);
    
    Blockly.TypeInf.defineFunction("at", 
          Type.fromList([Type.Lit("list", [Type.Var("a")]), Type.Lit("Number"), Type.Var("a")]));
    this.setAsFunction("at");
  }
};

Blockly.Blocks['lists_cons'] = {
  init: function() {
    var a = Blockly.TypeVar.getUnusedTypeVar();
    this.setColour(210);
    this.appendValueInput('ITEM');
    this.appendValueInput('LST')
        .appendField(new Blockly.FieldLabel(":","blocklyTextEmph") );
    this.setOutput(true);
    this.setInputsInline(true);
    
    var lst = Type.Lit("list", [Type.Var("a")]);
    Blockly.TypeInf.defineFunction(":", Type.fromList([Type.Var("a"), lst, lst ]));
    this.setAsFunction(lst);

  }
};



Blockly.Blocks['lists_create_with_typed'] = {
  /**
   * Block for creating a list with any number of elements of any type.
   * @this Blockly.Block
   */
  init: function() {
    this.setColour(260);
    this.appendValueInput('ADD0')
        .appendField(new Blockly.FieldImage("ims/format-list-bulleted.svg",20,20))
        .appendField(new Blockly.FieldLabel("List","blocklyTextEmph"));
    this.appendValueInput('ADD1');
    this.appendValueInput('ADD2');
    this.setOutput(true);
    this.setMutator(new Blockly.Mutator(['lists_create_with_item']));
    this.setTooltip(Blockly.Msg.LISTS_CREATE_WITH_TOOLTIP);
    this.itemCount_ = 3;
    var tps = [];
    for(var k = 0; k < this.itemCount_; k++){
      tps.push(Type.Var("a"));
    }
    tps.push(Type.Lit("list",[Type.Var("a")]));
    this.arrows = Type.fromList(tps);
  },
  getType: function(){
    return this.outputConnection.typeExpr.children[0];
  },
  /**
   * Create XML to represent list inputs.
   * @return {Element} XML storage element.
   * @this Blockly.Block
   */
  mutationToDom: function() {
    var container = document.createElement('mutation');
    container.setAttribute('items', this.itemCount_);
    return container;
  },
  /**
   * Parse XML to restore the list inputs.
   * @param {!Element} xmlElement XML storage element.
   * @this Blockly.Block
   */
  domToMutation: function(xmlElement) {
    for (var x = 0; x < this.itemCount_; x++) {
      this.removeInput('ADD' + x);
    }
    this.itemCount_ = parseInt(xmlElement.getAttribute('items'), 10);
    for (var x = 0; x < this.itemCount_; x++) {
      var input = this.appendValueInput('ADD' + x);
      if (x == 0) {

        input.appendField(new Blockly.FieldImage("ims/format-list-bulleted.svg",20,20));
        input.appendField("List");
      }
    }
    if (this.itemCount_ == 0) {
      this.appendDummyInput('EMPTY')
          .appendField(Blockly.Msg.LISTS_CREATE_EMPTY_TITLE);
    }
    var tps = [];
    for(var k = 0; k < this.itemCount_; k++){
      tps.push(Type.Var("a"));
    }
    tps.push(Type.Lit("list",[Type.Var("a")]));
    this.arrows = Type.fromList(tps);
    this.initArrows();
  },
  /**
   * Populate the mutator's dialog with this block's components.
   * @param {!Blockly.Workspace} workspace Mutator's workspace.
   * @return {!Blockly.Block} Root block in mutator.
   * @this Blockly.Block
   */
  decompose: function(workspace) {
    var containerBlock =
        workspace.newBlock('lists_create_with_container');
    containerBlock.initSvg();
    var connection = containerBlock.getInput('STACK').connection;
    for (var x = 0; x < this.itemCount_; x++) {
      var itemBlock = workspace.newBlock('lists_create_with_item');
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
  compose: function(containerBlock) {
    // Disconnect all input blocks and remove all inputs.
    if (this.itemCount_ == 0) {
      this.removeInput('EMPTY');
    } else {
      for (var x = this.itemCount_ - 1; x >= 0; x--) {
        this.removeInput('ADD' + x);
      }
    }
    this.itemCount_ = 0;
    // Rebuild the block's inputs.
    var itemBlock = containerBlock.getInputTargetBlock('STACK');
    while (itemBlock) {
      var input = this.appendValueInput('ADD' + this.itemCount_);
      if (this.itemCount_ == 0) {
        input.appendField(new Blockly.FieldImage("ims/format-list-bulleted.svg",20,20));
        input.appendField("list");
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
          .appendField("[]");
    }

    var tps = [];
    for(var k = 0; k < this.itemCount_; k++){
      tps.push(Type.Var("a"));
    }
    tps.push(Type.Lit("list",[Type.Var("a")]));
    this.arrows = Type.fromList(tps);
    this.initArrows();

    this.renderMoveConnections_();


  },
  /**
   * Store pointers to any connected child blocks.
   * @param {!Blockly.Block} containerBlock Root block in mutator.
   * @this Blockly.Block
   */
  saveConnections: function(containerBlock) {
    var itemBlock = containerBlock.getInputTargetBlock('STACK');
    var x = 0;
    while (itemBlock) {
      var input = this.getInput('ADD' + x);
      itemBlock.valueConnection_ = input && input.connection.targetConnection;
      x++;
      itemBlock = itemBlock.nextConnection &&
          itemBlock.nextConnection.targetBlock();
    }
  }
};


