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
    this.varTypes_ = [Type.generateTypeVar('lc'), Type.generateTypeVar('lc'), Type.generateTypeVar('lc')];
    this.appendValueInput("DO")
        .appendField(new Blockly.FieldLabel("List Comprehension","blocklyTextEmph"))
    this.appendValueInput('VAR0')
          .setAlign(Blockly.ALIGN_RIGHT)
          .appendField(new Blockly.FieldLocalVar(this.vars_[0],this.getArgType(0)))
          .appendField('\u2190');
    this.appendValueInput('VAR1')
          .setAlign(Blockly.ALIGN_RIGHT)
          .appendField(new Blockly.FieldLocalVar(this.vars_[1],this.getArgType(1)))
          .appendField('\u2190');
    this.setOutput(true);
    this.setMutator(new Blockly.Mutator(['lists_comp_var', 'lists_comp_guard']));
    this.setTooltip(Blockly.Msg.LISTS_CREATE_WITH_TOOLTIP);
    this.varCount_ = 2;
    this.guardCount_ = 0;
    this.resetArrows();
    Blockly.TypeInf.defineFunction("&&&", Type.fromList([Type.Lit("Truth"),Type.Lit("Truth"),Type.Lit("Truth")]));
    Blockly.TypeInf.defineFunction("filtB", Type.fromList([Type.Var('a'), Type.Lit("Truth"), Type.Var('a')  ]));
    Blockly.TypeInf.defineFunction("<]", Type.fromList([Type.Lit("list", [Type.Var("a")]), Type.Var("a")  ]));
    Blockly.TypeInf.defineFunction("MK", Type.fromList([Type.Var("a"), Type.Lit("list", [Type.Var("a")]) ]));
  },

  foldr1 : function(fn, xs) {
    var result = xs[xs.length - 1];
      for (var i = xs.length - 2; i > -1; i--) {
        result = fn(xs[i], result);
      }
    return result;
  },

  getExpr: function(){

    // Do main exp
    var mainExp = Exp.Var('undef');
    if (this.getInput("DO").connection.isConnected())
      mainExp = this.getInput("DO").connection.targetBlock().getExpr();

    mainExp.tag = this.getInput("DO").connection;
    
    // Do Guards
    var guardExps = [];
    for(var i = 0; i < this.guardCount_; i++){
      var inp = this.getInput("GUARD" + i);
      if(inp.connection && inp.connection.isConnected()){
        var exp = inp.connection.targetBlock().getExpr();
        exp.tag = inp.connection;
        guardExps.push(exp);
      }
      else{
        var exp = Exp.Var('undef');
        exp.tag = inp.connection;
        guardExps.push(exp);
      }
    }

    var boolComb = (a,b) => Exp.AppFunc([a,b],Exp.Var("&&&"));
    var guardExp;
    if(guardExps.length == 0){
      guardExp = Exp.Lit('Truth');
    }
    else if(guardExps.length == 1){
      var inp = this.getInput("GUARD0");
      if(inp.connection.isConnected()){
        guardExp = inp.connection.targetBlock().getExpr();
        guardExp.tag = inp.connection;
      }
      else{
        guardExp = Exp.Lit('Truth');
        guardExp.tag = inp.connection;
      }
    }
    else{
      guardExp = this.foldr1(boolComb,guardExps);
    }
    
    // Do variables
    var func = (a,b) => Exp.Let(a,b,c);   //Exp.AppFunc([a,b],Exp.Var(":"));
    var result = Exp.AppFunc([mainExp, guardExp], Exp.Var('filtB'));
    for(var i = this.varCount_ - 1; i !== -1; i--){
      var varName = this.vars_[i];
      var inp = this.getInput("VAR" + i);
      if(inp && inp.connection.isConnected()){
        var exp = inp.connection.targetBlock().getExpr();
        exp.tag = inp.connection;
        
        var letExp = Exp.App(Exp.Var("<]"),exp);
        var field = inp.fieldRow[0];
        if(!field.typeExpr)
          throw "Wrong field !";
        letExp.tag = field;

        result = Exp.Let(varName, letExp, result); 
      }
      else{
        var exp = Exp.Var('undef');
        exp.tag = inp.connection;
        
        var letExp = Exp.App(Exp.Var("<]"),exp);
        var field = inp.fieldRow[0];
        if(!field.typeExpr)
          throw "Wrong field !";
        letExp.tag = field;

        result = Exp.Let(varName, letExp, result); 
      }
    }
  
    // Do result
    result = Exp.AppFunc( [result], Exp.Var("MK"));
    result.tag = this.outputConnection;
    return result;

  },

  foldr: function (fn, ult, xs) {
    var result = ult;
      for (var i = xs.length - 1; i !== -1; i--) {
        result = fn(xs[i], result);
      }
    return result;
  },

  resetArrows: function(){
    this.arrows = null;
    var tps = [];
    this.varTypes_ = [];

    var a = Type.generateTypeVar('a');

    tps.push(a);
    for(var i = 0; i < this.varCount_; i++){
      var varTp = Type.generateTypeVar('lc');
      this.varTypes_.push(varTp);
      var t = Type.Lit("list", [varTp]);
      tps.push(t);
    }
    for(var i = 0; i < this.guardCount_; i++){
      tps.push(Type.Lit("Truth"));
    }

    tps.push(Type.Lit("list",[a]));
    this.arrows = Type.fromList(tps);
    this.initArrows(false);
  },

  assignVars: function(){
    var i = 0;
    var thisBlock = this;
    this.inputList.forEach(function(inp){
      if(inp.name.startsWith('VAR')){
        for(var f = 0; f < inp.fieldRow.length; f++){
          var fieldvar = inp.fieldRow[f];
          if(fieldvar instanceof Blockly.FieldLocalVar){
            var tp = thisBlock.varTypes_[i++];
            fieldvar.typeExpr = tp;
            break;
          }
        }
      };
    });
  },

  getArgType: function(localId){
    return this.varTypes_[localId];
  },

  getVars: function(connection){
    var i = 0;
    var available = [];
    for(i = 0; i < this.varCount_; i++){
      if(this.getInput('DO').connection == connection)
        return this.vars_;

      if(this.getInput('VAR' + i) && this.getInput('VAR' + i).connection == connection)
        return available;

      if(this.getInput('GUARD' + i) && this.getInput('GUARD' + i).connection == connection)
        return this.vars_;

      available = available.concat(this.vars_[i]);
      
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

        var input = this.appendValueInput('VAR' + i)
            .setAlign(Blockly.ALIGN_RIGHT)
            .appendField(new Blockly.FieldLocalVar(name, this.getArgType(i)))
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

    this.varCount_ = 0;
    this.guardCount_ = 0;
    // Rebuild the block's inputs.
    var itemBlock = containerBlock.getInputTargetBlock('STACK');
    while (itemBlock) {
      if(itemBlock.type == 'lists_comp_var')
      {
        var name = itemBlock.getFieldValue('NAME');
        this.vars_[this.varCount_] = name;
        var input = this.appendValueInput('VAR' + this.varCount_)
            .setAlign(Blockly.ALIGN_RIGHT)
            .appendField(new Blockly.FieldLocalVar(name, this.getArgType(this.varCount_)))
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

    Blockly.TypeInf.inferWorkspace(this.workspace);
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
   * Mutator block for adding items.
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
    this.appendValueInput("LEFT")
        .appendField("[");
    this.appendValueInput("RIGHT")
        .setAlign(Blockly.ALIGN_RIGHT)
        .appendField("...")
    this.appendDummyInput()
        .setAlign(Blockly.ALIGN_RIGHT)
        .appendField("]");
    this.setInputsInline(true);
    this.setOutput(true);

    Blockly.TypeInf.defineFunction("[..]", Type.fromList([Type.Lit("Number"), Type.Lit("Number"), Type.Lit("list", [Type.Lit("Number")])]));
    this.setAsFunction("[..]");

    this.setColour(listsHUE);
    this.setTooltip('Generates a list of numbers between the first and second inputs');
  }
};

Blockly.Blocks['lists_numgenstep'] = {
  init: function() {
    this.appendValueInput("LEFT")
        .appendField("[");
    this.appendValueInput("NEXT")
        .setAlign(Blockly.ALIGN_RIGHT)
        .appendField(",");
    this.appendValueInput("RIGHT")
        .setAlign(Blockly.ALIGN_RIGHT)
        .appendField("...")
    this.appendDummyInput()
        .setAlign(Blockly.ALIGN_RIGHT)
        .appendField("]");
    this.setInputsInline(true);
    this.setOutput(true);

    Blockly.TypeInf.defineFunction("[,..]", Type.fromList([Type.Lit("Number"), Type.Lit("Number"), Type.Lit("Number"), Type.Lit("list", [Type.Lit("Number")])]));
    this.setAsFunction("[,..]");

    this.setColour(listsHUE);
    this.setTooltip('Generates a list of numbers between the first and second inputs, with a step');
  }
};

Blockly.Blocks['lists_length'] = {
  init: function() {
    this.setColour(210);
    this.appendValueInput('LST')
        .appendField(new Blockly.FieldLabel("length","blocklyTextEmph"))
        .appendField("(");
    this.appendDummyInput()
        .setAlign(Blockly.ALIGN_RIGHT)
        .appendField(")");
    this.setOutput(true);
    Blockly.TypeInf.defineFunction("length", Type.fromList([Type.Lit("list", [Type.Var("a")]), Type.Lit("Number") ]));
    this.setAsFunction("length");
  }
};

Blockly.Blocks['lists_repeating'] = {
  init: function() {
    this.setColour(listsHUE);
    this.appendValueInput('LST')
        .appendField(new Blockly.FieldLabel("repeating","blocklyTextEmph"))
        .appendField("(");
    this.appendDummyInput()
        .setAlign(Blockly.ALIGN_RIGHT)
        .appendField(")");
    this.setOutput(true);
    var lstType = Type.Lit("list", [Type.Var("a")]);
    Blockly.TypeInf.defineFunction("repeating", Type.fromList([lstType, lstType]));
    this.setAsFunction("repeating");
  }
};

Blockly.Blocks['lists_shuffled'] = {
  init: function() {
    this.setColour(listsHUE);
    this.appendValueInput('LST')
        .appendField(new Blockly.FieldLabel("shuffled","blocklyTextEmph"))
        .appendField("(");
    this.appendValueInput('SEED')
        .setAlign(Blockly.ALIGN_RIGHT)
        .appendField(",");
    this.appendDummyInput()
        .setAlign(Blockly.ALIGN_RIGHT)
        .appendField(")");
    this.setOutput(true);
    var lstType = Type.Lit("list", [Type.Var("a")]);
    Blockly.TypeInf.defineFunction("shuffled", Type.fromList([lstType, Type.Lit("Number"), lstType]));
    this.setAsFunction("shuffled");
  }
};

Blockly.Blocks['lists_sorted'] = {
  init: function() {
    this.setColour(listsHUE);
    this.appendValueInput('LST')
        .appendField(new Blockly.FieldLabel("sorted","blocklyTextEmph"))
        .appendField("(");
    this.appendDummyInput()
        .setAlign(Blockly.ALIGN_RIGHT)
        .appendField(")");
    this.setOutput(true);
    var lstType = Type.Lit("list", [Type.Lit("Number")]);
    Blockly.TypeInf.defineFunction("sorted", Type.fromList([lstType, lstType]));
    this.setAsFunction("sorted");
  }
};

Blockly.Blocks['lists_reversed'] = {
  init: function() {
    this.setColour(listsHUE);
    this.appendValueInput('LST')
        .appendField(new Blockly.FieldLabel("reversed","blocklyTextEmph"))
        .appendField("(");
    this.appendDummyInput()
        .setAlign(Blockly.ALIGN_RIGHT)
        .appendField(")");
    this.setOutput(true);
    var lstType = Type.Lit("list", [Type.Var("a")]);
    Blockly.TypeInf.defineFunction("reversed", Type.fromList([lstType, lstType]));
    this.setAsFunction("reversed");
  }
};

Blockly.Blocks['lists_first'] = {
  init: function() {
    this.setColour(listsHUE);
    this.appendValueInput('LST')
        .appendField(new Blockly.FieldLabel("first","blocklyTextEmph"))
        .appendField("(");
    this.appendValueInput('COUNT')
        .setAlign(Blockly.ALIGN_RIGHT)
        .appendField(",");
    this.appendDummyInput()
        .setAlign(Blockly.ALIGN_RIGHT)
        .appendField(")");
    this.setOutput(true);
    var lstType = Type.Lit("list", [Type.Var("a")]);
    Blockly.TypeInf.defineFunction("first", Type.fromList([lstType, Type.Lit("Number"), lstType]));
    this.setAsFunction("first");
  }
};

Blockly.Blocks['lists_rest'] = {
  init: function() {
    this.setColour(listsHUE);
    this.appendValueInput('LST')
        .appendField(new Blockly.FieldLabel("rest","blocklyTextEmph"))
        .appendField("(");
    this.appendValueInput('COUNT')
        .setAlign(Blockly.ALIGN_RIGHT)
        .appendField(",");
    this.appendDummyInput()
        .setAlign(Blockly.ALIGN_RIGHT)
        .appendField(")");
    this.setOutput(true);
    var lstType = Type.Lit("list", [Type.Var("a")]);
    Blockly.TypeInf.defineFunction("rest", Type.fromList([lstType, Type.Lit("Number"), lstType]));
    this.setAsFunction("rest");
  }
};

Blockly.Blocks['lists_at'] = {
  init: function() {
    this.setColour(180);
    this.appendValueInput('LST');
    this.appendValueInput('POS')
        .setAlign(Blockly.ALIGN_RIGHT)
        .appendField(new Blockly.FieldLabel("#","blocklyTextEmph"));
    this.setOutput(true);
    this.setInputsInline(true);

    Blockly.TypeInf.defineFunction("at", 
          Type.fromList([Type.Lit("list", [Type.Var("a")]), Type.Lit("Number"), Type.Var("a")]));
    this.setAsFunction("at");
  }
};

Blockly.Blocks['lists_cons'] = {
  init: function() {
    this.setColour(listsHUE);
    this.appendValueInput('ITEM');
    this.appendValueInput('LST')
        .setAlign(Blockly.ALIGN_RIGHT)
        .appendField(new Blockly.FieldLabel(":","blocklyTextEmph"));
    this.setOutput(true);
    this.setInputsInline(true);
    
    var lst = Type.Lit("list", [Type.Var("a")]);
    Blockly.TypeInf.defineFunction(":", Type.fromList([Type.Var("a"), lst, lst ]));
    this.setAsFunction(":");

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


    Blockly.TypeInf.defineFunction(":", Type.fromList([Type.Var("a"),Type.Lit("list",[Type.Var('a')]),Type.Lit("list", [Type.Var('a')]) ]));
  },
  getType: function(){
    return this.outputConnection.typeExpr.children[0];
  },
  
  foldr: function (fn, ult, xs) {
    var result = ult;
      for (var i = xs.length - 1; i !== -1; i--) {
        result = fn(xs[i], result);
      }
    return result;
  },

  getExpr: function(){
    var exps = [];
    this.inputList.forEach(function(inp){
      if(inp.connection.isConnected()){
        var exp = inp.connection.targetBlock().getExpr();
        exp.tag = inp.connection;
        exps.push(exp);
      }
      else{
        var exp = Exp.Var('undef');
        exp.tag = inp.connection;
        exps.push(exp);
      }
    });
    var func = (a,b) => Exp.AppFunc([a,b],Exp.Var(":"));
    var e = this.foldr(func,Exp.Var("[]"),exps);
    e.tag = this.outputConnection;
    return e;
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
          .appendField("[ ]");
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
        input.appendField(new Blockly.FieldImage("ims/format-list-bulleted.svg",20,20))
        input.appendField(new Blockly.FieldLabel("List","blocklyTextEmph"));
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
          .appendField("[ ]");
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
