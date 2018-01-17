/*
 * Copyright 2018 The CodeWorld Authors. All rights reserved.
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

goog.provide('Blockly.Blocks.cwPictures');

goog.require('Blockly.Blocks');

var picsHUE = 160;
Blockly.Blocks['cwCombine'] = {
  init: function() {
    this.appendValueInput('PIC0');
    this.appendValueInput('PIC1')
        .appendField(new Blockly.FieldLabel("&","blocklyTextEmph") );
    this.setColour(picsHUE);
    this.setMutator(new Blockly.Mutator(['pics_combine_ele']));
    this.setTooltip('Combine multiple pictures');
    this.itemCount_ = 2;
    this.setOutput(true);

    Blockly.TypeInf.defineFunction("&", Type.fromList([Type.Lit("Picture"),Type.Lit("Picture"),Type.Lit("Picture")]));
    this.setAsFunction("&");
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
    var func = (a,b) => Exp.AppFunc([a,b],Exp.Var("&"));
    var e = this.foldr1(func,exps);
    return e;
  },


  decompose: function(workspace) {
    var containerBlock =
        workspace.newBlock('pics_combine_container');
    containerBlock.initSvg();
    var connection = containerBlock.getInput('STACK').connection;

    for (var x = 0; x < this.itemCount_; x++) {
      var itemBlock = workspace.newBlock('pics_combine_ele');
      itemBlock.initSvg();
      connection.connect(itemBlock.previousConnection);
      connection = itemBlock.nextConnection;
    }

    return containerBlock;
  },

  compose: function(containerBlock) {
    var tps = [];
    
    for (var x = 0; x < this.itemCount_; x++) {
      this.removeInput('PIC' + x);
    }

    this.itemCount_ = 0;
    // Rebuild the block's inputs.
    var itemBlock = containerBlock.getInputTargetBlock('STACK');
    while (itemBlock) {
      var input = this.appendValueInput('PIC' + this.itemCount_);
      tps.push(Type.Lit("Picture"));
      if (this.itemCount_ > 0) {
        input.appendField(new Blockly.FieldLabel("&","blocklyTextEmph") );
      }
      if (itemBlock.valueConnection_) {
        input.connection.connect(itemBlock.valueConnection_);
      }
      itemBlock = itemBlock.nextConnection &&
          itemBlock.nextConnection.targetBlock();
      this.itemCount_++;
    }
    tps.push(Type.Lit("Picture"));
    this.arrows = Type.fromList(tps);
    this.initArrows();
    this.renderMoveConnections_();

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
    var tps = [];
    this.inputList = [];
    for (var i = 0; i < this.itemCount_; i++){
      var input = this.appendValueInput('PIC' + i);
      tps.push(Type.Lit("Picture"));
      if (i > 0) {
        input.appendField(new Blockly.FieldLabel("&","blocklyTextEmph") );
      }
    };
    tps.push(Type.Lit("Picture"));
    this.arrows = Type.fromList(tps);
    this.initArrows();
  },
  saveConnections: function(containerBlock) {
    var itemBlock = containerBlock.getInputTargetBlock('STACK');
    var x = 0;
    while (itemBlock) {
      var input = this.getInput('PIC' + x);
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

Blockly.Blocks['pics_combine_ele'] = {
  /**
   * Mutator block for procedure argument.
   * @this Blockly.Block
   */
  init: function() {
    this.appendDummyInput()
        .appendField('picture');
    this.setPreviousStatement(true);
    this.setNextStatement(true);
    this.setColour(picsHUE);
    this.setTooltip('Adds a picture input');
    this.contextMenu = false;
  },
  getExpr: null
};

Blockly.Blocks['pics_combine_container'] = {
  init: function() {
    this.setColour(picsHUE);
    this.appendDummyInput()
        .appendField('Picture inputs');
    this.appendStatementInput('STACK');
    this.setTooltip('A list of inputs that the combine block should have');
    this.contextMenu = false;
  },
  getExpr: null
};

Blockly.Blocks['lists_pictures'] = {
  init: function() {
    this.setColour(160);
    this.appendValueInput('LST')
        .appendField(new Blockly.FieldLabel("pictures","blocklyTextEmph"))
        .appendField('(');
    this.appendDummyInput()
        .setAlign(Blockly.ALIGN_RIGHT)
        .appendField(')');
    this.setInputsInline(false);
    this.setOutput(true);
    var pic = Type.Lit("Picture");
    Blockly.TypeInf.defineFunction("pictures", Type.fromList([Type.Lit("list", [pic]), pic]));
    this.setAsFunction("pictures");
  }
};

Blockly.Blocks['lists_polyline'] = {
  init: function() {
    this.setColour(160);
    this.appendValueInput('LST')
        .appendField(new Blockly.FieldLabel("polyline","blocklyTextEmph"))
        .appendField('(');
    this.appendDummyInput()
        .setAlign(Blockly.ALIGN_RIGHT)
        .appendField(')');
    this.setInputsInline(false);
    this.setOutput(true);
    var pair = Type.Lit("pair", [Type.Lit("Number"), Type.Lit("Number")]);
    Blockly.TypeInf.defineFunction("polyline", Type.fromList([Type.Lit("list", [pair]), Type.Lit("Picture")]));
    this.setAsFunction("polyline");
  }
};

Blockly.Blocks['lists_thickPolyline'] = {
  init: function() {
    this.setColour(160);
    this.appendValueInput('LST')
        .appendField(new Blockly.FieldLabel("thickPolyline","blocklyTextEmph"))
        .appendField('(');
    this.appendValueInput('THICKNESS')
        .setAlign(Blockly.ALIGN_RIGHT)
        .appendField(',');
    this.appendDummyInput()
        .setAlign(Blockly.ALIGN_RIGHT)
        .appendField(')');
    this.setInputsInline(false);
    this.setOutput(true);

    var pair = Type.Lit("pair", [Type.Lit("Number"), Type.Lit("Number")]);
    var pts = Type.Lit("list", [pair]);
    var num = Type.Lit("Number");
    Blockly.TypeInf.defineFunction("thickPolyline", Type.fromList([pts, num, Type.Lit("Picture")]));
    this.setAsFunction("thickPolyline");
  }
};

Blockly.Blocks['lists_polygon'] = {
  init: function() {
    this.setColour(160);
    this.appendValueInput('LST')
        .appendField(new Blockly.FieldLabel("polygon","blocklyTextEmph"))
        .appendField('(');
    this.appendDummyInput()
        .setAlign(Blockly.ALIGN_RIGHT)
        .appendField(')');
    this.setInputsInline(false);
    this.setOutput(true);
    var pair = Type.Lit("pair", [Type.Lit("Number"), Type.Lit("Number")]);
    Blockly.TypeInf.defineFunction("polygon", Type.fromList([Type.Lit("list", [pair]), Type.Lit("Picture")]));
    this.setAsFunction("polygon");
  }
};

Blockly.Blocks['lists_solidPolygon'] = {
  init: function() {
    this.setColour(160);
    this.appendValueInput('LST')
        .appendField(new Blockly.FieldLabel("solidPolygon","blocklyTextEmph"))
        .appendField('(');
    this.appendDummyInput()
        .setAlign(Blockly.ALIGN_RIGHT)
        .appendField(')');
    this.setInputsInline(false);
    this.setOutput(true);
    var pair = Type.Lit("pair", [Type.Lit("Number"), Type.Lit("Number")]);
    Blockly.TypeInf.defineFunction("solidPolygon", Type.fromList([Type.Lit("list", [pair]), Type.Lit("Picture")]));
    this.setAsFunction("solidPolygon");
  }
};

Blockly.Blocks['lists_thickPolygon'] = {
  init: function() {
    this.setColour(160);
    this.appendValueInput('LST')
        .appendField(new Blockly.FieldLabel("thickPolygon","blocklyTextEmph"))
        .appendField('(');
    this.appendValueInput('THICKNESS')
        .setAlign(Blockly.ALIGN_RIGHT)
        .appendField(',');
    this.appendDummyInput()
        .setAlign(Blockly.ALIGN_RIGHT)
        .appendField(')');
    this.setInputsInline(false);
    this.setOutput(true);

    var pair = Type.Lit("pair", [Type.Lit("Number"), Type.Lit("Number")]);
    var pts = Type.Lit("list", [pair]);
    var num = Type.Lit("Number");
    Blockly.TypeInf.defineFunction("thickPolygon", Type.fromList([pts, num, Type.Lit("Picture")]));
    this.setAsFunction("thickPolygon");
  }
};

Blockly.Blocks['lists_curve'] = {
  init: function() {
    this.setColour(160);
    this.appendValueInput('LST')
        .appendField(new Blockly.FieldLabel("curve","blocklyTextEmph"))
        .appendField('(');
    this.appendDummyInput()
        .setAlign(Blockly.ALIGN_RIGHT)
        .appendField(')');
    this.setInputsInline(false);
    this.setOutput(true);
    var pair = Type.Lit("pair", [Type.Lit("Number"), Type.Lit("Number")]);
    Blockly.TypeInf.defineFunction("curve", Type.fromList([Type.Lit("list", [pair]), Type.Lit("Picture")]));
    this.setAsFunction("curve");
  }
};

Blockly.Blocks['lists_thickCurve'] = {
  init: function() {
    this.setColour(160);
    this.appendValueInput('LST')
        .appendField(new Blockly.FieldLabel("thickCurve","blocklyTextEmph"))
        .appendField('(');
    this.appendValueInput('THICKNESS')
        .setAlign(Blockly.ALIGN_RIGHT)
        .appendField(',');
    this.appendDummyInput()
        .setAlign(Blockly.ALIGN_RIGHT)
        .appendField(')');
    this.setInputsInline(false);
    this.setOutput(true);

    var pair = Type.Lit("pair", [Type.Lit("Number"), Type.Lit("Number")]);
    var pts = Type.Lit("list", [pair]);
    var num = Type.Lit("Number");
    Blockly.TypeInf.defineFunction("thickCurve", Type.fromList([pts, num, Type.Lit("Picture")]));
    this.setAsFunction("thickCurve");
  }
};

Blockly.Blocks['lists_closedCurve'] = {
  init: function() {
    this.setColour(160);
    this.appendValueInput('LST')
        .appendField(new Blockly.FieldLabel("closedCurve","blocklyTextEmph"))
        .appendField('(');
    this.appendDummyInput()
        .setAlign(Blockly.ALIGN_RIGHT)
        .appendField(')');
    this.setInputsInline(false);
    this.setOutput(true);
    var pair = Type.Lit("pair", [Type.Lit("Number"), Type.Lit("Number")]);
    Blockly.TypeInf.defineFunction("closedCurve", Type.fromList([Type.Lit("list", [pair]), Type.Lit("Picture")]));
    this.setAsFunction("closedCurve");
  }
};

Blockly.Blocks['lists_solidClosedCurve'] = {
  init: function() {
    this.setColour(160);
    this.appendValueInput('LST')
        .appendField(new Blockly.FieldLabel("solidClosedCurve","blocklyTextEmph"))
        .appendField('(');
    this.appendDummyInput()
        .setAlign(Blockly.ALIGN_RIGHT)
        .appendField(')');
    this.setInputsInline(false);
    this.setOutput(true);
    var pair = Type.Lit("pair", [Type.Lit("Number"), Type.Lit("Number")]);
    Blockly.TypeInf.defineFunction("solidClosedCurve", Type.fromList([Type.Lit("list", [pair]), Type.Lit("Picture")]));
    this.setAsFunction("solidClosedCurve");
  }
};

Blockly.Blocks['lists_thickClosedCurve'] = {
  init: function() {
    this.setColour(160);
    this.appendValueInput('LST')
        .appendField(new Blockly.FieldLabel("thickClosedCurve","blocklyTextEmph"))
        .appendField('(');
    this.appendValueInput('THICKNESS')
        .setAlign(Blockly.ALIGN_RIGHT)
        .appendField(',');
    this.appendDummyInput()
        .setAlign(Blockly.ALIGN_RIGHT)
        .appendField(')');
    this.setInputsInline(false);
    this.setOutput(true);

    var pair = Type.Lit("pair", [Type.Lit("Number"), Type.Lit("Number")]);
    var pts = Type.Lit("list", [pair]);
    var num = Type.Lit("Number");
    Blockly.TypeInf.defineFunction("thickClosedCurve", Type.fromList([pts, num, Type.Lit("Picture")]));
    this.setAsFunction("thickClosedCurve");
  }
};
