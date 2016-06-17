/*
 * Copyright 2016 The CodeWorld Authors. All Rights Reserved.
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

goog.provide('Blockly.FunBlocks');

goog.require('Blockly.Generator');

Blockly.FunBlocks = new Blockly.Generator('FunBlocks');

Blockly.FunBlocks.addReservedWords('Blockly,do,let,where,if,then,else,in');

Blockly.FunBlocks.init = function(workspace) {

  Blockly.FunBlocks.definitions_ = Object.create(null);

  Blockly.FunBlocks.functionNames_ = Object.create(null);

  if (!Blockly.FunBlocks.variableDB_) {
    Blockly.FunBlocks.variableDB_ =
        new Blockly.Names(Blockly.FunBlocks.RESERVED_WORDS_);
  } else {
    Blockly.FunBlocks.variableDB_.reset();
  }

  var defvars = [];
  var variables = Blockly.Variables.allVariables(workspace);
  if (variables.length) {
    for (var i = 0; i < variables.length; i++) {
      defvars[i] = Blockly.FunBlocks.variableDB_.getName(variables[i],
          Blockly.Variables.NAME_TYPE);
    }
    Blockly.FunBlocks.definitions_['variables'] =
        'let ' + defvars.join(', ') + ';';
  }
};

Blockly.FunBlocks.finish = function(code) {

  var definitions = [];
  for (var name in Blockly.FunBlocks.definitions_) {
    definitions.push(Blockly.FunBlocks.definitions_[name]);
  }

  delete Blockly.FunBlocks.definitions_;
  delete Blockly.FunBlocks.functionNames_;
  Blockly.FunBlocks.variableDB_.reset();
  return definitions.join('\n\n') + '\n\n\n' + code;
};

Blockly.FunBlocks.scrubNakedValue = function(line) {
  return line; // + ';\n';
};

Blockly.FunBlocks.quote_ = function(string) {
    // TODO escape strings
    return '"' + string + '"';
};

Blockly.FunBlocks.scrub_ = function(block, code) {
  var commentCode = '';
  if (!block.outputConnection || !block.outputConnection.targetConnection) {
    var comment = block.getCommentText();
    if (comment) {
      commentCode += Blockly.FunBlocks.prefixLines(comment, '// ') + '\n';
    }
    for (var x = 0; x < block.inputList.length; x++) {
      if (block.inputList[x].type == Blockly.INPUT_VALUE) {
        var childBlock = block.inputList[x].connection.targetBlock();
        if (childBlock) {
          var comment = Blockly.FunBlocks.allNestedComments(childBlock);
          if (comment) {
            commentCode += Blockly.FunBlocks.prefixLines(comment, '// ');
          }
        }
      }
    }
  }
  var nextBlock = block.nextConnection && block.nextConnection.targetBlock();
  var nextCode = Blockly.FunBlocks.blockToCode(nextBlock);
  return commentCode + code + nextCode;
}


Blockly.FunBlocks.workspaceToCode = function(workspace) {
  if (!workspace) {
    // Backwards compatability from before there could be multiple workspaces.
    console.warn('No workspace specified in workspaceToCode call.  Guessing.');
    workspace = Blockly.getMainWorkspace();
  }
  var code = [];
  this.init(workspace);
  var blocks = workspace.getTopBlocks(true);
  for (var x = 0, block; block = blocks[x]; x++) {
    if (block.outputConnection != null)
      continue; // skip non terminal blocks
    var line = this.blockToCode(block);
    if (goog.isArray(line)) {
      // Value blocks return tuples of code and operator order.
      // Top-level blocks don't care about operator order.
      line = line[0];
    }
    if (line) {
      if (block.outputConnection && this.scrubNakedValue) {
        // This block is a naked value.  Ask the language's code generator if
        // it wants to append a semicolon, or something.
        line = this.scrubNakedValue(line);
      }
      code.push(line);
    }
  }
  code = code.join(this.lineSeparator);  // Blank line between each section.
  code = this.finish(code);
  // Final scrubbing of whitespace.
  code = code.replace(/^\s+\n/, '');
  code = code.replace(/\n\s+$/, '\n');
  code = code.replace(/[ \t]+\n/g, '\n');
  return code;
};

