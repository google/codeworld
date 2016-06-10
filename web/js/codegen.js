'use strict';

goog.provide('Blockly.FunBlocks');

goog.require('Blockly.Generator');

Blockly.FunBlocks = new Blockly.Generator('FunBlocks');

Blockly.FunBlocks.addReservedWords('Blockly,do,let,where,if,then,else,in');

Blockly.FunBlocks.ORDER_ATOMIC = 0;         // 0 "" ...
Blockly.FunBlocks.ORDER_MEMBER = 1;         // . []
Blockly.FunBlocks.ORDER_NEW = 1;            // new
Blockly.FunBlocks.ORDER_FUNCTION_CALL = 2;  // ()
Blockly.FunBlocks.ORDER_INCREMENT = 3;      // ++
Blockly.FunBlocks.ORDER_DECREMENT = 3;      // --
Blockly.FunBlocks.ORDER_LOGICAL_NOT = 4;    // !
Blockly.FunBlocks.ORDER_BITWISE_NOT = 4;    // ~
Blockly.FunBlocks.ORDER_UNARY_PLUS = 4;     // +
Blockly.FunBlocks.ORDER_UNARY_NEGATION = 4; // -
Blockly.FunBlocks.ORDER_TYPEOF = 4;         // typeof
Blockly.FunBlocks.ORDER_VOID = 4;           // void
Blockly.FunBlocks.ORDER_DELETE = 4;         // delete
Blockly.FunBlocks.ORDER_MULTIPLICATION = 5; // *
Blockly.FunBlocks.ORDER_DIVISION = 5;       // /
Blockly.FunBlocks.ORDER_MODULUS = 5;        // %
Blockly.FunBlocks.ORDER_ADDITION = 6;       // +
Blockly.FunBlocks.ORDER_SUBTRACTION = 6;    // -
Blockly.FunBlocks.ORDER_BITWISE_SHIFT = 7;  // << >> >>>
Blockly.FunBlocks.ORDER_RELATIONAL = 8;     // < <= > >=
Blockly.FunBlocks.ORDER_IN = 8;             // in
Blockly.FunBlocks.ORDER_INSTANCEOF = 8;     // instanceof
Blockly.FunBlocks.ORDER_EQUALITY = 9;       // == != === !==
Blockly.FunBlocks.ORDER_BITWISE_AND = 10;   // &
Blockly.FunBlocks.ORDER_BITWISE_XOR = 11;   // ^
Blockly.FunBlocks.ORDER_BITWISE_OR = 12;    // |
Blockly.FunBlocks.ORDER_LOGICAL_AND = 13;   // &&
Blockly.FunBlocks.ORDER_LOGICAL_OR = 14;    // ||
Blockly.FunBlocks.ORDER_CONDITIONAL = 15;   // ?:
Blockly.FunBlocks.ORDER_ASSIGNMENT = 16;    // = += -= *= /= %= <<= >>= ...
Blockly.FunBlocks.ORDER_COMMA = 17;         // ,
Blockly.FunBlocks.ORDER_NONE = 99;          // (...)

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



// Moved to Haskell land
/*
Blockly.FunBlocks['cw_text'] = function(block)
{
  var argument = block.getFieldValue('TEXT');
  var code = 'text("' + argument + '")';
  return code;
  // return [code, Blockly.FunBlocks.ORDER_MEMBER];
}



Blockly.FunBlocks['cw_number'] = function(block)
{
  var argument = block.getFieldValue('NUMBER');
  var code = argument;
  return [code, Blockly.FunBlocks.ORDER_MEMBER];
}


Blockly.FunBlocks['cw_drawingOf'] = function(block)
{
  var argument = Blockly.FunBlocks.valueToCode(block, 'VALUE', Blockly.FunBlocks.ORDER_NONE);

  var code = 'main = drawingOf(' + argument + ');';
  return [code, Blockly.FunBlocks.ORDER_NONE]
}


Blockly.FunBlocks['cw_combine'] = function(block) {
  var value_comb1 = Blockly.FunBlocks.valueToCode(block, 'Comb1', Blockly.FunBlocks.ORDER_ATOMIC);
  var value_comb2 = Blockly.FunBlocks.valueToCode(block, 'Comb2', Blockly.FunBlocks.ORDER_ATOMIC);

  var code = '(' + value_comb1 + ') &  (' + value_comb2 + ')';

  return [code, Blockly.FunBlocks.ORDER_NONE];
};


Blockly.FunBlocks['cw_translate'] = function(block) {
  var value_picture = Blockly.FunBlocks.valueToCode(block, 'PICTURE', Blockly.FunBlocks.ORDER_ATOMIC);
  var value_x = Blockly.FunBlocks.valueToCode(block, 'X', Blockly.FunBlocks.ORDER_ATOMIC);
  var value_name = Blockly.FunBlocks.valueToCode(block, 'Y', Blockly.FunBlocks.ORDER_ATOMIC);
  var code = 'translated (' + value_picture + ',' + value_x + ',' + value_name + ')';
  return [code, Blockly.FunBlocks.ORDER_NONE];
};
*/

