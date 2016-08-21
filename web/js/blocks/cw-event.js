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

goog.provide('Blockly.Blocks.cwEvent');

goog.require('Blockly.Blocks');

Blockly.cwEvent = function(){};
Blockly.cwEvent.generateEventBuiltins = function(xmlList){

  var LeftButton = new Blockly.UserTypes.Product("LeftButton",[]);
  var RightButton = new Blockly.UserTypes.Product("RightButton",[]);
  var MiddleButton = new Blockly.UserTypes.Product("MiddleButton",[]);
  var MouseButton = new Blockly.UserTypes.Sum("MouseButton", [LeftButton,RightButton,MiddleButton]);


  var point = new Blockly.TypeExpr("pair",[new Blockly.TypeExpr("Number"), new Blockly.TypeExpr("Number")]);
  var KeyPress = new Blockly.UserTypes.Product("KeyPress", [new Blockly.TypeExpr("Text") ]);
  var KeyRelease = new Blockly.UserTypes.Product("KeyRelease", [new Blockly.TypeExpr("Text") ]);
  var MousePress = new Blockly.UserTypes.Product("MousePress", [new Blockly.TypeExpr("MouseButton") ]);
  var MouseRelease = new Blockly.UserTypes.Product("MouseRelease", [ new Blockly.TypeExpr("MouseButton")]);
  var MouseMovement = new Blockly.UserTypes.Product("MouseMovement", [point]);
  var Event = new Blockly.UserTypes.Sum("Event",[KeyPress, KeyRelease, MousePress, MouseRelease, MouseMovement]);


  var bs = [MouseButton, Event];


  bs.forEach(function(sum){
    Blockly.UserTypes.generateConstructors_(sum, xmlList);
    Blockly.UserTypes.generateCase_(sum, xmlList);
  });

};

Blockly.cwEvent.eventFlyoutCategory = function(workspace){

  var xmlList = [];

  Blockly.cwEvent.generateEventBuiltins(xmlList);

  return xmlList;
};


