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

goog.provide('Blockly.Init');

goog.require('Blockly');

goog.require('Blockly.Blocks.cwEvent');

// Initialize Blockly logic specific to CodeWorld

// Top level program blocks, only allow one of each
Blockly.Flyout.programBlockList = ["cwAnimationOf", "cwDrawingOf", "cwSimulationOf","cwInteractionOf" ];
// Automatically generate a type block for each of these
Blockly.UserTypes.builtinsStatic = ["Bool", "Number", "Color", "Picture", "Text"];
// Add a these blockTypes to the toolbox
Blockly.UserTypes.builtinsDynamic = ["type_list"];
// Enable the Event drawer
Blockly.Flyout.customDrawers["EVENT"] = Blockly.cwEvent.eventFlyoutCategory;
