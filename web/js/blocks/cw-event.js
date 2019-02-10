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

goog.provide('Blockly.Blocks.cwEvent');

goog.require('Blockly.Blocks');

Blockly.cwEvent = () => {};
Blockly.cwEvent.generateEventBuiltins = xmlList => {

    let LeftButton = new Blockly.UserTypes.Product("LeftButton", []);
    Blockly.TypeInf.addUserDefinedConstructor("LeftButton", Type.fromList([Type.Lit("MouseButton")]));

    let RightButton = new Blockly.UserTypes.Product("RightButton", []);
    Blockly.TypeInf.addUserDefinedConstructor("RightButton", Type.fromList([Type.Lit("MouseButton")]));

    let MiddleButton = new Blockly.UserTypes.Product("MiddleButton", []);
    Blockly.TypeInf.addUserDefinedConstructor("MiddleButton", Type.fromList([Type.Lit("MouseButton")]));

    let MouseButton = new Blockly.UserTypes.Sum("MouseButton", [LeftButton, RightButton, MiddleButton]);


    let point = Type.Lit("pair", [Type.Lit("Number"), Type.Lit("Number")]);

    let KeyPress = new Blockly.UserTypes.Product("KeyPress", [Type.Lit("Text")]);
    Blockly.TypeInf.addUserDefinedConstructor("KeyPress", Type.fromList([Type.Lit("Text"), Type.Lit("Event")]));

    let KeyRelease = new Blockly.UserTypes.Product("KeyRelease", [Type.Lit("Text")]);
    Blockly.TypeInf.addUserDefinedConstructor("KeyRelease", Type.fromList([Type.Lit("Text"), Type.Lit("Event")]));

    let MousePress = new Blockly.UserTypes.Product("MousePress", [Type.Lit("MouseButton"), point]);
    Blockly.TypeInf.addUserDefinedConstructor("MousePress", Type.fromList([Type.Lit("MouseButton"), point, Type.Lit("Event")]));

    let MouseRelease = new Blockly.UserTypes.Product("MouseRelease", [Type.Lit("MouseButton"), point]);
    Blockly.TypeInf.addUserDefinedConstructor("MouseRelease", Type.fromList([Type.Lit("MouseButton"), point, Type.Lit("Event")]));

    let MouseMovement = new Blockly.UserTypes.Product("MouseMovement", [point]);
    Blockly.TypeInf.addUserDefinedConstructor("MouseMovement", Type.fromList([point, Type.Lit("Event")]));

    let Event = new Blockly.UserTypes.Sum("Event", [KeyPress, KeyRelease, MousePress, MouseRelease, MouseMovement]);

    [MouseButton, Event].forEach(sum => {
        Blockly.UserTypes.generateConstructors_(sum, xmlList);
        Blockly.UserTypes.generateCase_(sum, xmlList);
    });
};

Blockly.cwEvent.eventFlyoutCategory = workspace => {
    let xmlList = [];
    Blockly.cwEvent.generateEventBuiltins(xmlList);
    return xmlList;
};