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

    const point = Type.Lit('pair', [Type.Lit('Number'), Type.Lit('Number')]);

    const KeyPress = new Blockly.UserTypes.Product('KeyPress', [Type.Lit(
        'Text')]);
    Blockly.TypeInf.addUserDefinedConstructor('KeyPress', Type.fromList([
        Type.Lit('Text'), Type.Lit('Event')
    ]));

    const KeyRelease = new Blockly.UserTypes.Product('KeyRelease', [Type.Lit(
        'Text')]);
    Blockly.TypeInf.addUserDefinedConstructor('KeyRelease', Type.fromList([
        Type.Lit('Text'), Type.Lit('Event')
    ]));

    const PointerPress = new Blockly.UserTypes.Product('PointerPress', [point]);
    Blockly.TypeInf.addUserDefinedConstructor('PointerPress', Type.fromList(
        [point, Type.Lit('Event')]));

    const PointerRelease = new Blockly.UserTypes.Product('PointerRelease', [point]);
    Blockly.TypeInf.addUserDefinedConstructor('PointerRelease', Type.fromList(
        [point, Type.Lit('Event')]));

    const PointerMovement = new Blockly.UserTypes.Product('PointerMovement', [point]);
    Blockly.TypeInf.addUserDefinedConstructor('PointerMovement', Type.fromList(
        [point, Type.Lit('Event')]));

    const TextEntry = new Blockly.UserTypes.Product('TextEntry', [Type.Lit('Text')]);
    Blockly.TypeInf.addUserDefinedConstructor('TextEntry', Type.fromList(
        [Type.Lit('Text'), Type.Lit('Event')]));

    const TimePassing = new Blockly.UserTypes.Product('TimePassing', [Type.Lit('Number')]);
    Blockly.TypeInf.addUserDefinedConstructor('TimePassing', Type.fromList(
        [Type.Lit('Number'), Type.Lit('Event')]));

    const Event = new Blockly.UserTypes.Sum('Event', [KeyPress, KeyRelease,
        PointerPress, PointerRelease, PointerMovement, TextEntry, TimePassing
    ]);

    [Event].forEach(sum => {
        Blockly.UserTypes.generateConstructors_(sum, xmlList);
        Blockly.UserTypes.generateCase_(sum, xmlList);
    });
};

Blockly.cwEvent.eventFlyoutCategory = workspace => {
    const xmlList = [];
    Blockly.cwEvent.generateEventBuiltins(xmlList);
    return xmlList;
};
