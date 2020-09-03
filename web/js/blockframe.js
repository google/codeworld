/*
 * Copyright 2020 The CodeWorld Authors. All rights reserved.
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

let frameId = null;

function loadXml(data) {
  const workspace = Blockly.getMainWorkspace();
  if (!workspace) {
    setTimeout(() => {
      loadXml(data);
    }, 100);
    return; // Try again a little later
  }
  const xml = Blockly.Xml.textToDom(data);
  Blockly.Xml.domToWorkspace(xml, workspace);

  const metrics = workspace.getMetrics();
  const maxX = metrics.contentWidth + 50;
  const maxY = metrics.contentHeight + 50;
  frameId.setAttribute('width', maxX);
  frameId.setAttribute('height', maxY);
}

function setId(j_id_param) {
  frameId = j_id_param;
}

document.getElementById('blocklyDiv').addEventListener(
  'click',
  () => {
    if (window.parent) {
      const workspace = Blockly.getMainWorkspace();

      const xml = Blockly.Xml.workspaceToDom(workspace);
      const xml_text = Blockly.Xml.domToText(xml);

      window.parent.postMessage({ type: 'loadSample', code: xml_text }, '*');
    }
  },
  false
);

window.loadXml = loadXml;
window.setId = setId;
