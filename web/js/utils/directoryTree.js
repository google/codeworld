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

const directoryTreeId = '#directoryTree';
const nodeTypes = {
  DIRECTORY: 'directory',
  PROJECT: 'project',
};
const events = {
  SELECTION_CLEARED: 'selectionCleared',
};

function clearSelectedNode() {
  const directoryTree = $(directoryTreeId);

  const state = directoryTree.tree('getState');
  state.selected_node = [];
  directoryTree.tree('setState', state);

  directoryTree.trigger(events.SELECTION_CLEARED);
}

function selectNode(node) {
  $(directoryTreeId).tree('selectNode', node);
}

let idCounter = 0;

function createNodeId(type, name) {
  return `jqtree-element__${type}__${name}__${idCounter++}}`;
}

function getSelectedNode() {
  const node = $(directoryTreeId).tree('getSelectedNode');

  return node ? node : null;
}

function isDirectory(node) {
  return node ? node.type === nodeTypes.DIRECTORY : false;
}

function isProject(node) {
  return node ? node.type === nodeTypes.PROJECT : false;
}

export {
  createNodeId,
  selectNode,
  clearSelectedNode,
  getSelectedNode,
  isDirectory,
  isProject,
  nodeTypes,
  events,
};
