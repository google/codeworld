"use strict";

const directoryTreeId = "#directoryTree";
const nodeTypes = {
  DIRECTORY: "directory",
  PROJECT: "project",
};

function clearSelectedNode() {
  const directoryTree = $(directoryTreeId);

  const state = directoryTree.tree("getState");
  state.selected_node = [];
  directoryTree.tree("setState", state);
}

function selectNode(node) {
  $(directoryTreeId).tree("selectNode", node);
}

let idCounter = 0;
function createNodeId(type, name) {
  return `jqtree-element__${type}__${name}__${idCounter++}}`;
}

function getSelectedNode() {
  const node = $(directoryTreeId).tree("getSelectedNode");

  return node ? node : null;
}

function isDirectory(node) {
  return node ? node.type === nodeTypes.DIRECTORY : false;
}

function isProject(node) {
  return node ? node.type === nodeTypes.PROJECT : false;
}

window.utils.directoryTree = {
  createNodeId,
  selectNode,
  clearSelectedNode,
  getSelectedNode,
  isDirectory,
  isProject,
  nodeTypes,
};
