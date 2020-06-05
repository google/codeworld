const directoryTreeId = '#directoryTree'

function clearSelectedNode() {
  const directoryTree = $(directoryTreeId)

  const state = directoryTree.tree('getState')
  state.selected_node = []
  directoryTree.tree('setState', state)
}

function createNodeId(type, name) {
  return `jqtree-element__${type}__${name}`
}

function getCurrentProjectName() {
  const directoryTree = $(directoryTreeId)
  
  const state = directoryTree.tree('getState')
  const selectedNodeId = state.selected_node[0]

  if (!selectedNodeId) {
    return null
  }

  const { name } = directoryTree.tree('getNodeById', selectedNodeId)

  return name
}

window.utils.directoryTree = {
  clearSelectedNode,
  createNodeId,
  getCurrentProjectName
}
