# Client Interface

CodeWorld offers a UI that needs to communicate to running programs.
Programs run in a sandbox, so they are not allowed to directly invoke
code in the UI, or vice versa.  Instead, the program sends, and
registers listeners for,
[Window.postMessage](https://developer.mozilla.org/en-US/docs/Web/API/Window/postMessage)
style events that are used to communicate with the UI.

In principle, it should be possible to satisfy some other use cases by
providing a new implementation of this interface in a different UI.  Keep
in mind, though, that this is not a stable API, and may change in the
future.  If you have a good use case and need a stable API for it, please
contact codeworld-users@googlegroups.com to discuss your needs so we can
evaluate the best way to accomplish your goals.

The program will always post messages to window.parent.  Therefore, a UI
must run in the direct parent frame to listen for messages there.
Programs, in turn, always listen for messages in the frame they are
running in.

## Messages from program to UI

These are messages that are sent by the program to its parent frame, to be
handled by the UI:

### `{type: 'consoleOut', msgType: t, str: s}`

This indicates output can be displayed in an information console.  Programs
redirect stdout, stderr, and debug output to this.  The `type` attribute
is one of `log`, `warning`, or `error`.

### `{type: 'programStarted'}`

This indicates that the program has started running.

### `{type: 'showGraphics'}

This indicates that the program needs to be visible.  Until this message
is sent, the UI may hide the frame where the program is running, assuming
that it will not display anything.

### `{type: 'debugReady'}`

This indicates that the program is ready to handle messages from the
inspection debugger.

### `{type: 'debugActive', fullPic: pic}`

This indicates that the program has paused and is ready to communicate
with the inspection debugger.  The `fullPic` attribute gives the current
expression tree.  (See the later section for the structure of the
expression tree.)

### `{type: 'debugFinished'}`

This indicates that the program has stopped cooperating with the
inspection debugger, and continued running.  The UI may wish to stop
displaying debugger controls.

### `{type: 'nodeClicked', nodeId: id}`

This indicates that a node was clicked on the program output while
debugging was active.  The UI can respond as it chooses.

If id is -1, this indicates that the click wasn't on any node.

### `{type: 'nodeHovered', nodeId: id}`

This indicates that a node was hovered over with the pointer on the
program output while debugging was active.  The UI can respond as it
chooses.

If id is -1, this indicates that the pointer is no longer hovering
over any node.

## Messages from UI to program

These are messages that are sent by the UI to the program, to
influence its behavior.

### `{type: 'graphicsShown'}`

This informs the program that its frame is now visible.  It may want to
respond by continuing to run behavior that assumes it is visible.

### `{type: 'debugSelect', nodeId: id}`

This instructs the program to select the portion of the picture
corresponding to the `nodeId` attribute.  If the `nodeId` attribute is
-1, then the selection should be cancelled.

### `{type: 'debugHighlight', nodeId: id}`

This instructs the program to highlight in its output the portion of
the picture corresponding to the `nodeId` attribute.  If the `nodeId`
attribute is -1, then the highlight should be cancelled.

### `{type: 'startDebug'}`

This instructs the program to enter debug mode, freezing its normal
functions and interacting with the debugger.  The program will respond
by immediately sending a `debugActive` message back with the current
expression tree.

### `{type: 'stopDebug'}`

This instructs the program to leave debug mode and resume its normal
functions.  The program will acknowledge that it has done so by sending
a `debugFinished` message.
