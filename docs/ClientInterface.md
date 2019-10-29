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

### `{type: 'message', msgType: t, str: s}`

This indicates output can be displayed in an information console.  Programs
redirect stdout, stderr, and debug output to this.  The `type` attribute
is one of `log`, `warning`, or `error`.

### `{type: 'programStarted'}`

This indicates that the program has started running.

### `{type: 'showCanvas'}

This indicates that the program needs to be visible.  Until this message
is sent, the UI may hide the frame where the program is running, assuming
that it will not display anything.

### `{type: 'initDebug'}`

This indicates that the program is ready to handle messages from the
inspection debugger.

### `{type: 'openTreeDialog', nodeId: id, fullPic: pic}`

This indicates that the program has paused and is ready to communicate
with the inspection debugger.  The `nodeId` attribute gives the id of
the node in the expression tree that should be selected.  If present,
the `fullPic` attribute gives the entire expression tree.  (See the
later section for the structure of the expression tree.)

This may be called many times during a pause.  The first time, it
always includes the `fullPic` attribute.  After that, `fullPic` may be
omitted for performance, and should be assumed to be the same as the
last call.

### `{type: 'destroyTreeDialog'}`

This indicates that the program has stopped cooperating with the
inspection debugger, and continued running.  The UI may wish to stop
displaying debugger controls.

## Messages from UI to program

These are messages that are sent by the UI to the program, to
influence its behavior.

### `{type: 'canvasShown'}`

This informs the program that its frame is now visible.  It may want to
respond by continuing to run behavior that assumes it is visible.

### `{type: 'highlight', nodeId: id}`

This instructs the program to highlight in its output the portion of
the picture corresponding to the `nodeId` attribute.  If the `nodeId`
attribute is -1, then the highlight should be cancelled.

### `{type: 'toggleDebug'}`

This instructs the program to either enter or exit debug mode.  The
program will respond with either an `openTreeDialog` message containing
a `fullPic` attribute, or a `destroyTreeDialog` message, as described
above.
