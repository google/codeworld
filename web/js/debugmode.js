
/*
 * Copyright 2017 The CodeWorld Authors. All rights reserved.
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
"use strict";

(function () {
    let available = false;
    let active = false;

    // Checked by parent.updateUI
    window.debugAvailable = false;
    window.debugActive = false;

    // These functions are provided by a debugmode-supported entrypoint when
    // calling initDebugMode
    //  debugGetNode :: { x :: Double, y :: Double } -> Int
    //   Returns the nodeId of the shape at the coordinate (x,y) of the canvas.
    //   Negative return indicates no shape at that point.
    let debugGetNode = null;
    //  debugSetActive :: Bool -> ()
    //   Indicates to the entry point when debugmode has been turned off and on
    let debugSetActive = null;
    //  debugGetPicture :: () -> Object
    //   Gets an object showing the current state of the Picture being drawn. Is
    //   only called directly after debugSetActive(true).
    let debugGetPicture = null;
    //  debugHighlightShape :: (Bool, Int) -> ()
    //   Indicates to the entry point should highlight or select a shape or tree
    //   of shapes. A true value indicates highlight (change color and bring to
    //   front) and false indicates select (change color and do not change
    //   position). A negative value indicates to stop highlighting or selecting.
    //   At most one shape may be highlighted and one shape selected at a time.
    let debugHighlightShape = null;

    let infobox = null;
    let cachedPic = null;
    let canvas = null;

    function openTreeDialog(id) {
        parent.openTreeDialog(id);
    }

    function closeTreeDialog() {
        parent.destroyTreeDialog();
    }

    // Globals

    function initDebugMode(getNode, setActive, getPicture, highlightShape) {
        debugGetNode = getNode;
        debugSetActive = setActive;
        debugGetPicture = getPicture;
        debugHighlightShape = highlightShape;

        if (!available) {
            canvas = document.getElementById("screen");

            canvas.addEventListener("mousemove", function (evt) {
                if (active) {
                    let nodeId = debugGetNode({
                        x: evt.clientX,
                        y: evt.clientY
                    });

                    debugHighlightShape(true, nodeId);
                }
            });

            canvas.addEventListener("mouseout", function (evt) {
                if (active) {
                    debugHighlightShape(true, -1);
                }
            });
            
            canvas.addEventListener("click", function (evt) {
                if (active) {
                    let nodeId = debugGetNode({
                        x: evt.clientX,
                        y: evt.clientY
                    });

                    if (nodeId >= 0) {
                        parent.openTreeDialog();
                    }
                }
            });

            available = true;
            window.debugAvailable = true;
        }

    }
    window.initDebugMode = initDebugMode;

    function startDebugMode() {
        if (!available) {
            throw new Error("Debug mode is not available.");
        }

        active = true;
        debugSetActive(true);
        cachedPic = debugGetPicture();

        parent.initTreeDialog(cachedPic, debugHighlightShape);

        window.debugActive = true;
        parent.updateUI()
    }
    window.startDebugMode = startDebugMode;

    function stopDebugMode() {
        if (active) {
            infobox.style.display = "none";
        }

        active = false;
        debugSetActive(false);
        cachedPic = null;

        parent.destroyTreeDialog();

        window.debugActive = false;
        parent.updateUI();
    }
    window.stopDebugMode = stopDebugMode;

    function toggleDebugMode() {
        if (active) {
            stopDebugMode();
        } else {
            startDebugMode();
        }
    }
    window.toggleDebugMode = toggleDebugMode;
})();
