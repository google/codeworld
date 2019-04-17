(function (mod) {
    if (typeof exports === 'object' && typeof module === 'object') { // CommonJS
        mod(
            require('codemirror/lib/codemirror'),
            require('codemirror/addon/display/panel')
        );
    }
    else if (typeof define === 'function' && define.amd) { // AMD
        define([
            'codemirror/lib/codemirror',
            'codemirror/addon/display/panel'
        ], mod);
    }
    else { // Plain browser env
        mod(CodeMirror);
    }
})(function (CodeMirror) {
    "use strict";

    var PANEL_ELEMENT_CLASS = "CodeMirror-buttonsPanel";

    CodeMirror.defineOption("buttons", [], function (cm, value, old) {
        var panelNode = document.createElement("div");
        panelNode.className = PANEL_ELEMENT_CLASS;
        for (var i = 0, len = value.length; i < len; i++) {
            var button = createButton(cm, value[i]);
            panelNode.appendChild(button);
        }
        cm.addPanel(panelNode);
    });

    function createButton(cm, config) {
        var buttonNode;

        if (config.el) {
            if (typeof config.el === 'function') {
                buttonNode = config.el(cm);
            } else {
                buttonNode = config.el;
            }
        } else {
            buttonNode = document.createElement('div');
            buttonNode.innerHTML = config.label;

            buttonNode.addEventListener('click', function (e) {
                e.preventDefault();
                cm.focus();
                config.callback(cm);
            });

            if (config.class) {
                buttonNode.className = config.class;
            }

            if (config.title) {
                buttonNode.setAttribute('title', config.title);
            }
        }

        if (config.hotkey) {
            var map = {};
            map[config.hotkey] = config.callback;
            cm.addKeyMap(CodeMirror.normalizeKeyMap(map));
        }

        return buttonNode;
    }
});
