/*!
 * JqTree 1.4.10
 *
 * Copyright 2019 Marco Braak
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
 *
 */ !(function (e) {
  const t = {};
  function o(n) {
    if (t[n]) return t[n].exports;
    const r = (t[n] = { i: n, l: !1, exports: {} });
    return e[n].call(r.exports, r, r.exports, o), (r.l = !0), r.exports;
  }
  (o.m = e),
    (o.c = t),
    (o.d = function (e, t, n) {
      o.o(e, t) || Object.defineProperty(e, t, { enumerable: !0, get: n });
    }),
    (o.r = function (e) {
      "undefined" !== typeof Symbol &&
        Symbol.toStringTag &&
        Object.defineProperty(e, Symbol.toStringTag, { value: "Module" }),
        Object.defineProperty(e, "__esModule", { value: !0 });
    }),
    (o.t = function (e, t) {
      if ((1 & t && (e = o(e)), 8 & t)) return e;
      if (4 & t && "object" === typeof e && e && e.__esModule) return e;
      const n = Object.create(null);
      if (
        (o.r(n),
        Object.defineProperty(n, "default", { enumerable: !0, value: e }),
        2 & t && "string" !== typeof e)
      ) {
        for (const r in e) {
          o.d(
            n,
            r,
            ((t) => {
              return e[t];
            }).bind(null, r)
          );
        }
      }
      return n;
    }),
    (o.n = function (e) {
      const t =
        e && e.__esModule
          ? function () {
              return e.default;
            }
          : function () {
              return e;
            };
      return o.d(t, "a", t), t;
    }),
    (o.o = function (e, t) {
      return Object.prototype.hasOwnProperty.call(e, t);
    }),
    (o.p = ""),
    o((o.s = 16));
})([
  function (e, t, o) {
    let n;
    (t.__esModule = !0),
      (function (e) {
        (e[(e.Before = 1)] = "Before"),
          (e[(e.After = 2)] = "After"),
          (e[(e.Inside = 3)] = "Inside"),
          (e[(e.None = 4)] = "None");
      })((n = t.Position || (t.Position = {})));
    const r = {
      before: n.Before,
      after: n.After,
      inside: n.Inside,
      none: n.None,
    };
    (t.getPositionName = function (e) {
      for (const t in r) if (r.hasOwnProperty(t) && r[t] === e) return t;
      return "";
    }),
      (t.getPosition = function (e) {
        return r[e];
      });
    const i = (function () {
      function e(t, o, n) {
        void 0 === o && (o = !1),
          void 0 === n && (n = e),
          (this.name = ""),
          this.setData(t),
          (this.children = []),
          (this.parent = null),
          o &&
            ((this.idMapping = {}), (this.tree = this), (this.nodeClass = n));
      }
      return (
        (e.prototype.setData = function (e) {
          const t = this,
            o = function (e) {
              null != e && (t.name = e);
            };
          if (e) {
            if ("object" !== typeof e) o(e);
            else {
              for (const n in e) {
                if (e.hasOwnProperty(n)) {
                  let r = e[n];
                  "label" === n ? o(r) : "children" !== n && (this[n] = r);
                }
              }
            }
          }
        }),
        (e.prototype.loadFromData = function (e) {
          this.removeChildren();
          for (let t = 0, o = e; t < o.length; t++) {
            const n = o[t],
              r = new this.tree.nodeClass(n);
            this.addChild(r),
              "object" === typeof n && n.children && r.loadFromData(n.children);
          }
        }),
        (e.prototype.addChild = function (e) {
          this.children.push(e), e._setParent(this);
        }),
        (e.prototype.addChildAtPosition = function (e, t) {
          this.children.splice(t, 0, e), e._setParent(this);
        }),
        (e.prototype.removeChild = function (e) {
          e.removeChildren(), this._removeChild(e);
        }),
        (e.prototype.getChildIndex = function (e) {
          return jQuery.inArray(e, this.children);
        }),
        (e.prototype.hasChildren = function () {
          return 0 !== this.children.length;
        }),
        (e.prototype.isFolder = function () {
          return this.hasChildren() || this.load_on_demand;
        }),
        (e.prototype.iterate = function (e) {
          var t = function (o, n) {
            if (o.children) {
              for (let r = 0, i = o.children; r < i.length; r++) {
                const s = i[r];
                e(s, n) && s.hasChildren() && t(s, n + 1);
              }
            }
          };
          t(this, 0);
        }),
        (e.prototype.moveNode = function (e, t, o) {
          e.parent &&
            !e.isParentOf(t) &&
            (e.parent._removeChild(e),
            o === n.After
              ? t.parent &&
                t.parent.addChildAtPosition(e, t.parent.getChildIndex(t) + 1)
              : o === n.Before
              ? t.parent &&
                t.parent.addChildAtPosition(e, t.parent.getChildIndex(t))
              : o === n.Inside && t.addChildAtPosition(e, 0));
        }),
        (e.prototype.getData = function (e) {
          function t(e) {
            return e.map((e) => {
              const o = {};
              for (const n in e) {
                if (
                  -1 === ["parent", "children", "element", "tree"].indexOf(n) &&
                  Object.prototype.hasOwnProperty.call(e, n)
                ) {
                  const r = e[n];
                  o[n] = r;
                }
              }
              return e.hasChildren() && (o.children = t(e.children)), o;
            });
          }
          return void 0 === e && (e = !1), t(e ? [this] : this.children);
        }),
        (e.prototype.getNodeByName = function (e) {
          return this.getNodeByCallback((t) => {
            return t.name === e;
          });
        }),
        (e.prototype.getNodeByCallback = function (e) {
          let t = null;
          return (
            this.iterate((o) => {
              return !e(o) || ((t = o), !1);
            }),
            t
          );
        }),
        (e.prototype.addAfter = function (e) {
          if (this.parent) {
            const t = new this.tree.nodeClass(e),
              o = this.parent.getChildIndex(this);
            return (
              this.parent.addChildAtPosition(t, o + 1),
              "object" === typeof e &&
                e.children &&
                e.children.length &&
                t.loadFromData(e.children),
              t
            );
          }
          return null;
        }),
        (e.prototype.addBefore = function (e) {
          if (this.parent) {
            const t = new this.tree.nodeClass(e),
              o = this.parent.getChildIndex(this);
            return (
              this.parent.addChildAtPosition(t, o),
              "object" === typeof e &&
                e.children &&
                e.children.length &&
                t.loadFromData(e.children),
              t
            );
          }
          return null;
        }),
        (e.prototype.addParent = function (e) {
          if (this.parent) {
            const t = new this.tree.nodeClass(e);
            t._setParent(this.tree);
            for (
              var o = this.parent, n = 0, r = o.children;
              n < r.length;
              n++
            ) {
              const i = r[n];
              t.addChild(i);
            }
            return (o.children = []), o.addChild(t), t;
          }
          return null;
        }),
        (e.prototype.remove = function () {
          this.parent && (this.parent.removeChild(this), (this.parent = null));
        }),
        (e.prototype.append = function (e) {
          const t = new this.tree.nodeClass(e);
          return (
            this.addChild(t),
            "object" === typeof e &&
              e.children &&
              e.children.length &&
              t.loadFromData(e.children),
            t
          );
        }),
        (e.prototype.prepend = function (e) {
          const t = new this.tree.nodeClass(e);
          return (
            this.addChildAtPosition(t, 0),
            "object" === typeof e &&
              e.children &&
              e.children.length &&
              t.loadFromData(e.children),
            t
          );
        }),
        (e.prototype.isParentOf = function (e) {
          for (let t = e.parent; t; ) {
            if (t === this) return !0;
            t = t.parent;
          }
          return !1;
        }),
        (e.prototype.getLevel = function () {
          for (var e = 0, t = this; t.parent; ) (e += 1), (t = t.parent);
          return e;
        }),
        (e.prototype.getNodeById = function (e) {
          return this.idMapping[e];
        }),
        (e.prototype.addNodeToIndex = function (e) {
          null != e.id && (this.idMapping[e.id] = e);
        }),
        (e.prototype.removeNodeFromIndex = function (e) {
          null != e.id && delete this.idMapping[e.id];
        }),
        (e.prototype.removeChildren = function () {
          const e = this;
          this.iterate((t) => {
            return e.tree.removeNodeFromIndex(t), !0;
          }),
            (this.children = []);
        }),
        (e.prototype.getPreviousSibling = function () {
          if (this.parent) {
            const e = this.parent.getChildIndex(this) - 1;
            return e >= 0 ? this.parent.children[e] : null;
          }
          return null;
        }),
        (e.prototype.getNextSibling = function () {
          if (this.parent) {
            const e = this.parent.getChildIndex(this) + 1;
            return e < this.parent.children.length
              ? this.parent.children[e]
              : null;
          }
          return null;
        }),
        (e.prototype.getNodesByProperty = function (e, t) {
          return this.filter((o) => {
            return o[e] === t;
          });
        }),
        (e.prototype.filter = function (e) {
          const t = [];
          return (
            this.iterate((o) => {
              return e(o) && t.push(o), !0;
            }),
            t
          );
        }),
        (e.prototype.getNextNode = function (e) {
          if (
            (void 0 === e && (e = !0), e && this.hasChildren() && this.is_open)
          ) {
            return this.children[0];
          }
          if (this.parent) {
            const t = this.getNextSibling();
            return t || this.parent.getNextNode(!1);
          }
          return null;
        }),
        (e.prototype.getPreviousNode = function () {
          if (this.parent) {
            const e = this.getPreviousSibling();
            return e
              ? e.hasChildren() && e.is_open
                ? e.getLastChild()
                : e
              : this.getParent();
          }
          return null;
        }),
        (e.prototype.getParent = function () {
          return this.parent && this.parent.parent ? this.parent : null;
        }),
        (e.prototype.getLastChild = function () {
          if (this.hasChildren()) {
            const e = this.children[this.children.length - 1];
            return e.hasChildren() && e.is_open ? e.getLastChild() : e;
          }
          return null;
        }),
        (e.prototype.initFromData = function (e) {
          let t,
            o = this,
            n = function (e) {
              for (let t = 0, n = e; t < n.length; t++) {
                const r = n[t],
                  i = new o.tree.nodeClass("");
                i.initFromData(r), o.addChild(i);
              }
            };
          (t = e), o.setData(t), t.children && n(t.children);
        }),
        (e.prototype._setParent = function (e) {
          (this.parent = e),
            (this.tree = e.tree),
            this.tree.addNodeToIndex(this);
        }),
        (e.prototype._removeChild = function (e) {
          this.children.splice(this.getChildIndex(e), 1),
            this.tree.removeNodeFromIndex(e);
        }),
        e
      );
    })();
    t.Node = i;
  },
  function (e, t, o) {
    (t.__esModule = !0),
      (t.isInt = function (e) {
        return "number" === typeof e && e % 1 == 0;
      }),
      (t.isFunction = function (e) {
        return "function" === typeof e;
      }),
      (t.htmlEscape = function (e) {
        return `${e}`
          .replace(/&/g, "&amp;")
          .replace(/</g, "&lt;")
          .replace(/>/g, "&gt;")
          .replace(/"/g, "&quot;")
          .replace(/'/g, "&#x27;")
          .replace(/\//g, "&#x2F;");
      }),
      (t.getBoolString = function (e) {
        return e ? "true" : "false";
      });
  },
  function (e, t) {
    e.exports = jQuery;
  },
  function (e, t, o) {
    t.__esModule = !0;
    const n = (function () {
      function e(e, t) {
        this.$el = jQuery(e);
        const o = this.constructor.defaults;
        this.options = jQuery.extend({}, o, t);
      }
      return (
        (e.register = function (t, o) {
          const n = function () {
            return `simple_widget_${o}`;
          };
          function r(t, o) {
            const n = jQuery.data(t, o);
            return n && n instanceof e ? n : null;
          }
          jQuery.fn[o] = function (o) {
            for (var i = [], s = 1; s < arguments.length; s++) {
              i[s - 1] = arguments[s];
            }
            if (void 0 === o || "object" === typeof o) {
              return (function (e, o) {
                for (let i = n(), s = 0, a = e.get(); s < a.length; s++) {
                  const l = a[s];
                  if (!r(l, i)) {
                    const d = new t(l, o);
                    jQuery.data(l, i) || jQuery.data(l, i, d), d._init();
                  }
                }
                return e;
              })(this, o);
            }
            if ("string" === typeof o && "_" !== o[0]) {
              const a = o;
              return "destroy" === a
                ? (function (e) {
                    for (let t = n(), o = 0, i = e.get(); o < i.length; o++) {
                      const s = i[o],
                        a = r(s, t);
                      a && a.destroy(), jQuery.removeData(s, t);
                    }
                  })(this)
                : "get_widget_class" === a
                ? t
                : (function (t, o, r) {
                    for (var i = null, s = 0, a = t.get(); s < a.length; s++) {
                      const l = a[s],
                        d = jQuery.data(l, n());
                      if (d && d instanceof e) {
                        const u = d[o];
                        u && "function" === typeof u && (i = u.apply(d, r));
                      }
                    }
                    return i;
                  })(this, a, i);
            }
          };
        }),
        (e.prototype.destroy = function () {
          this._deinit();
        }),
        (e.prototype._init = function () {}),
        (e.prototype._deinit = function () {}),
        (e.defaults = {}),
        e
      );
    })();
    t.default = n;
  },
  function (e, t, o) {
    let n,
      r =
        (this && this.__extends) ||
        ((n = function (e, t) {
          return (n =
            Object.setPrototypeOf ||
            ({ __proto__: [] } instanceof Array &&
              function (e, t) {
                e.__proto__ = t;
              }) ||
            function (e, t) {
              for (const o in t) t.hasOwnProperty(o) && (e[o] = t[o]);
            })(e, t);
        }),
        function (e, t) {
          function o() {
            this.constructor = e;
          }
          n(e, t),
            (e.prototype =
              null === t
                ? Object.create(t)
                : ((o.prototype = t.prototype), new o()));
        });
    t.__esModule = !0;
    const i = o(5),
      s = o(2),
      a = o(6),
      l = o(7),
      d = o(8),
      u = o(9),
      h = o(10),
      p = o(11),
      c = o(12),
      f = o(13),
      g = o(3),
      m = o(0),
      v = o(1),
      y = o(14),
      _ = "Node parameter is empty",
      N = (function (e) {
        function t() {
          const t = (null !== e && e.apply(this, arguments)) || this;
          return (
            (t._handleClick = function (e) {
              const o = t._getClickTarget(e.target);
              if (o) {
                if ("button" === o.type) {
                  t.toggle(o.node, t.options.slide),
                    e.preventDefault(),
                    e.stopPropagation();
                } else if ("label" === o.type) {
                  const n = o.node;
                  t
                    ._triggerEvent("tree.click", { node: n, click_event: e })
                    .isDefaultPrevented() || t._selectNode(n, !0);
                }
              }
            }),
            (t._handleDblclick = function (e) {
              const o = t._getClickTarget(e.target);
              o &&
                "label" === o.type &&
                t._triggerEvent("tree.dblclick", {
                  node: o.node,
                  click_event: e,
                });
            }),
            (t._handleContextmenu = function (e) {
              const o = s(e.target).closest("ul.jqtree-tree .jqtree-element");
              if (o.length) {
                const n = t._getNode(o);
                if (n) {
                  return (
                    e.preventDefault(),
                    e.stopPropagation(),
                    t._triggerEvent("tree.contextmenu", {
                      node: n,
                      click_event: e,
                    }),
                    !1
                  );
                }
              }
              return null;
            }),
            t
          );
        }
        return (
          r(t, e),
          (t.prototype.toggle = function (e, t) {
            if (!e) throw Error(_);
            const o = null == t ? this.options.slide : t;
            return (
              e.is_open ? this.closeNode(e, o) : this.openNode(e, o),
              this.element
            );
          }),
          (t.prototype.getTree = function () {
            return this.tree;
          }),
          (t.prototype.selectNode = function (e) {
            return this._selectNode(e, !1), this.element;
          }),
          (t.prototype.getSelectedNode = function () {
            return (
              Boolean(this.selectNodeHandler) &&
              this.selectNodeHandler.getSelectedNode()
            );
          }),
          (t.prototype.toJson = function () {
            return JSON.stringify(this.tree.getData());
          }),
          (t.prototype.loadData = function (e, t) {
            return this._loadData(e, t), this.element;
          }),
          (t.prototype.loadDataFromUrl = function (e, t, o) {
            return (
              "string" === typeof e
                ? this._loadDataFromUrl(e, t, o)
                : this._loadDataFromUrl(null, e, t),
              this.element
            );
          }),
          (t.prototype.reload = function (e) {
            return this._loadDataFromUrl(null, null, e), this.element;
          }),
          (t.prototype.getNodeById = function (e) {
            return this.tree.getNodeById(e);
          }),
          (t.prototype.getNodeByName = function (e) {
            return this.tree.getNodeByName(e);
          }),
          (t.prototype.getNodesByProperty = function (e, t) {
            return this.tree.getNodesByProperty(e, t);
          }),
          (t.prototype.getNodeByHtmlElement = function (e) {
            return this._getNode(s(e));
          }),
          (t.prototype.getNodeByCallback = function (e) {
            return this.tree.getNodeByCallback(e);
          }),
          (t.prototype.openNode = function (e, t, o) {
            const n = this;
            if (!e) throw Error(_);
            const r = (function () {
                let e, r;
                return (
                  v.isFunction(t) ? ((e = t), (r = null)) : ((r = t), (e = o)),
                  null == r && (r = n.options.slide),
                  [r, e]
                );
              })(),
              i = r[0],
              s = r[1];
            return this._openNode(e, i, s), this.element;
          }),
          (t.prototype.closeNode = function (e, t) {
            if (!e) throw Error(_);
            const o = null == t ? this.options.slide : t;
            return (
              e.isFolder() &&
                (new y.FolderElement(e, this).close(
                  o,
                  this.options.animationSpeed
                ),
                this._saveState()),
              this.element
            );
          }),
          (t.prototype.isDragging = function () {
            return Boolean(this.dndHandler) && this.dndHandler.isDragging;
          }),
          (t.prototype.refreshHitAreas = function () {
            return this.dndHandler && this.dndHandler.refresh(), this.element;
          }),
          (t.prototype.addNodeAfter = function (e, t) {
            const o = t.addAfter(e);
            return o && this._refreshElements(t.parent), o;
          }),
          (t.prototype.addNodeBefore = function (e, t) {
            if (!t) throw Error("Parameter is empty: existingNode");
            const o = t.addBefore(e);
            return o && this._refreshElements(t.parent), o;
          }),
          (t.prototype.addParentNode = function (e, t) {
            if (!t) throw Error("Parameter is empty: existingNode");
            const o = t.addParent(e);
            return o && this._refreshElements(o.parent), o;
          }),
          (t.prototype.removeNode = function (e) {
            if (!e) throw Error(_);
            const t = e;
            return (
              t.parent &&
                this.selectNodeHandler &&
                (this.selectNodeHandler.removeFromSelection(t, !0),
                t.remove(),
                this._refreshElements(t.parent)),
              this.element
            );
          }),
          (t.prototype.appendNode = function (e, t) {
            const o = t || this.tree,
              n = o.append(e);
            return this._refreshElements(o), n;
          }),
          (t.prototype.prependNode = function (e, t) {
            const o = t || this.tree,
              n = o.prepend(e);
            return this._refreshElements(o), n;
          }),
          (t.prototype.updateNode = function (e, t) {
            if (!e) throw Error(_);
            const o = t.id && t.id !== e.id;
            return (
              o && this.tree.removeNodeFromIndex(e),
              e.setData(t),
              o && this.tree.addNodeToIndex(e),
              "object" === typeof t &&
                t.children &&
                (e.removeChildren(),
                t.children.length && e.loadFromData(t.children)),
              this._refreshElements(e),
              this._selectCurrentNode(),
              this.element
            );
          }),
          (t.prototype.moveNode = function (e, t, o) {
            if (!e) throw Error(_);
            if (!t) throw Error("Parameter is empty: targetNode");
            const n = m.getPosition(o);
            return (
              this.tree.moveNode(e, t, n),
              this._refreshElements(null),
              this.element
            );
          }),
          (t.prototype.getStateFromStorage = function () {
            if (this.saveStateHandler) {
              return this.saveStateHandler.getStateFromStorage();
            }
          }),
          (t.prototype.addToSelection = function (e, t) {
            if (!e) throw Error(_);
            const o = e;
            return (
              this.selectNodeHandler &&
                (this.selectNodeHandler.addToSelection(o),
                this._getNodeElementForNode(o).select(t || !0),
                this._saveState()),
              this.element
            );
          }),
          (t.prototype.getSelectedNodes = function () {
            return this.selectNodeHandler
              ? this.selectNodeHandler.getSelectedNodes()
              : [];
          }),
          (t.prototype.isNodeSelected = function (e) {
            if (!e) throw Error(_);
            return (
              Boolean(this.selectNodeHandler) &&
              this.selectNodeHandler.isNodeSelected(e)
            );
          }),
          (t.prototype.removeFromSelection = function (e) {
            if (!e) throw Error(_);
            return (
              this.selectNodeHandler &&
                (this.selectNodeHandler.removeFromSelection(e),
                this._getNodeElementForNode(e).deselect(),
                this._saveState()),
              this.element
            );
          }),
          (t.prototype.scrollToNode = function (e) {
            if (!e) throw Error(_);
            if (this.scrollHandler) {
              const t = s(e.element).offset(),
                o = t ? t.top : 0,
                n = this.$el.offset(),
                r = o - (n ? n.top : 0);
              this.scrollHandler.scrollToY(r);
            }
            return this.element;
          }),
          (t.prototype.getState = function () {
            if (this.saveStateHandler) return this.saveStateHandler.getState();
          }),
          (t.prototype.setState = function (e) {
            return (
              this.saveStateHandler &&
                (this.saveStateHandler.setInitialState(e),
                this._refreshElements(null)),
              this.element
            );
          }),
          (t.prototype.setOption = function (e, t) {
            return (this.options[e] = t), this.element;
          }),
          (t.prototype.moveDown = function () {
            return this.keyHandler && this.keyHandler.moveDown(), this.element;
          }),
          (t.prototype.moveUp = function () {
            return this.keyHandler && this.keyHandler.moveUp(), this.element;
          }),
          (t.prototype.getVersion = function () {
            return i.default;
          }),
          (t.prototype.testGenerateHitAreas = function (e) {
            return this.dndHandler
              ? ((this.dndHandler.currentItem = this._getNodeElementForNode(e)),
                this.dndHandler.generateHitAreas(),
                this.dndHandler.hitAreas)
              : [];
          }),
          (t.prototype._triggerEvent = function (e, t) {
            const o = s.Event(e);
            return s.extend(o, t), this.element.trigger(o), o;
          }),
          (t.prototype._openNode = function (e, t, o) {
            const n = this;
            void 0 === t && (t = !0);
            const r = function (e, t, o) {
              new y.FolderElement(e, n).open(o, t, n.options.animationSpeed);
            };
            if (e.isFolder()) {
              if (e.load_on_demand) this._loadFolderOnDemand(e, t, o);
              else {
                for (let i = e.parent; i; ) {
                  i.parent && r(i, !1, null), (i = i.parent);
                }
                r(e, t, o), this._saveState();
              }
            }
          }),
          (t.prototype._refreshElements = function (e) {
            this.renderer.render(e), this._triggerEvent("tree.refresh");
          }),
          (t.prototype._getNodeElementForNode = function (e) {
            return e.isFolder()
              ? new y.FolderElement(e, this)
              : new y.NodeElement(e, this);
          }),
          (t.prototype._getNodeElement = function (e) {
            const t = this._getNode(e);
            return t ? this._getNodeElementForNode(t) : null;
          }),
          (t.prototype._containsElement = function (e) {
            const t = this._getNode(s(e));
            return null != t && t.tree === this.tree;
          }),
          (t.prototype._getScrollLeft = function () {
            return (
              (this.scrollHandler && this.scrollHandler.getScrollLeft()) || 0
            );
          }),
          (t.prototype._init = function () {
            e.prototype._init.call(this),
              (this.element = this.$el),
              (this.mouseDelay = 300),
              (this.isInitialized = !1),
              (this.options.rtl = this._getRtlOption()),
              null === this.options.closedIcon &&
                (this.options.closedIcon = this._getDefaultClosedIcon()),
              (this.renderer = new l.default(this)),
              (this.dataLoader = new d.default(this)),
              null != p.default
                ? (this.saveStateHandler = new p.default(this))
                : (this.options.saveState = !1),
              null != f.default &&
                (this.selectNodeHandler = new f.default(this)),
              null != a.DragAndDropHandler
                ? (this.dndHandler = new a.DragAndDropHandler(this))
                : (this.options.dragAndDrop = !1),
              null != c.default && (this.scrollHandler = new c.default(this)),
              null != u.default &&
                null != f.default &&
                (this.keyHandler = new u.default(this)),
              this._initData(),
              this.element.click(this._handleClick),
              this.element.dblclick(this._handleDblclick),
              this.options.useContextMenu &&
                this.element.on("contextmenu", this._handleContextmenu);
          }),
          (t.prototype._deinit = function () {
            this.element.empty(),
              this.element.off(),
              this.keyHandler && this.keyHandler.deinit(),
              (this.tree = new m.Node({}, !0)),
              e.prototype._deinit.call(this);
          }),
          (t.prototype._mouseCapture = function (e) {
            return (
              !(!this.options.dragAndDrop || !this.dndHandler) &&
              this.dndHandler.mouseCapture(e)
            );
          }),
          (t.prototype._mouseStart = function (e) {
            return (
              !(!this.options.dragAndDrop || !this.dndHandler) &&
              this.dndHandler.mouseStart(e)
            );
          }),
          (t.prototype._mouseDrag = function (e) {
            if (this.options.dragAndDrop && this.dndHandler) {
              const t = this.dndHandler.mouseDrag(e);
              return (
                this.scrollHandler && this.scrollHandler.checkScrolling(), t
              );
            }
            return !1;
          }),
          (t.prototype._mouseStop = function (e) {
            return (
              !(!this.options.dragAndDrop || !this.dndHandler) &&
              this.dndHandler.mouseStop(e)
            );
          }),
          (t.prototype._initData = function () {
            this.options.data
              ? this._loadData(this.options.data, null)
              : this._getDataUrlInfo(null)
              ? this._loadDataFromUrl(null, null, null)
              : this._loadData([], null);
          }),
          (t.prototype._getDataUrlInfo = function (e) {
            let t,
              o = this,
              n = this.options.dataUrl || this.element.data("url"),
              r = function (t) {
                if (e && e.id) {
                  var n = { node: e.id };
                  t.data = n;
                } else {
                  const r = o._getNodeIdToBeSelected();
                  if (r) {
                    n = { selected_node: r };
                    t.data = n;
                  }
                }
              };
            return "function" === typeof n
              ? n(e)
              : "string" === typeof n
              ? (r((t = { url: n })), t)
              : "object" === typeof n
              ? (r(n), n)
              : n;
          }),
          (t.prototype._getNodeIdToBeSelected = function () {
            return this.options.saveState && this.saveStateHandler
              ? this.saveStateHandler.getNodeIdToBeSelected()
              : null;
          }),
          (t.prototype._initTree = function (e) {
            const t = this,
              o = function () {
                t.isInitialized ||
                  ((t.isInitialized = !0), t._triggerEvent("tree.init"));
              };
            (this.tree = new this.options.nodeClass(
              null,
              !0,
              this.options.nodeClass
            )),
              this.selectNodeHandler && this.selectNodeHandler.clear(),
              this.tree.loadFromData(e);
            const n = this._setInitialState();
            this._refreshElements(null),
              n ? this._setInitialStateOnDemand(o) : o();
          }),
          (t.prototype._setInitialState = function () {
            let e = this,
              t = (function () {
                if (e.options.saveState && e.saveStateHandler) {
                  const t = e.saveStateHandler.getStateFromStorage();
                  return t
                    ? [!0, e.saveStateHandler.setInitialState(t)]
                    : [!1, !1];
                }
                return [!1, !1];
              })(),
              o = t[0],
              n = t[1];
            return (
              o ||
                (n = (function () {
                  if (!1 === e.options.autoOpen) return !1;
                  let t = e._getAutoOpenMaxLevel(),
                    o = !1;
                  return (
                    e.tree.iterate((e, n) => {
                      return e.load_on_demand
                        ? ((o = !0), !1)
                        : Boolean(e.hasChildren()) &&
                            ((e.is_open = !0), n !== t);
                    }),
                    o
                  );
                })()),
              n
            );
          }),
          (t.prototype._setInitialStateOnDemand = function (e) {
            let t,
              o,
              n,
              r = this;
            (function () {
              if (r.options.saveState && r.saveStateHandler) {
                const t = r.saveStateHandler.getStateFromStorage();
                return (
                  Boolean(t) &&
                  (r.saveStateHandler.setInitialStateOnDemand(t, e), !0)
                );
              }
              return !1;
            })() ||
              ((t = r._getAutoOpenMaxLevel()),
              (o = 0),
              (n = function () {
                r.tree.iterate((e, i) => {
                  return e.load_on_demand
                    ? (e.is_loading ||
                        (function (e) {
                          (o += 1),
                            r._openNode(e, !1, () => {
                              (o -= 1), n();
                            });
                        })(e),
                      !1)
                    : (r._openNode(e, !1, null), i !== t);
                }),
                  0 === o && e();
              })());
          }),
          (t.prototype._getAutoOpenMaxLevel = function () {
            return !0 === this.options.autoOpen
              ? -1
              : parseInt(this.options.autoOpen, 10);
          }),
          (t.prototype._getClickTarget = function (e) {
            const t = s(e),
              o = t.closest(".jqtree-toggler");
            if (o.length) {
              if ((n = this._getNode(o))) return { type: "button", node: n };
            } else {
              var n,
                r = t.closest(".jqtree-element");
              if (r.length) {
                if ((n = this._getNode(r))) return { type: "label", node: n };
              }
            }
            return null;
          }),
          (t.prototype._getNode = function (e) {
            const t = e.closest("li.jqtree_common");
            return 0 === t.length ? null : t.data("node");
          }),
          (t.prototype._saveState = function () {
            this.options.saveState &&
              this.saveStateHandler &&
              this.saveStateHandler.saveState();
          }),
          (t.prototype._selectCurrentNode = function () {
            const e = this.getSelectedNode();
            if (e) {
              const t = this._getNodeElementForNode(e);
              t && t.select(!0);
            }
          }),
          (t.prototype._deselectCurrentNode = function () {
            const e = this.getSelectedNode();
            e && this.removeFromSelection(e);
          }),
          (t.prototype._getDefaultClosedIcon = function () {
            return this.options.rtl ? "&#x25c0;" : "&#x25ba;";
          }),
          (t.prototype._getRtlOption = function () {
            if (null != this.options.rtl) return this.options.rtl;
            const e = this.element.data("rtl");
            return null != e && !1 !== e;
          }),
          (t.prototype._selectNode = function (e, t) {
            const o = this;
            if ((void 0 === t && (t = !1), this.selectNodeHandler)) {
              const n = function () {
                o.options.saveState &&
                  o.saveStateHandler &&
                  o.saveStateHandler.saveState();
              };
              if (!e) return this._deselectCurrentNode(), void n();
              if (
                o.options.onCanSelectNode
                  ? o.options.selectable && o.options.onCanSelectNode(e)
                  : o.options.selectable
              ) {
                let r,
                  i = e;
                if (this.selectNodeHandler.isNodeSelected(i)) {
                  t &&
                    (this._deselectCurrentNode(),
                    this._triggerEvent("tree.select", {
                      node: null,
                      previous_node: i,
                    }));
                } else {
                  const s = this.getSelectedNode();
                  this._deselectCurrentNode(),
                    this.addToSelection(i),
                    this._triggerEvent("tree.select", {
                      node: i,
                      deselected_node: s,
                    }),
                    (r = e.parent) &&
                      r.parent &&
                      !r.is_open &&
                      o.openNode(r, !1);
                }
                n();
              }
            }
          }),
          (t.prototype._loadData = function (e, t) {
            e &&
              (this._triggerEvent("tree.load_data", { tree_data: e }),
              t
                ? (this._deselectNodes(t), this._loadSubtree(e, t))
                : this._initTree(e),
              this.isDragging() &&
                this.dndHandler &&
                this.dndHandler.refresh());
          }),
          (t.prototype._deselectNodes = function (e) {
            if (this.selectNodeHandler) {
              for (
                let t = 0, o = this.selectNodeHandler.getSelectedNodesUnder(e);
                t < o.length;
                t++
              ) {
                const n = o[t];
                this.selectNodeHandler.removeFromSelection(n);
              }
            }
          }),
          (t.prototype._loadSubtree = function (e, t) {
            t.loadFromData(e),
              (t.load_on_demand = !1),
              (t.is_loading = !1),
              this._refreshElements(t);
          }),
          (t.prototype._loadDataFromUrl = function (e, t, o) {
            const n = e || this._getDataUrlInfo(t);
            this.dataLoader.loadFromUrl(n, t, o);
          }),
          (t.prototype._loadFolderOnDemand = function (e, t, o) {
            const n = this;
            void 0 === t && (t = !0),
              (e.is_loading = !0),
              this._loadDataFromUrl(null, e, () => {
                n._openNode(e, t, o);
              });
          }),
          (t.defaults = {
            animationSpeed: "fast",
            autoOpen: !1,
            saveState: !1,
            dragAndDrop: !1,
            selectable: !0,
            useContextMenu: !0,
            onCanSelectNode: null,
            onSetStateFromStorage: null,
            onGetStateFromStorage: null,
            onCreateLi: null,
            onIsMoveHandle: null,
            onCanMove: null,
            onCanMoveTo: null,
            onLoadFailed: null,
            autoEscape: !0,
            dataUrl: null,
            closedIcon: null,
            openedIcon: "&#x25bc;",
            slide: !0,
            nodeClass: m.Node,
            dataFilter: null,
            keyboardSupport: !0,
            openFolderDelay: 500,
            rtl: !1,
            onDragMove: null,
            onDragStop: null,
            buttonLeft: !0,
            onLoading: null,
            tabIndex: 0,
          }),
          t
        );
      })(h.default);
    g.default.register(N, "tree");
  },
  function (e, t, o) {
    t.__esModule = !0;
    t.default = "1.4.10";
  },
  function (e, t, o) {
    let n,
      r =
        (this && this.__extends) ||
        ((n = function (e, t) {
          return (n =
            Object.setPrototypeOf ||
            ({ __proto__: [] } instanceof Array &&
              function (e, t) {
                e.__proto__ = t;
              }) ||
            function (e, t) {
              for (const o in t) t.hasOwnProperty(o) && (e[o] = t[o]);
            })(e, t);
        }),
        function (e, t) {
          function o() {
            this.constructor = e;
          }
          n(e, t),
            (e.prototype =
              null === t
                ? Object.create(t)
                : ((o.prototype = t.prototype), new o()));
        });
    t.__esModule = !0;
    const i = o(2),
      s = o(0),
      a = o(1),
      l = (function () {
        function e(e) {
          (this.treeWidget = e),
            (this.hoveredArea = null),
            (this.hitAreas = []),
            (this.isDragging = !1),
            (this.currentItem = null),
            (this.positionInfo = null);
        }
        return (
          (e.prototype.mouseCapture = function (e) {
            const t = i(e.target);
            if (!this.mustCaptureElement(t)) return null;
            if (
              this.treeWidget.options.onIsMoveHandle &&
              !this.treeWidget.options.onIsMoveHandle(t)
            ) {
              return null;
            }
            let o = this.treeWidget._getNodeElement(t);
            return (
              o &&
                this.treeWidget.options.onCanMove &&
                (this.treeWidget.options.onCanMove(o.node) || (o = null)),
              (this.currentItem = o),
              null != this.currentItem
            );
          }),
          (e.prototype.generateHitAreas = function () {
            if (this.currentItem) {
              const e = new d(
                this.treeWidget.tree,
                this.currentItem.node,
                this.getTreeDimensions().bottom
              );
              this.hitAreas = e.generate();
            } else this.hitAreas = [];
          }),
          (e.prototype.mouseStart = function (e) {
            if (this.currentItem && void 0 !== e.pageX && void 0 !== e.pageY) {
              this.refresh();
              const t = i(e.target).offset(),
                o = t ? t.left : 0,
                n = t ? t.top : 0,
                r = this.currentItem.node,
                s = this.treeWidget.options.autoEscape
                  ? a.htmlEscape(r.name)
                  : r.name;
              return (
                (this.dragElement = new u(
                  s,
                  e.pageX - o,
                  e.pageY - n,
                  this.treeWidget.element
                )),
                (this.isDragging = !0),
                (this.positionInfo = e),
                this.currentItem.$element.addClass("jqtree-moving"),
                !0
              );
            }
            return !1;
          }),
          (e.prototype.mouseDrag = function (e) {
            if (
              this.currentItem &&
              this.dragElement &&
              void 0 !== e.pageX &&
              void 0 !== e.pageY
            ) {
              this.dragElement.move(e.pageX, e.pageY), (this.positionInfo = e);
              const t = this.findHoveredArea(e.pageX, e.pageY);
              return (
                this.canMoveToArea(t) && t
                  ? (t.node.isFolder() || this.stopOpenFolderTimer(),
                    this.hoveredArea !== t &&
                      ((this.hoveredArea = t),
                      this.mustOpenFolderTimer(t)
                        ? this.startOpenFolderTimer(t.node)
                        : this.stopOpenFolderTimer(),
                      this.updateDropHint()))
                  : (this.removeHover(),
                    this.removeDropHint(),
                    this.stopOpenFolderTimer()),
                t ||
                  (this.treeWidget.options.onDragMove &&
                    this.treeWidget.options.onDragMove(
                      this.currentItem.node,
                      e.originalEvent
                    )),
                !0
              );
            }
            return !1;
          }),
          (e.prototype.mouseStop = function (e) {
            this.moveItem(e),
              this.clear(),
              this.removeHover(),
              this.removeDropHint(),
              this.removeHitAreas();
            const t = this.currentItem;
            return (
              this.currentItem &&
                (this.currentItem.$element.removeClass("jqtree-moving"),
                (this.currentItem = null)),
              (this.isDragging = !1),
              (this.positionInfo = null),
              !this.hoveredArea &&
                t &&
                this.treeWidget.options.onDragStop &&
                this.treeWidget.options.onDragStop(t.node, e.originalEvent),
              !1
            );
          }),
          (e.prototype.refresh = function () {
            this.removeHitAreas(),
              this.currentItem &&
                (this.generateHitAreas(),
                (this.currentItem = this.treeWidget._getNodeElementForNode(
                  this.currentItem.node
                )),
                this.isDragging &&
                  this.currentItem.$element.addClass("jqtree-moving"));
          }),
          (e.prototype.mustCaptureElement = function (e) {
            return !e.is("input,select,textarea");
          }),
          (e.prototype.canMoveToArea = function (e) {
            if (e && this.currentItem) {
              if (this.treeWidget.options.onCanMoveTo) {
                const t = s.getPositionName(e.position);
                return this.treeWidget.options.onCanMoveTo(
                  this.currentItem.node,
                  e.node,
                  t
                );
              }
              return !0;
            }
            return !1;
          }),
          (e.prototype.removeHitAreas = function () {
            this.hitAreas = [];
          }),
          (e.prototype.clear = function () {
            this.dragElement &&
              (this.dragElement.remove(), (this.dragElement = null));
          }),
          (e.prototype.removeDropHint = function () {
            this.previousGhost && this.previousGhost.remove();
          }),
          (e.prototype.removeHover = function () {
            this.hoveredArea = null;
          }),
          (e.prototype.findHoveredArea = function (e, t) {
            const o = this.getTreeDimensions();
            if (e < o.left || t < o.top || e > o.right || t > o.bottom) {
              return null;
            }
            for (let n = 0, r = this.hitAreas.length; n < r; ) {
              const i = (n + r) >> 1,
                s = this.hitAreas[i];
              if (t < s.top) r = i;
              else {
                if (!(t > s.bottom)) return s;
                n = i + 1;
              }
            }
            return null;
          }),
          (e.prototype.mustOpenFolderTimer = function (e) {
            const t = e.node;
            return (
              t.isFolder() && !t.is_open && e.position === s.Position.Inside
            );
          }),
          (e.prototype.updateDropHint = function () {
            if (this.hoveredArea) {
              this.removeDropHint();
              const e = this.treeWidget._getNodeElementForNode(
                this.hoveredArea.node
              );
              this.previousGhost = e.addDropHint(this.hoveredArea.position);
            }
          }),
          (e.prototype.startOpenFolderTimer = function (e) {
            const t = this;
            this.stopOpenFolderTimer(),
              (this.openFolderTimer = window.setTimeout(() => {
                t.treeWidget._openNode(e, t.treeWidget.options.slide, () => {
                  t.refresh(), t.updateDropHint();
                });
              }, this.treeWidget.options.openFolderDelay));
          }),
          (e.prototype.stopOpenFolderTimer = function () {
            this.openFolderTimer &&
              (clearTimeout(this.openFolderTimer),
              (this.openFolderTimer = null));
          }),
          (e.prototype.moveItem = function (e) {
            const t = this;
            if (
              this.currentItem &&
              this.hoveredArea &&
              this.hoveredArea.position !== s.Position.None &&
              this.canMoveToArea(this.hoveredArea)
            ) {
              const o = this.currentItem.node,
                n = this.hoveredArea.node,
                r = this.hoveredArea.position,
                i = o.parent;
              r === s.Position.Inside && (this.hoveredArea.node.is_open = !0);
              const a = function () {
                t.treeWidget.tree.moveNode(o, n, r),
                  t.treeWidget.element.empty(),
                  t.treeWidget._refreshElements(null);
              };
              this.treeWidget
                ._triggerEvent("tree.move", {
                  move_info: {
                    moved_node: o,
                    target_node: n,
                    position: s.getPositionName(r),
                    previous_parent: i,
                    do_move: a,
                    original_event: e.originalEvent,
                  },
                })
                .isDefaultPrevented() || a();
            }
          }),
          (e.prototype.getTreeDimensions = function () {
            const e = this.treeWidget.element.offset();
            if (e) {
              const t = this.treeWidget.element,
                o = t.width() || 0,
                n = t.height() || 0,
                r = e.left + this.treeWidget._getScrollLeft();
              return {
                left: r,
                top: e.top,
                right: r + o,
                bottom: e.top + n + 16,
              };
            }
            return { left: 0, top: 0, right: 0, bottom: 0 };
          }),
          e
        );
      })();
    t.DragAndDropHandler = l;
    var d = (function (e) {
      function t(t, o, n) {
        const r = e.call(this, t) || this;
        return (r.currentNode = o), (r.treeBottom = n), r;
      }
      return (
        r(t, e),
        (t.prototype.generate = function () {
          return (
            (this.positions = []),
            (this.lastTop = 0),
            this.iterate(),
            this.generateHitAreas(this.positions)
          );
        }),
        (t.prototype.generateHitAreas = function (e) {
          for (var t = -1, o = [], n = [], r = 0, i = e; r < i.length; r++) {
            const s = i[r];
            s.top !== t &&
              o.length &&
              (o.length && this.generateHitAreasForGroup(n, o, t, s.top),
              (t = s.top),
              (o = [])),
              o.push(s);
          }
          return this.generateHitAreasForGroup(n, o, t, this.treeBottom), n;
        }),
        (t.prototype.handleOpenFolder = function (e, t) {
          return (
            e !== this.currentNode &&
            (e.children[0] !== this.currentNode &&
              this.addPosition(e, s.Position.Inside, this.getTop(t)),
            !0)
          );
        }),
        (t.prototype.handleClosedFolder = function (e, t, o) {
          const n = this.getTop(o);
          e === this.currentNode
            ? this.addPosition(e, s.Position.None, n)
            : (this.addPosition(e, s.Position.Inside, n),
              t !== this.currentNode &&
                this.addPosition(e, s.Position.After, n));
        }),
        (t.prototype.handleFirstNode = function (e) {
          e !== this.currentNode &&
            this.addPosition(e, s.Position.Before, this.getTop(i(e.element)));
        }),
        (t.prototype.handleAfterOpenFolder = function (e, t) {
          e === this.currentNode || t === this.currentNode
            ? this.addPosition(e, s.Position.None, this.lastTop)
            : this.addPosition(e, s.Position.After, this.lastTop);
        }),
        (t.prototype.handleNode = function (e, t, o) {
          const n = this.getTop(o);
          e === this.currentNode
            ? this.addPosition(e, s.Position.None, n)
            : this.addPosition(e, s.Position.Inside, n),
            t === this.currentNode || e === this.currentNode
              ? this.addPosition(e, s.Position.None, n)
              : this.addPosition(e, s.Position.After, n);
        }),
        (t.prototype.getTop = function (e) {
          const t = e.offset();
          return t ? t.top : 0;
        }),
        (t.prototype.addPosition = function (e, t, o) {
          const n = { top: o, bottom: 0, node: e, position: t };
          this.positions.push(n), (this.lastTop = o);
        }),
        (t.prototype.generateHitAreasForGroup = function (e, t, o, n) {
          for (
            let r = Math.min(t.length, 4),
              i = Math.round((n - o) / r),
              s = o,
              a = 0;
            a < r;

          ) {
            const l = t[a];
            e.push({
              top: s,
              bottom: s + i,
              node: l.node,
              position: l.position,
            }),
              (s += i),
              (a += 1);
          }
        }),
        t
      );
    })(
      (function () {
        function e(e) {
          this.tree = e;
        }
        return (
          (e.prototype.iterate = function () {
            var e = this,
              t = !0,
              o = function (n, r) {
                let s = (n.is_open || !n.element) && n.hasChildren(),
                  a = null;
                if (n.element) {
                  if (!(a = i(n.element)).is(":visible")) return;
                  t && (e.handleFirstNode(n), (t = !1)),
                    n.hasChildren()
                      ? n.is_open
                        ? e.handleOpenFolder(n, a) || (s = !1)
                        : e.handleClosedFolder(n, r, a)
                      : e.handleNode(n, r, a);
                }
                if (s) {
                  const l = n.children.length;
                  n.children.forEach((e, t) => {
                    o(n.children[t], t === l - 1 ? null : n.children[t + 1]);
                  }),
                    n.is_open && a && e.handleAfterOpenFolder(n, r);
                }
              };
            o(this.tree, null);
          }),
          e
        );
      })()
    );
    t.HitAreasGenerator = d;
    var u = (function () {
      function e(e, t, o, n) {
        (this.offsetX = t),
          (this.offsetY = o),
          (this.$element = i(
            `<span class="jqtree-title jqtree-dragging">${e}</span>`
          )),
          this.$element.css("position", "absolute"),
          n.append(this.$element);
      }
      return (
        (e.prototype.move = function (e, t) {
          this.$element.offset({
            left: e - this.offsetX,
            top: t - this.offsetY,
          });
        }),
        (e.prototype.remove = function () {
          this.$element.remove();
        }),
        e
      );
    })();
  },
  function (e, t, o) {
    t.__esModule = !0;
    const n = o(1),
      r = (function () {
        function e(e) {
          (this.treeWidget = e),
            (this.openedIconElement = this.createButtonElement(
              e.options.openedIcon
            )),
            (this.closedIconElement = this.createButtonElement(
              e.options.closedIcon
            ));
        }
        return (
          (e.prototype.render = function (e) {
            e && e.parent ? this.renderFromNode(e) : this.renderFromRoot();
          }),
          (e.prototype.renderFromRoot = function () {
            const e = this.treeWidget.element;
            e.empty(),
              this.createDomElements(
                e[0],
                this.treeWidget.tree.children,
                !0,
                1
              );
          }),
          (e.prototype.renderFromNode = function (e) {
            const t = jQuery(e.element),
              o = this.createLi(e, e.getLevel());
            this.attachNodeData(e, o),
              t.after(o),
              t.remove(),
              e.children &&
                this.createDomElements(o, e.children, !1, e.getLevel() + 1);
          }),
          (e.prototype.createDomElements = function (e, t, o, n) {
            const r = this.createUl(o);
            e.appendChild(r);
            for (let i = 0, s = t; i < s.length; i++) {
              const a = s[i],
                l = this.createLi(a, n);
              r.appendChild(l),
                this.attachNodeData(a, l),
                a.hasChildren() &&
                  this.createDomElements(l, a.children, !1, n + 1);
            }
          }),
          (e.prototype.attachNodeData = function (e, t) {
            (e.element = t), jQuery(t).data("node", e);
          }),
          (e.prototype.createUl = function (e) {
            let t, o;
            e
              ? ((t = "jqtree-tree"),
                (o = "tree"),
                this.treeWidget.options.rtl && (t += " jqtree-rtl"))
              : ((t = ""), (o = "group"));
            const n = document.createElement("ul");
            return (
              (n.className = `jqtree_common ${t}`), n.setAttribute("role", o), n
            );
          }),
          (e.prototype.createLi = function (e, t) {
            const o = Boolean(
                this.treeWidget.selectNodeHandler &&
                  this.treeWidget.selectNodeHandler.isNodeSelected(e)
              ),
              n = e.isFolder()
                ? this.createFolderLi(e, t, o)
                : this.createNodeLi(e, t, o);
            return (
              this.treeWidget.options.onCreateLi &&
                this.treeWidget.options.onCreateLi(e, jQuery(n), o),
              n
            );
          }),
          (e.prototype.createFolderLi = function (e, t, o) {
            const n = this.getButtonClasses(e),
              r = this.getFolderClasses(e, o),
              i = e.is_open ? this.openedIconElement : this.closedIconElement,
              s = document.createElement("li");
            (s.className = `jqtree_common ${r}`),
              s.setAttribute("role", "presentation");
            const a = document.createElement("div");
            (a.className = "jqtree-element jqtree_common"),
              a.setAttribute("role", "presentation"),
              s.appendChild(a);
            const l = document.createElement("a");
            return (
              (l.className = n),
              l.appendChild(i.cloneNode(!0)),
              l.setAttribute("role", "presentation"),
              l.setAttribute("aria-hidden", "true"),
              this.treeWidget.options.buttonLeft && a.appendChild(l),
              a.appendChild(this.createTitleSpan(e.name, t, o, e.is_open, !0)),
              this.treeWidget.options.buttonLeft || a.appendChild(l),
              s
            );
          }),
          (e.prototype.createNodeLi = function (e, t, o) {
            const n = ["jqtree_common"];
            o && n.push("jqtree-selected");
            const r = n.join(" "),
              i = document.createElement("li");
            (i.className = r), i.setAttribute("role", "presentation");
            const s = document.createElement("div");
            return (
              (s.className = "jqtree-element jqtree_common"),
              s.setAttribute("role", "presentation"),
              i.appendChild(s),
              s.appendChild(this.createTitleSpan(e.name, t, o, e.is_open, !1)),
              i
            );
          }),
          (e.prototype.createTitleSpan = function (e, t, o, r, i) {
            let s = document.createElement("span"),
              a = "jqtree-title jqtree_common";
            return (
              i && (a += " jqtree-title-folder"),
              (s.className = a),
              s.setAttribute("role", "treeitem"),
              s.setAttribute("aria-level", `${t}`),
              s.setAttribute("aria-selected", n.getBoolString(o)),
              s.setAttribute("aria-expanded", n.getBoolString(r)),
              o && s.setAttribute("tabindex", this.treeWidget.options.tabIndex),
              (s.innerHTML = this.escapeIfNecessary(e)),
              s
            );
          }),
          (e.prototype.getButtonClasses = function (e) {
            const t = ["jqtree-toggler", "jqtree_common"];
            return (
              e.is_open || t.push("jqtree-closed"),
              this.treeWidget.options.buttonLeft
                ? t.push("jqtree-toggler-left")
                : t.push("jqtree-toggler-right"),
              t.join(" ")
            );
          }),
          (e.prototype.getFolderClasses = function (e, t) {
            const o = ["jqtree-folder"];
            return (
              e.is_open || o.push("jqtree-closed"),
              t && o.push("jqtree-selected"),
              e.is_loading && o.push("jqtree-loading"),
              o.join(" ")
            );
          }),
          (e.prototype.escapeIfNecessary = function (e) {
            return this.treeWidget.options.autoEscape ? n.htmlEscape(e) : e;
          }),
          (e.prototype.createButtonElement = function (e) {
            if ("string" === typeof e) {
              const t = document.createElement("div");
              return (t.innerHTML = e), document.createTextNode(t.innerHTML);
            }
            return jQuery(e)[0];
          }),
          e
        );
      })();
    t.default = r;
  },
  function (e, t, o) {
    t.__esModule = !0;
    const n = (function () {
      function e(e) {
        this.treeWidget = e;
      }
      return (
        (e.prototype.loadFromUrl = function (e, t, o) {
          const n = this;
          if (e) {
            const r = this.getDomElement(t);
            this.addLoadingClass(r), this.notifyLoading(!0, t, r);
            const i = function () {
              n.removeLoadingClass(r), n.notifyLoading(!1, t, r);
            };
            this.submitRequest(
              e,
              (e) => {
                i(),
                  n.treeWidget.loadData(n.parseData(e), t),
                  o && "function" === typeof o && o();
              },
              (e) => {
                i();
                const t = n.treeWidget.options.onLoadFailed;
                t && t(e);
              }
            );
          }
        }),
        (e.prototype.addLoadingClass = function (e) {
          e && e.addClass("jqtree-loading");
        }),
        (e.prototype.removeLoadingClass = function (e) {
          e && e.removeClass("jqtree-loading");
        }),
        (e.prototype.getDomElement = function (e) {
          return e ? jQuery(e.element) : this.treeWidget.element;
        }),
        (e.prototype.notifyLoading = function (e, t, o) {
          const n = this.treeWidget.options.onLoading;
          n && n(e, t, o),
            this.treeWidget._triggerEvent("tree.loading_data", {
              isLoading: e,
              node: t,
              $el: o,
            });
        }),
        (e.prototype.submitRequest = function (e, t, o) {
          const n = jQuery.extend(
            { method: "GET" },
            "string" === typeof e ? { url: e } : e,
            { cache: !1, dataType: "json", success: t, error: o }
          );
          (n.method = n.method.toUpperCase()), jQuery.ajax(n);
        }),
        (e.prototype.parseData = function (e) {
          const t = this.treeWidget.options.dataFilter,
            o =
              e instanceof Array || "object" === typeof e
                ? e
                : null != e
                ? jQuery.parseJSON(e)
                : [];
          return t ? t(o) : o;
        }),
        e
      );
    })();
    t.default = n;
  },
  function (e, t, o) {
    t.__esModule = !0;
    const n = (function () {
      function e(t) {
        const o = this;
        (this.handleKeyDown = function (t) {
          if (!o.canHandleKeyboard()) return !0;
          switch (t.which) {
            case e.DOWN:
              return o.moveDown();
            case e.UP:
              return o.moveUp();
            case e.RIGHT:
              return o.moveRight();
            case e.LEFT:
              return o.moveLeft();
            default:
              return !0;
          }
        }),
          (this.treeWidget = t),
          t.options.keyboardSupport &&
            jQuery(document).on("keydown.jqtree", this.handleKeyDown);
      }
      return (
        (e.prototype.deinit = function () {
          jQuery(document).off("keydown.jqtree");
        }),
        (e.prototype.moveDown = function () {
          const e = this.treeWidget.getSelectedNode();
          return Boolean(e) && this.selectNode(e.getNextNode());
        }),
        (e.prototype.moveUp = function () {
          const e = this.treeWidget.getSelectedNode();
          return Boolean(e) && this.selectNode(e.getPreviousNode());
        }),
        (e.prototype.moveRight = function () {
          const e = this.treeWidget.getSelectedNode();
          return (
            !e ||
            !e.isFolder() ||
            (e.is_open
              ? this.selectNode(e.getNextNode())
              : (this.treeWidget.openNode(e), !1))
          );
        }),
        (e.prototype.moveLeft = function () {
          const e = this.treeWidget.getSelectedNode();
          return (
            !e ||
            (e.isFolder() && e.is_open
              ? (this.treeWidget.closeNode(e), !1)
              : this.selectNode(e.getParent()))
          );
        }),
        (e.prototype.selectNode = function (e) {
          return (
            !e ||
            (this.treeWidget.selectNode(e),
            this.treeWidget.scrollHandler &&
              !this.treeWidget.scrollHandler.isScrolledIntoView(
                jQuery(e.element).find(".jqtree-element")
              ) &&
              this.treeWidget.scrollToNode(e),
            !1)
          );
        }),
        (e.prototype.canHandleKeyboard = function () {
          return (
            this.treeWidget.options.keyboardSupport &&
            this.isFocusOnTree() &&
            null != this.treeWidget.getSelectedNode()
          );
        }),
        (e.prototype.isFocusOnTree = function () {
          const e = document.activeElement;
          return Boolean(
            e && "SPAN" === e.tagName && this.treeWidget._containsElement(e)
          );
        }),
        (e.LEFT = 37),
        (e.UP = 38),
        (e.RIGHT = 39),
        (e.DOWN = 40),
        e
      );
    })();
    t.default = n;
  },
  function (e, t, o) {
    let n,
      r =
        (this && this.__extends) ||
        ((n = function (e, t) {
          return (n =
            Object.setPrototypeOf ||
            ({ __proto__: [] } instanceof Array &&
              function (e, t) {
                e.__proto__ = t;
              }) ||
            function (e, t) {
              for (const o in t) t.hasOwnProperty(o) && (e[o] = t[o]);
            })(e, t);
        }),
        function (e, t) {
          function o() {
            this.constructor = e;
          }
          n(e, t),
            (e.prototype =
              null === t
                ? Object.create(t)
                : ((o.prototype = t.prototype), new o()));
        });
    t.__esModule = !0;
    const i = (function (e) {
      function t() {
        const t = (null !== e && e.apply(this, arguments)) || this;
        return (
          (t.mouseDown = function (e) {
            if (1 === e.which) {
              const o = t._handleMouseDown(t._getPositionInfo(e));
              return o && e.preventDefault(), o;
            }
          }),
          (t.mouseMove = function (e) {
            return t._handleMouseMove(e, t._getPositionInfo(e));
          }),
          (t.mouseUp = function (e) {
            return t._handleMouseUp(t._getPositionInfo(e));
          }),
          (t.touchStart = function (e) {
            const o = e.originalEvent;
            if (!(o.touches.length > 1)) {
              const n = o.changedTouches[0];
              return t._handleMouseDown(t._getPositionInfo(n));
            }
          }),
          (t.touchMove = function (e) {
            const o = e.originalEvent;
            if (!(o.touches.length > 1)) {
              const n = o.changedTouches[0];
              return t._handleMouseMove(e, t._getPositionInfo(n));
            }
          }),
          (t.touchEnd = function (e) {
            const o = e.originalEvent;
            if (!(o.touches.length > 1)) {
              const n = o.changedTouches[0];
              return t._handleMouseUp(t._getPositionInfo(n));
            }
          }),
          t
        );
      }
      return (
        r(t, e),
        (t.prototype.setMouseDelay = function (e) {
          this.mouseDelay = e;
        }),
        (t.prototype._init = function () {
          this.$el.on("mousedown.mousewidget", this.mouseDown),
            this.$el.on("touchstart.mousewidget", this.touchStart),
            (this.isMouseStarted = !1),
            (this.mouseDelay = 0),
            (this.mouseDelayTimer = null),
            (this.isMouseDelayMet = !0),
            (this.mouseDownInfo = null);
        }),
        (t.prototype._deinit = function () {
          this.$el.off("mousedown.mousewidget"),
            this.$el.off("touchstart.mousewidget");
          const e = jQuery(document);
          e.off("mousemove.mousewidget"), e.off("mouseup.mousewidget");
        }),
        (t.prototype._handleMouseDown = function (e) {
          if (
            (this.isMouseStarted && this._handleMouseUp(e),
            (this.mouseDownInfo = e),
            this._mouseCapture(e))
          ) {
            return this._handleStartMouse(), !0;
          }
        }),
        (t.prototype._handleStartMouse = function () {
          const e = jQuery(document);
          e.on("mousemove.mousewidget", this.mouseMove),
            e.on("touchmove.mousewidget", this.touchMove),
            e.on("mouseup.mousewidget", this.mouseUp),
            e.on("touchend.mousewidget", this.touchEnd),
            this.mouseDelay && this._startMouseDelayTimer();
        }),
        (t.prototype._startMouseDelayTimer = function () {
          const e = this;
          this.mouseDelayTimer && clearTimeout(this.mouseDelayTimer),
            (this.mouseDelayTimer = window.setTimeout(() => {
              e.isMouseDelayMet = !0;
            }, this.mouseDelay)),
            (this.isMouseDelayMet = !1);
        }),
        (t.prototype._handleMouseMove = function (e, t) {
          return this.isMouseStarted
            ? (this._mouseDrag(t), e.preventDefault())
            : !(!this.mouseDelay || this.isMouseDelayMet) ||
                (this.mouseDownInfo &&
                  (this.isMouseStarted =
                    !1 !== this._mouseStart(this.mouseDownInfo)),
                this.isMouseStarted
                  ? this._mouseDrag(t)
                  : this._handleMouseUp(t),
                !this.isMouseStarted);
        }),
        (t.prototype._getPositionInfo = function (e) {
          return {
            pageX: e.pageX,
            pageY: e.pageY,
            target: e.target,
            originalEvent: e,
          };
        }),
        (t.prototype._handleMouseUp = function (e) {
          const t = jQuery(document);
          t.off("mousemove.mousewidget"),
            t.off("touchmove.mousewidget"),
            t.off("mouseup.mousewidget"),
            t.off("touchend.mousewidget"),
            this.isMouseStarted &&
              ((this.isMouseStarted = !1), this._mouseStop(e));
        }),
        t
      );
    })(o(3).default);
    t.default = i;
  },
  function (e, t, o) {
    t.__esModule = !0;
    const n = o(1),
      r = (function () {
        function e(e) {
          this.treeWidget = e;
        }
        return (
          (e.prototype.saveState = function () {
            const e = JSON.stringify(this.getState());
            this.treeWidget.options.onSetStateFromStorage
              ? this.treeWidget.options.onSetStateFromStorage(e)
              : this.supportsLocalStorage() &&
                localStorage.setItem(this.getKeyName(), e);
          }),
          (e.prototype.getStateFromStorage = function () {
            const e = this._loadFromStorage();
            return e ? this._parseState(e) : null;
          }),
          (e.prototype.getState = function () {
            let e,
              t = this;
            return {
              open_nodes:
                ((e = []),
                t.treeWidget.tree.iterate((t) => {
                  return (
                    t.is_open && t.id && t.hasChildren() && e.push(t.id), !0
                  );
                }),
                e),
              selected_node: t.treeWidget.getSelectedNodes().map((e) => {
                return e.id;
              }),
            };
          }),
          (e.prototype.setInitialState = function (e) {
            if (e) {
              let t = !1;
              return (
                e.open_nodes && (t = this._openInitialNodes(e.open_nodes)),
                e.selected_node &&
                  (this._resetSelection(),
                  this._selectInitialNodes(e.selected_node)),
                t
              );
            }
            return !1;
          }),
          (e.prototype.setInitialStateOnDemand = function (e, t) {
            e
              ? this._setInitialStateOnDemand(e.open_nodes, e.selected_node, t)
              : t();
          }),
          (e.prototype.getNodeIdToBeSelected = function () {
            const e = this.getStateFromStorage();
            return e && e.selected_node ? e.selected_node[0] : null;
          }),
          (e.prototype._parseState = function (e) {
            const t = jQuery.parseJSON(e);
            return (
              t &&
                t.selected_node &&
                n.isInt(t.selected_node) &&
                (t.selected_node = [t.selected_node]),
              t
            );
          }),
          (e.prototype._loadFromStorage = function () {
            return this.treeWidget.options.onGetStateFromStorage
              ? this.treeWidget.options.onGetStateFromStorage()
              : this.supportsLocalStorage()
              ? localStorage.getItem(this.getKeyName())
              : void 0;
          }),
          (e.prototype._openInitialNodes = function (e) {
            for (var t = !1, o = 0, n = e; o < n.length; o++) {
              const r = n[o],
                i = this.treeWidget.getNodeById(r);
              i && (i.load_on_demand ? (t = !0) : (i.is_open = !0));
            }
            return t;
          }),
          (e.prototype._selectInitialNodes = function (e) {
            for (var t = 0, o = 0, n = e; o < n.length; o++) {
              const r = n[o],
                i = this.treeWidget.getNodeById(r);
              i &&
                ((t += 1),
                this.treeWidget.selectNodeHandler &&
                  this.treeWidget.selectNodeHandler.addToSelection(i));
            }
            return 0 !== t;
          }),
          (e.prototype._resetSelection = function () {
            const e = this.treeWidget.selectNodeHandler;
            e &&
              e.getSelectedNodes().forEach((t) => {
                e.removeFromSelection(t);
              });
          }),
          (e.prototype._setInitialStateOnDemand = function (e, t, o) {
            var n = this,
              r = 0,
              i = e,
              s = function () {
                for (var e = [], s = 0, l = i; s < l.length; s++) {
                  const d = l[s],
                    u = n.treeWidget.getNodeById(d);
                  u
                    ? u.is_loading ||
                      (u.load_on_demand
                        ? a(u)
                        : n.treeWidget._openNode(u, !1, null))
                    : e.push(d);
                }
                (i = e),
                  n._selectInitialNodes(t) &&
                    n.treeWidget._refreshElements(null),
                  0 === r && o();
              },
              a = function (e) {
                (r += 1),
                  n.treeWidget._openNode(e, !1, () => {
                    (r -= 1), s();
                  });
              };
            s();
          }),
          (e.prototype.getKeyName = function () {
            return "string" === typeof this.treeWidget.options.saveState
              ? this.treeWidget.options.saveState
              : "tree";
          }),
          (e.prototype.supportsLocalStorage = function () {
            return (
              null == this._supportsLocalStorage &&
                (this._supportsLocalStorage = (function () {
                  if (null == localStorage) return !1;
                  try {
                    const e = "_storage_test";
                    sessionStorage.setItem(e, "value"),
                      sessionStorage.removeItem(e);
                  } catch (e) {
                    return !1;
                  }
                  return !0;
                })()),
              this._supportsLocalStorage
            );
          }),
          e
        );
      })();
    t.default = r;
  },
  function (e, t, o) {
    t.__esModule = !0;
    const n = (function () {
      function e(e) {
        (this.treeWidget = e),
          (this.previousTop = -1),
          (this.isInitialized = !1);
      }
      return (
        (e.prototype.checkScrolling = function () {
          this.ensureInit(),
            this.checkVerticalScrolling(),
            this.checkHorizontalScrolling();
        }),
        (e.prototype.scrollToY = function (e) {
          if ((this.ensureInit(), this.$scrollParent)) {
            this.$scrollParent[0].scrollTop = e;
          } else {
            const t = this.treeWidget.$el.offset(),
              o = t ? t.top : 0;
            jQuery(document).scrollTop(e + o);
          }
        }),
        (e.prototype.isScrolledIntoView = function (e) {
          let t, o, n, r;
          this.ensureInit();
          let i,
            s = e.height() || 0;
          this.$scrollParent
            ? ((r = 0),
              (o = this.$scrollParent.height() || 0),
              (t =
                (n = ((i = e.offset()) ? i.top : 0) - this.scrollParentTop) +
                s))
            : ((o =
                (r = jQuery(window).scrollTop() || 0) +
                (jQuery(window).height() || 0)),
              (t = (n = (i = e.offset()) ? i.top : 0) + s));
          return t <= o && n >= r;
        }),
        (e.prototype.getScrollLeft = function () {
          return (this.$scrollParent && this.$scrollParent.scrollLeft()) || 0;
        }),
        (e.prototype.initScrollParent = function () {
          const e = this,
            t = function () {
              (e.scrollParentTop = 0), (e.$scrollParent = null);
            };
          "fixed" === this.treeWidget.$el.css("position") && t();
          const o = (function () {
            const t = ["overflow", "overflow-y"],
              o = function (e) {
                for (let o = 0, n = t; o < n.length; o++) {
                  const r = n[o],
                    i = e.css(r);
                  if ("auto" === i || "scroll" === i) return !0;
                }
                return !1;
              };
            if (o(e.treeWidget.$el)) return e.treeWidget.$el;
            for (
              let n = 0, r = e.treeWidget.$el.parents().get();
              n < r.length;
              n++
            ) {
              const i = r[n],
                s = jQuery(i);
              if (o(s)) return s;
            }
            return null;
          })();
          if (o && o.length && "HTML" !== o[0].tagName) {
            this.$scrollParent = o;
            const n = this.$scrollParent.offset();
            this.scrollParentTop = n ? n.top : 0;
          } else t();
          this.isInitialized = !0;
        }),
        (e.prototype.ensureInit = function () {
          this.isInitialized || this.initScrollParent();
        }),
        (e.prototype.handleVerticalScrollingWithScrollParent = function (e) {
          const t = this.$scrollParent && this.$scrollParent[0];
          t &&
            (this.scrollParentTop + t.offsetHeight - e.bottom < 20
              ? ((t.scrollTop += 20),
                this.treeWidget.refreshHitAreas(),
                (this.previousTop = -1))
              : e.top - this.scrollParentTop < 20 &&
                ((t.scrollTop -= 20),
                this.treeWidget.refreshHitAreas(),
                (this.previousTop = -1)));
        }),
        (e.prototype.handleVerticalScrollingWithDocument = function (e) {
          const t = jQuery(document).scrollTop() || 0;
          e.top - t < 20
            ? jQuery(document).scrollTop(t - 20)
            : (jQuery(window).height() || 0) - (e.bottom - t) < 20 &&
              jQuery(document).scrollTop(t + 20);
        }),
        (e.prototype.checkVerticalScrolling = function () {
          const e =
            this.treeWidget.dndHandler &&
            this.treeWidget.dndHandler.hoveredArea;
          e &&
            e.top !== this.previousTop &&
            ((this.previousTop = e.top),
            this.$scrollParent
              ? this.handleVerticalScrollingWithScrollParent(e)
              : this.handleVerticalScrollingWithDocument(e));
        }),
        (e.prototype.checkHorizontalScrolling = function () {
          const e =
            this.treeWidget.dndHandler &&
            this.treeWidget.dndHandler.positionInfo;
          e &&
            (this.$scrollParent
              ? this.handleHorizontalScrollingWithParent(e)
              : this.handleHorizontalScrollingWithDocument(e));
        }),
        (e.prototype.handleHorizontalScrollingWithParent = function (e) {
          if (void 0 !== e.pageX && void 0 !== e.pageY) {
            const t = this.$scrollParent,
              o = t && t.offset();
            if (t && o) {
              const n = t[0],
                r = n.scrollLeft + n.clientWidth < n.scrollWidth,
                i = n.scrollLeft > 0,
                s = o.left + n.clientWidth,
                a = o.left,
                l = e.pageX > s - 20,
                d = e.pageX < a + 20;
              l && r
                ? (n.scrollLeft = Math.min(n.scrollLeft + 20, n.scrollWidth))
                : d && i && (n.scrollLeft = Math.max(n.scrollLeft - 20, 0));
            }
          }
        }),
        (e.prototype.handleHorizontalScrollingWithDocument = function (e) {
          if (void 0 !== e.pageX && void 0 !== e.pageY) {
            const t = jQuery(document),
              o = t.scrollLeft() || 0,
              n = jQuery(window).width() || 0,
              r = o > 0,
              i = e.pageX > n - 20,
              s = e.pageX - o < 20;
            i
              ? t.scrollLeft(o + 20)
              : s && r && t.scrollLeft(Math.max(o - 20, 0));
          }
        }),
        e
      );
    })();
    t.default = n;
  },
  function (e, t, o) {
    t.__esModule = !0;
    const n = (function () {
      function e(e) {
        (this.treeWidget = e), this.clear();
      }
      return (
        (e.prototype.getSelectedNode = function () {
          const e = this.getSelectedNodes();
          return Boolean(e.length) && e[0];
        }),
        (e.prototype.getSelectedNodes = function () {
          if (this.selectedSingleNode) return [this.selectedSingleNode];
          const e = [];
          for (const t in this.selectedNodes) {
            if (this.selectedNodes.hasOwnProperty(t)) {
              const o = this.treeWidget.getNodeById(t);
              o && e.push(o);
            }
          }
          return e;
        }),
        (e.prototype.getSelectedNodesUnder = function (e) {
          if (this.selectedSingleNode) {
            return e.isParentOf(this.selectedSingleNode)
              ? [this.selectedSingleNode]
              : [];
          }
          const t = [];
          for (const o in this.selectedNodes) {
            if (this.selectedNodes.hasOwnProperty(o)) {
              const n = this.treeWidget.getNodeById(o);
              n && e.isParentOf(n) && t.push(n);
            }
          }
          return t;
        }),
        (e.prototype.isNodeSelected = function (e) {
          return (
            Boolean(e) &&
            (null != e.id
              ? Boolean(this.selectedNodes[e.id])
              : Boolean(this.selectedSingleNode) &&
                this.selectedSingleNode.element === e.element)
          );
        }),
        (e.prototype.clear = function () {
          (this.selectedNodes = {}), (this.selectedSingleNode = null);
        }),
        (e.prototype.removeFromSelection = function (e, t) {
          const o = this;
          void 0 === t && (t = !1),
            null == e.id
              ? this.selectedSingleNode &&
                e.element === this.selectedSingleNode.element &&
                (this.selectedSingleNode = null)
              : (delete this.selectedNodes[e.id],
                t &&
                  e.iterate(() => {
                    return delete o.selectedNodes[e.id], !0;
                  }));
        }),
        (e.prototype.addToSelection = function (e) {
          null != e.id
            ? (this.selectedNodes[e.id] = !0)
            : (this.selectedSingleNode = e);
        }),
        e
      );
    })();
    t.default = n;
  },
  function (e, t, o) {
    let n,
      r =
        (this && this.__extends) ||
        ((n = function (e, t) {
          return (n =
            Object.setPrototypeOf ||
            ({ __proto__: [] } instanceof Array &&
              function (e, t) {
                e.__proto__ = t;
              }) ||
            function (e, t) {
              for (const o in t) t.hasOwnProperty(o) && (e[o] = t[o]);
            })(e, t);
        }),
        function (e, t) {
          function o() {
            this.constructor = e;
          }
          n(e, t),
            (e.prototype =
              null === t
                ? Object.create(t)
                : ((o.prototype = t.prototype), new o()));
        });
    t.__esModule = !0;
    const i = o(0),
      s = (function () {
        function e(e, t) {
          this.init(e, t);
        }
        return (
          (e.prototype.init = function (e, t) {
            (this.node = e),
              (this.treeWidget = t),
              e.element || (e.element = this.treeWidget.element.get(0)),
              (this.$element = jQuery(e.element));
          }),
          (e.prototype.addDropHint = function (e) {
            return this.mustShowBorderDropHint(e)
              ? new l(this.$element, this.treeWidget._getScrollLeft())
              : new d(this.node, this.$element, e);
          }),
          (e.prototype.select = function (e) {
            const t = this.getLi();
            t.addClass("jqtree-selected"), t.attr("aria-selected", "true");
            const o = this.getSpan();
            o.attr("tabindex", this.treeWidget.options.tabIndex),
              e && o.focus();
          }),
          (e.prototype.deselect = function () {
            const e = this.getLi();
            e.removeClass("jqtree-selected"), e.attr("aria-selected", "false");
            const t = this.getSpan();
            t.removeAttr("tabindex"), t.blur();
          }),
          (e.prototype.getUl = function () {
            return this.$element.children("ul:first");
          }),
          (e.prototype.getSpan = function () {
            return this.$element
              .children(".jqtree-element")
              .find("span.jqtree-title");
          }),
          (e.prototype.getLi = function () {
            return this.$element;
          }),
          (e.prototype.mustShowBorderDropHint = function (e) {
            return e === i.Position.Inside;
          }),
          e
        );
      })();
    t.NodeElement = s;
    const a = (function (e) {
      function t() {
        return (null !== e && e.apply(this, arguments)) || this;
      }
      return (
        r(t, e),
        (t.prototype.open = function (e, t, o) {
          const n = this;
          if (
            (void 0 === t && (t = !0),
            void 0 === o && (o = "fast"),
            !this.node.is_open)
          ) {
            this.node.is_open = !0;
            const r = this.getButton();
            r.removeClass("jqtree-closed"), r.html("");
            const i = r.get(0);
            if (i) {
              const s = this.treeWidget.renderer.openedIconElement.cloneNode(
                !0
              );
              i.appendChild(s);
            }
            const a = function () {
              n.getLi().removeClass("jqtree-closed"),
                n.getSpan().attr("aria-expanded", "true"),
                e && e(n.node),
                n.treeWidget._triggerEvent("tree.open", { node: n.node });
            };
            t ? this.getUl().slideDown(o, a) : (this.getUl().show(), a());
          }
        }),
        (t.prototype.close = function (e, t) {
          const o = this;
          if (
            (void 0 === e && (e = !0),
            void 0 === t && (t = "fast"),
            this.node.is_open)
          ) {
            this.node.is_open = !1;
            const n = this.getButton();
            n.addClass("jqtree-closed"), n.html("");
            const r = n.get(0);
            if (r) {
              const i = this.treeWidget.renderer.closedIconElement.cloneNode(
                !0
              );
              r.appendChild(i);
            }
            const s = function () {
              o.getLi().addClass("jqtree-closed"),
                o.getSpan().attr("aria-expanded", "false"),
                o.treeWidget._triggerEvent("tree.close", { node: o.node });
            };
            e ? this.getUl().slideUp(t, s) : (this.getUl().hide(), s());
          }
        }),
        (t.prototype.mustShowBorderDropHint = function (e) {
          return !this.node.is_open && e === i.Position.Inside;
        }),
        (t.prototype.getButton = function () {
          return this.$element
            .children(".jqtree-element")
            .find("a.jqtree-toggler");
        }),
        t
      );
    })(s);
    t.FolderElement = a;
    var l = (function () {
      function e(e, t) {
        const o = e.children(".jqtree-element"),
          n = e.width() || 0,
          r = Math.max(n + t - 4, 0),
          i = o.outerHeight() || 0,
          s = Math.max(i - 4, 0);
        (this.$hint = jQuery('<span class="jqtree-border"></span>')),
          o.append(this.$hint),
          this.$hint.css({ width: r, height: s });
      }
      return (
        (e.prototype.remove = function () {
          this.$hint.remove();
        }),
        e
      );
    })();
    t.BorderDropHint = l;
    var d = (function () {
      function e(e, t, o) {
        (this.$element = t),
          (this.node = e),
          (this.$ghost = jQuery(
            '<li class="jqtree_common jqtree-ghost"><span class="jqtree_common jqtree-circle"></span>\n            <span class="jqtree_common jqtree-line"></span></li>'
          )),
          o === i.Position.After
            ? this.moveAfter()
            : o === i.Position.Before
            ? this.moveBefore()
            : o === i.Position.Inside &&
              (e.isFolder() && e.is_open
                ? this.moveInsideOpenFolder()
                : this.moveInside());
      }
      return (
        (e.prototype.remove = function () {
          this.$ghost.remove();
        }),
        (e.prototype.moveAfter = function () {
          this.$element.after(this.$ghost);
        }),
        (e.prototype.moveBefore = function () {
          this.$element.before(this.$ghost);
        }),
        (e.prototype.moveInsideOpenFolder = function () {
          jQuery(this.node.children[0].element).before(this.$ghost);
        }),
        (e.prototype.moveInside = function () {
          this.$element.after(this.$ghost),
            this.$ghost.addClass("jqtree-inside");
        }),
        e
      );
    })();
  },
  ,
  function (e, t, o) {
    e.exports = o(4);
  },
]);
