global.Delegator = require('dom-delegator');
global.vdom      = require('virtual-dom');
global.hook      = require('virtual-dom/virtual-hyperscript/hooks/ev-hook');
global.focusHook = require('virtual-dom/virtual-hyperscript/hooks/focus-hook');


global.Delegator();


global.h             = vdom.h;
global.diff          = vdom.diff;
global.patch         = vdom.patch;
global.createElement = vdom.create;
