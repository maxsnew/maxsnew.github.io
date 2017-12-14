// Generated by purs version 0.11.6
"use strict";
var $foreign = require("./foreign");
var Control_Applicative = require("../Control.Applicative");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Ref = require("../Control.Monad.Eff.Ref");
var DOM = require("../DOM");
var DOM_Event_EventTarget = require("../DOM.Event.EventTarget");
var DOM_Node_Types = require("../DOM.Node.Types");
var Data_Function_Uncurried = require("../Data.Function.Uncurried");
var Data_Nullable = require("../Data.Nullable");
var Data_StrMap = require("../Data.StrMap");
var Data_StrMap_ST = require("../Data.StrMap.ST");
var Data_Unit = require("../Data.Unit");
var Halogen_VDom_Types = require("../Halogen.VDom.Types");
var Prelude = require("../Prelude");
var Unsafe_Coerce = require("../Unsafe.Coerce");
var unsafeLookup = $foreign.unsafeGetAny;
var unsafeFreeze = Unsafe_Coerce.unsafeCoerce;
var pokeMutMap = $foreign.unsafeSetAny;
var newMutMap = Unsafe_Coerce.unsafeCoerce(Data_StrMap_ST["new"]);
var effUnit = Control_Applicative.pure(Control_Monad_Eff.applicativeEff)(Data_Unit.unit);
var effPure = Control_Applicative.pure(Control_Monad_Eff.applicativeEff);
var deleteMutMap = $foreign.unsafeDeleteAny;
module.exports = {
    deleteMutMap: deleteMutMap, 
    effPure: effPure, 
    effUnit: effUnit, 
    newMutMap: newMutMap, 
    pokeMutMap: pokeMutMap, 
    unsafeFreeze: unsafeFreeze, 
    unsafeLookup: unsafeLookup, 
    addEventListener: $foreign.addEventListener, 
    createElement: $foreign.createElement, 
    createTextNode: $foreign.createTextNode, 
    diffWithIxE: $foreign.diffWithIxE, 
    diffWithKeyAndIxE: $foreign.diffWithKeyAndIxE, 
    forE: $foreign.forE, 
    forInE: $foreign.forInE, 
    insertChildIx: $foreign.insertChildIx, 
    jsUndefined: $foreign.jsUndefined, 
    refEq: $foreign.refEq, 
    removeAttribute: $foreign.removeAttribute, 
    removeChild: $foreign.removeChild, 
    removeEventListener: $foreign.removeEventListener, 
    replicateE: $foreign.replicateE, 
    setAttribute: $foreign.setAttribute, 
    setTextContent: $foreign.setTextContent, 
    strMapWithIxE: $foreign.strMapWithIxE, 
    unsafeDeleteAny: $foreign.unsafeDeleteAny, 
    unsafeGetAny: $foreign.unsafeGetAny, 
    unsafeHasAny: $foreign.unsafeHasAny, 
    unsafeParent: $foreign.unsafeParent, 
    unsafeSetAny: $foreign.unsafeSetAny
};