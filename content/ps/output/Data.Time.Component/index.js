// Generated by purs version 0.11.6
"use strict";
var Control_Apply = require("../Control.Apply");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Boolean = require("../Data.Boolean");
var Data_Bounded = require("../Data.Bounded");
var Data_Enum = require("../Data.Enum");
var Data_Eq = require("../Data.Eq");
var Data_Generic = require("../Data.Generic");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Maybe = require("../Data.Maybe");
var Data_Ord = require("../Data.Ord");
var Data_Ring = require("../Data.Ring");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Show = require("../Data.Show");
var Data_Unit = require("../Data.Unit");
var Prelude = require("../Prelude");
var Second = function (x) {
    return x;
};
var Minute = function (x) {
    return x;
};
var Millisecond = function (x) {
    return x;
};
var Hour = function (x) {
    return x;
};
var showSecond = new Data_Show.Show(function (v) {
    return "(Second " + (Data_Show.show(Data_Show.showInt)(v) + ")");
});
var showMinute = new Data_Show.Show(function (v) {
    return "(Minute " + (Data_Show.show(Data_Show.showInt)(v) + ")");
});
var showMillisecond = new Data_Show.Show(function (v) {
    return "(Millisecond " + (Data_Show.show(Data_Show.showInt)(v) + ")");
});
var showHour = new Data_Show.Show(function (v) {
    return "(Hour " + (Data_Show.show(Data_Show.showInt)(v) + ")");
});
var ordSecond = Data_Ord.ordInt;
var ordMinute = Data_Ord.ordInt;
var ordMillisecond = Data_Ord.ordInt;
var ordHour = Data_Ord.ordInt;
var genericSecond = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Time.Component.Second" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Second))(Data_Generic.fromSpine(Data_Generic.genericInt)(v["value1"][0](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("Data.Time.Component.Second", [ {
        sigConstructor: "Data.Time.Component.Second", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(Data_Generic.genericInt)(Data_Generic.anyProxy);
        } ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("Data.Time.Component.Second", [ function ($dollarq) {
        return Data_Generic.toSpine(Data_Generic.genericInt)(v);
    } ]);
});
var genericMinute = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Time.Component.Minute" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Minute))(Data_Generic.fromSpine(Data_Generic.genericInt)(v["value1"][0](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("Data.Time.Component.Minute", [ {
        sigConstructor: "Data.Time.Component.Minute", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(Data_Generic.genericInt)(Data_Generic.anyProxy);
        } ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("Data.Time.Component.Minute", [ function ($dollarq) {
        return Data_Generic.toSpine(Data_Generic.genericInt)(v);
    } ]);
});
var genericMillisecond = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Time.Component.Millisecond" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Millisecond))(Data_Generic.fromSpine(Data_Generic.genericInt)(v["value1"][0](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("Data.Time.Component.Millisecond", [ {
        sigConstructor: "Data.Time.Component.Millisecond", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(Data_Generic.genericInt)(Data_Generic.anyProxy);
        } ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("Data.Time.Component.Millisecond", [ function ($dollarq) {
        return Data_Generic.toSpine(Data_Generic.genericInt)(v);
    } ]);
});
var genericHour = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "Data.Time.Component.Hour" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Hour))(Data_Generic.fromSpine(Data_Generic.genericInt)(v["value1"][0](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("Data.Time.Component.Hour", [ {
        sigConstructor: "Data.Time.Component.Hour", 
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(Data_Generic.genericInt)(Data_Generic.anyProxy);
        } ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("Data.Time.Component.Hour", [ function ($dollarq) {
        return Data_Generic.toSpine(Data_Generic.genericInt)(v);
    } ]);
});
var eqSecond = Data_Eq.eqInt;
var eqMinute = Data_Eq.eqInt;
var eqMillisecond = Data_Eq.eqInt;
var eqHour = Data_Eq.eqInt;
var boundedSecond = new Data_Bounded.Bounded(function () {
    return ordSecond;
}, 0, 59);
var boundedMinute = new Data_Bounded.Bounded(function () {
    return ordMinute;
}, 0, 59);
var boundedMillisecond = new Data_Bounded.Bounded(function () {
    return ordMillisecond;
}, 0, 999);
var boundedHour = new Data_Bounded.Bounded(function () {
    return ordHour;
}, 0, 23);
var boundedEnumSecond = new Data_Enum.BoundedEnum(function () {
    return boundedSecond;
}, function () {
    return enumSecond;
}, 60, function (v) {
    return v;
}, function (n) {
    if (n >= 0 && n <= 59) {
        return new Data_Maybe.Just(n);
    };
    if (Data_Boolean.otherwise) {
        return Data_Maybe.Nothing.value;
    };
    throw new Error("Failed pattern match at Data.Time.Component line 94, column 1 - line 94, column 49: " + [ n.constructor.name ]);
});
var enumSecond = new Data_Enum.Enum(function () {
    return ordSecond;
}, function ($64) {
    return Data_Enum.toEnum(boundedEnumSecond)((function (v) {
        return v - 1 | 0;
    })(Data_Enum.fromEnum(boundedEnumSecond)($64)));
}, function ($65) {
    return Data_Enum.toEnum(boundedEnumSecond)((function (v) {
        return v + 1 | 0;
    })(Data_Enum.fromEnum(boundedEnumSecond)($65)));
});
var boundedEnumMinute = new Data_Enum.BoundedEnum(function () {
    return boundedMinute;
}, function () {
    return enumMinute;
}, 60, function (v) {
    return v;
}, function (n) {
    if (n >= 0 && n <= 59) {
        return new Data_Maybe.Just(n);
    };
    if (Data_Boolean.otherwise) {
        return Data_Maybe.Nothing.value;
    };
    throw new Error("Failed pattern match at Data.Time.Component line 64, column 1 - line 64, column 49: " + [ n.constructor.name ]);
});
var enumMinute = new Data_Enum.Enum(function () {
    return ordMinute;
}, function ($66) {
    return Data_Enum.toEnum(boundedEnumMinute)((function (v) {
        return v - 1 | 0;
    })(Data_Enum.fromEnum(boundedEnumMinute)($66)));
}, function ($67) {
    return Data_Enum.toEnum(boundedEnumMinute)((function (v) {
        return v + 1 | 0;
    })(Data_Enum.fromEnum(boundedEnumMinute)($67)));
});
var boundedEnumMillisecond = new Data_Enum.BoundedEnum(function () {
    return boundedMillisecond;
}, function () {
    return enumMillisecond;
}, 1000, function (v) {
    return v;
}, function (n) {
    if (n >= 0 && n <= 999) {
        return new Data_Maybe.Just(n);
    };
    if (Data_Boolean.otherwise) {
        return Data_Maybe.Nothing.value;
    };
    throw new Error("Failed pattern match at Data.Time.Component line 125, column 1 - line 125, column 59: " + [ n.constructor.name ]);
});
var enumMillisecond = new Data_Enum.Enum(function () {
    return ordMillisecond;
}, function ($68) {
    return Data_Enum.toEnum(boundedEnumMillisecond)((function (v) {
        return v - 1 | 0;
    })(Data_Enum.fromEnum(boundedEnumMillisecond)($68)));
}, function ($69) {
    return Data_Enum.toEnum(boundedEnumMillisecond)((function (v) {
        return v + 1 | 0;
    })(Data_Enum.fromEnum(boundedEnumMillisecond)($69)));
});
var boundedEnumHour = new Data_Enum.BoundedEnum(function () {
    return boundedHour;
}, function () {
    return enumHour;
}, 24, function (v) {
    return v;
}, function (n) {
    if (n >= 0 && n <= 23) {
        return new Data_Maybe.Just(n);
    };
    if (Data_Boolean.otherwise) {
        return Data_Maybe.Nothing.value;
    };
    throw new Error("Failed pattern match at Data.Time.Component line 34, column 1 - line 34, column 45: " + [ n.constructor.name ]);
});
var enumHour = new Data_Enum.Enum(function () {
    return ordHour;
}, function ($70) {
    return Data_Enum.toEnum(boundedEnumHour)((function (v) {
        return v - 1 | 0;
    })(Data_Enum.fromEnum(boundedEnumHour)($70)));
}, function ($71) {
    return Data_Enum.toEnum(boundedEnumHour)((function (v) {
        return v + 1 | 0;
    })(Data_Enum.fromEnum(boundedEnumHour)($71)));
});
module.exports = {
    eqHour: eqHour, 
    ordHour: ordHour, 
    genericHour: genericHour, 
    boundedHour: boundedHour, 
    enumHour: enumHour, 
    boundedEnumHour: boundedEnumHour, 
    showHour: showHour, 
    eqMinute: eqMinute, 
    ordMinute: ordMinute, 
    genericMinute: genericMinute, 
    boundedMinute: boundedMinute, 
    enumMinute: enumMinute, 
    boundedEnumMinute: boundedEnumMinute, 
    showMinute: showMinute, 
    eqSecond: eqSecond, 
    ordSecond: ordSecond, 
    genericSecond: genericSecond, 
    boundedSecond: boundedSecond, 
    enumSecond: enumSecond, 
    boundedEnumSecond: boundedEnumSecond, 
    showSecond: showSecond, 
    eqMillisecond: eqMillisecond, 
    ordMillisecond: ordMillisecond, 
    genericMillisecond: genericMillisecond, 
    boundedMillisecond: boundedMillisecond, 
    enumMillisecond: enumMillisecond, 
    boundedEnumMillisecond: boundedEnumMillisecond, 
    showMillisecond: showMillisecond
};