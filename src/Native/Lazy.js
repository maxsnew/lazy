var _elm_lang$lazy$Native_Lazy = function() {

// mutates `lzy` Unevaluated thunk into Evaluated value, returning value.
function memoize(lzy) {
    if (lzy.ctor === 'Evaluating')
        throw Error("Lazy.memoize:  recursive evaluation error!!!");
    lzy.ctor = 'Evaluating';
    var v = lzy._0(lzy); // dummy placeholder arg
    lzy.ctor = 'Evaluated';
    lzy._0 = v;
    return v;
}

return {
    memoize: memoize
};

}();
