Elm.Native.Lazy = {};
Elm.Native.Lazy.make = function(localRuntime) {

    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.Lazy = localRuntime.Native.Lazy || {};
    if (localRuntime.Native.Lazy.values) {
        return localRuntime.Native.Lazy.values;
    }

    var Utils = Elm.Native.Utils.make(localRuntime);
    
    function lazy(f) {
        var value = {};
        var isEvaled = false;
        var memoed = function(x) {
            if(!isEvaled) {
                value = f(Utils.Tuple0);
                isEvaled = true;
            }
            return value;
        }
        return memoed;
    }

    return localRuntime.Native.Lazy.values = {
        lazy: lazy
    };
};
