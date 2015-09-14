Elm.Native.Ticker = {};
Elm.Native.Ticker.make = function (localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Ticker = localRuntime.Native.Ticker || {};
	
    if (localRuntime.Native.Ticker.values) {
		return localRuntime.Native.Ticker.values;
	}

	var Task = Elm.Native.Task.make(localRuntime);

    var wait = Task.asyncFunction(function(callback) {
        requestAnimationFrame(function(time) {
            callback(Task.succeed(time));
        });
    });
	
    return localRuntime.Native.Ticker.values = {
		wait: wait
	};
};
