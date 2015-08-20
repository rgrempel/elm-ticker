# elm-ticker

This module offers on-demand ticks, as an alternative to using `Time.fps`
or `Time.fpsWhen` to manage a `Signal` that produces ticks, or using `Task.sleep`
to delay a task, or using
[elm-animation-frame](http://package.elm-lang.org/packages/jwmerrill/elm-animation-frame/latest)
for a `Signal` of animtation frames.

* The ticks generated by this module will fire according to the browser's
  native `requestAnimationFrame()` method -- that is, they will fire when the
  browser needs a new frame.

* Multiple requests for a tick will be consolidated until the tick fires,
  so that `requestAnimationFrame()` is only called once per tick. When the tick
  fires, all the conslidated requests will be fulfilled.

* Once your tick fires, if you need another one (e.g. your animation is not
  finished), then you need to request another one -- they don't continue to
  flow automatically.

* You can use `dropWhileWaiting` to 'debounce' your `Signal` of models so that your
  `view` is only recalculated once per animation frame. This turns out to be a
  very significant optimization, especially where your app has many components
  which are mananging their own animations.
  To see how much difference it makes, compare spinning 100 squares
  [without using dropWhileWaiting](https://rgrempel.github.io/elm-ticker/unoptimized.html)
  vs. the otherwise-identical code [using dropWhileWaiting](https://rgrempel.github.io/elm-ticker/optimized.html).
  

Note that this module provides a `Signal` of tasks which need to be executed. In
order for that to happen, you will need to connect that signal to a port in
your main module. For instance:

```elm
port tickerTasks : Signal (Task () ())
port tickerTasks =
    Ticker.tasks
```

Also note that the tasks returned by `tick` and `tock` are *not* automatically
sent through this `Signal` -- you will need to arrange for those tasks to be
executed in same way that other tasks are executed in your app.

# Functions

<h3>
tick : (Time -> Task x a) -> Task () ()
</h3>

Returns a task which, when excecuted, waits for the browser to request an
animation frame (via the browser's native requestAnimationFrame() method). It
then calls the function you provided, passing along the Time provided by 
requestAnimationFrame(). The Task returned by your function is then immediately
executed.

<h3>
tock : Address action -> (Time -> action) -> Task () ()
</h3>

Like tick, except that you provide a function which returns an action, which
will be sent to the address you provide, when the next frame is needed. Typically,
the 'function' you provide will be an action tag, for an action like:

```elm
type Action =
    Tick Time |
    ... other actions
```

Then, assuming you have an `address` which accepts such actions, you could call:

```elm
tock address Tick
```

... which will return a Task that, when executed, will wait until an animation
frame is needed and then send the message to the address.

<h3>
tasks : Signal (Task () ())
</h3>

A signal of tasks which the module needs to be executed in order to maintain
its internal state. In order to use this module, you need to connect this
signal to a port in your main module -- see example above.

<h3>
dropWhileWaiting : a -> Signal a -> Signal a
</h3>

Given a `Signal`, and an initial value, returns a new `Signal` which will
drop all updates while we are waiting for a tick. When the tick arrives, and
all the requests for a tick have been fulfilled, the `Signal` will emit its
current value.

This is useful to prevent your `view` logic from running while we are waiting
for a tick. There's no point updating the view multiple times before the next
animation frame is needed, after all! So, if you have an app structure like this:

```elm
models : Signal Model

initialModel : Model

view : Model -> Html

port main : Signal Html
port main = Signal.map view models
```

... then you can create some efficiency by changing the last line to this:

```elm
port main = Signal.map view (dropWhileWaiting initialModel models)
```

Of course, your app may be wired differently -- the main thing is to apply
`dropWhileWaiting` just before your 'view' logic is applied. Do be careful not
to prevent other parts of your logic from running -- you still need every
`update`, for instance.

Note that the initial value which you supply as the second parameter is not
actually used -- it would only be used if the initial state would be to drop
updates, which it is not. However, for the initial value is still required in
order to satisfy the type checker -- it can be anything that has the
correct type.
