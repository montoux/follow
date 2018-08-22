# Montoux Follow

A Functional Reactive Programming (FRP) inspired library for managing state in ClojureScript.

Followers bring FRP ideas to a ClojureScript stack, e.g. for use with React.
A follower is a data transformation function raised to operate over state, where state can be encapsulated by
Atoms, or other followers. When state changes, the follower will recompute its internal value and notify listeners
so they can respond to the change, as long as the internal value is has changed.

When a state update occurs, the API performs a topological sort then updates followers in order to ensure that each
follower will only recompute once per state change (each listener will only be called once).

Followers are stateful objects with references to their state sources, so they need to be cleaned up manually when
they are no longer required.

## Usage

```clojure
(let [a (atom 1)
      b (f/follow (partial * 2) a)
      c (f/follow (partial * 2) b)
      d (f/follow + a b c)]
  (add-watch d :print (fn [k r o n] (println n)))

  (reset! a 2)          ;; prints 14
  (reset! a 1)          ;; prints 7

  (run! f/dispose [b c d]))

(let [f  (f/lift repeat)
      n! (f/latch identity 1)]
  (add-watch f :print (fn [k r o n] (println n)))
  (f/partial f n)
  (f/bind f (atom "x")) ;; prints [x]
  (n! 5)                ;; prints [x x x x x]
  (f/dispose f)
  )
```

## Installation

Montoux Follow is available from [clojars.org](https://clojars.org/montoux/follow).

Add this to your `:dependencies` in `project.clj`:

```clojure
[montoux/follow "1.0.0"]
```

## Montoux

[Montoux](http://montoux.com) is the global leader in pricing transformation for the life insurance industry.
Our customers include several of the world's leading insurance providers and we are expanding our business in
the United States and Asia, as well as Australia and New Zealand.

## License

Copyright © 2018 [Montoux Limited](https://montoux.com)

The use and distribution terms for this software are covered by the Eclipse Public License 1.0.
By using this software in any fashion, you are agreeing to be bound by the terms of this license.
You must not remove this notice, or any other, from this software.
