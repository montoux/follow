(ns montoux.follow.core
  "Followers are a tool for bringing Functional Reactive Programming ideas to a ClojureScript stack, e.g. for use with
  React. A follower is a data transformation function raised to operate over state, where state can be encapsulated by
  Atoms, or other followers. When state changes, the follower will recompute its internal value and notify listeners
  so they can respond to the change.

  When a state update occurs, the API performs a topological sort then updates followers in order to ensure that each
  follower will only recompute once per state change (each listener will only be called once).

  Followers are stateful objects with references to their state sources, so they need to be cleaned up manually when
  they are no longer required (JavaScript Weak References are not widely available).
  "
  (:refer-clojure :exclude [partial])
  (:require [cljs.core :as c]))


;; =====================================================================================================================
;; API for follower implementations
;; =====================================================================================================================


(defprotocol IFollower
  (-update [this]
           "Update the follower's cached state, if applicable.
           Returns [old new] states, or nil if no change."))


(defn follower? [follower]
  (satisfies? IFollower follower))


(defprotocol IDisposable
  (-dispose [this]))


(defn disposable? [obj]
  (satisfies? IDisposable obj))


(declare notify update-all)


;; =====================================================================================================================
;; Topological Sort
;;
;; These private functions are used by update-all to determine a partial ordering for of reachable followers that will
;; be invoked as a consequence of a state change. Performing a topological sort before updating followers ensures that
;; a follower is only updated once per state change (avoid exposing internal states during update propagation).
;;
;; These functions are public for use by API implementors and may be subject to change.
;; =====================================================================================================================

(defn- topo-visit
  "Recursive depth-first topological sort, using 'path' to detect cycles and 'visited' as internal state. `topo-visit`
  will return a vector containing:
   * a set of visited nodes
   * a partially ordered list of followers that includes all nodes reachable from the provided node.
   "
  [children-fn path [visited-set visited-ordered :as visited] x]
  (when (contains? path x)
    (throw (js/Error "Cycle in topological sort, already visiting " x)))
  (if (contains? visited-set x)
    visited
    (let [path (conj path x)
          [visited-set visited-ordered] (reduce (c/partial topo-visit children-fn path) visited (children-fn x))]
      [(conj! visited-set x) (conj visited-ordered x)])))


(defn topo-sort
  "Generic topological sort. `children-fn` should return a list of nodes reachable from the specified node."
  [children-fn xs]
  (not-empty (second (reduce (c/partial topo-visit children-fn #{}) [(transient #{}) nil] xs))))


(defn- watcher-children [follower]
  (->> (.-watches follower)
       vals
       reverse
       (filter follower?)
       not-empty))


(defn topo-sort-followers
  "Topologically sort the provided list of IWatch implementors using their 'watches' field. Returns a partially ordered
  list of reachable followers. Throws an exception if a cycle is detected."
  [& followers]
  (topo-sort watcher-children (reverse followers)))


;; =====================================================================================================================
;; Propagate state changes
;;
;; These functions are public for use by API implementors and may be subject to change.
;; =====================================================================================================================


(def ^:private ^:dynamic !queue nil)


(defn- queue [listener key ref old new]
  (swap! !queue conj [listener key ref old new]))


(defn- notify-all [queue]
  (doseq [[f & args] queue]
    (apply f args)))


(defn update-all
  "This function should be invoked by API implementors when a state change has occurred that is known to affect a group
  of followers.
  The function will perform a topological sort of the followers, update them, then notify their listeners."
  [& followers]
  (assert (every? #(satisfies? IFollower %) followers)
          (str "all followers must implement IFollower " (pr-str followers)))
  (let [q           (or !queue (atom []))
        to-visit    (apply topo-sort-followers (doall followers))
        will-visit? (set to-visit)]
    (binding [!queue q]
      (loop [notify?  (set followers)
             to-visit to-visit]
        (when-let [[follower & to-visit] (seq to-visit)]
          (if-let [[old new] (and (contains? notify? follower)
                                  ;; follower may have changed, recompute
                                  (-update follower))]
            ;; follower did change
            (let [notify? (reduce (fn [notify? [key listener]]
                                    (if (will-visit? listener)
                                      (conj notify? listener)
                                      (do (queue listener key follower old new)
                                          notify?)))
                                  notify?
                                  (.-watches follower))]
              (recur notify? to-visit))

            ;; follower did not change
            (recur notify? to-visit))
          )))
    (when-not !queue
      (notify-all @q))))


(defn notify
  "This function should be invoked by API implementors when a state change has occurred for a particular follower.
  It will notify all listeners of the reference, then call update-all on any follower of the reference.
  Provides compatibility with cljs.core/-notify-all while supporting followers as well as atoms."
  [ref old new]
  (let [q (or !queue (atom []))]
    (binding [!queue q]
      (->> (for [[key listener] (.-watches ref)]
             (if (satisfies? IFollower listener)
               listener
               (do (listener key ref old new) nil)))
           (remove nil?)
           (apply update-all)))
    (when-not !queue
      (notify-all @q))))


;;======================================================================================================================
;;                                          Propagate state changes
;; =====================================================================================================================


(deftype Follower [meta refs compute-fn watches state]
  IFollower
  (-update [this]
    (when-not compute-fn
      (when (and js/console (.-warn js/console))
        (.warn js/console "Follower has already been disposed!")))
    (let [old state
          new (when compute-fn (compute-fn))]
      (when-not (= old new)
        (set! (.-state this) new)
        [old new])))

  Fn
  IFn
  (-invoke [this k r o n]
    (when-let [[old new] (-update this)]
      (notify this old new)))

  IDeref
  (-deref [this]
    state)

  IMeta
  (-meta [this]
    meta)

  IWithMeta
  (-with-meta [this meta]
    (set! (.-meta this) meta)
    this)

  IPrintWithWriter
  (-pr-writer [follower writer opts]
    (-write writer "#<Follow ")
    (if (satisfies? IPrintWithWriter state)
      (-pr-writer state writer opts)
      (-write writer state))
    (-write writer ">"))

  IWatchable
  (-notify-watches [this old new]
    (notify this old new))
  (-add-watch [this key watch-fn]
    (set! (.-watches this) (assoc watches key watch-fn)))
  (-remove-watch [this key]
    (set! (.-watches this) (dissoc watches key)))

  IDisposable
  (-dispose [this]
    (doseq [ref refs]
      (remove-watch ref this))
    (set! (.-refs this) nil)
    (set! (.-compute-fn this) nil)
    (set! (.-watches this) nil)
    (set! (.-state this) nil)))


(extend-type Atom
  IWatchable
  (-notify-watches [this old new]
    (notify this old new)))


;; =====================================================================================================================
;; Public API for followers
;; =====================================================================================================================


(defn- assert-follower! [follower]
  (assert (instance? Follower follower) (str "first argument must be a follower (" follower ")")))


(defn- assert-ref! [ref]
  (assert (satisfies? IDeref ref) (str "argument must be deref-able (" ref ")"))
  (assert (satisfies? IWatchable ref) (str "argument must be watchable (" ref ")")))


(defn- refs [f]
  (.-refs f))


(defn lift
  "Constructs an unbound follower from a function. The arity remains the same, but arguments to the follower should be
  stateful (refs). Arguments do not have to be provided here, they can be provided later with `partial` or `bind`.
  Followers constructed with `lift` do not need to be disposed, they are not linked until `bind` is called."
  [f & args]
  (run! assert-ref! args)
  (let [follower (->Follower nil (vec args) nil nil nil)]
    (set! (.-compute-fn follower) (fn update* [] (apply f (map deref (refs follower)))))
    follower))


(defn- partial-1
  "Partially apply a reference to a follower without binding the follower."
  [follower ref]
  (assert-ref! ref)
  (set! (.-refs follower) (conj (refs follower) ref))
  follower)


(defn partial
  "Partially apply args (refs) to an unbound follower without binding the follower. Refs and their values should match
  the follower's encapsulated function's arity. This operation mutates the follower so that when it is bound the given
  refs will be derefed to provide arguments to the encapsulated function."
  [follower & args]
  (assert-follower! follower)
  (reduce partial-1 follower args))


(defn- bind-1
  "Given a follower and a watchable reference, binds the follower to the reference so that when the reference changes
  the follower will update. Adds the reference at the end of any previously-bound references."
  [follower ref]
  (assert-ref! ref)
  (add-watch ref follower follower)
  follower)


(defn bind
  "Given an unbound follower created by `lift`, binds the follower to its arguments by adding watches. Binding will
  trigger an initial state calculation for the follower, and notify any watches that already exist that an initial
  state has been established. Once a follower has been bound it must be explicitly disposed by calling `dispose`, which
  will remove watches and destroy internal state. If provided, arguments should be refs that encapsulate appropriate
  state and match the follower's encapsulated function's arity."
  [follower & args]
  (assert (instance? Follower follower) "bind called on something other than a follower")
  (let [follower (reduce partial-1 follower args)
        follower (reduce bind-1 follower (refs follower))]
    ;; invoking the follower forces an update, initialising the follower's value
    (follower)
    follower))


(defn follow
  "Create a follower and binds it a list of IDeref/IWatchable refs in one step. The follower should be destroyed with
  dispose."
  [f & args]
  (apply bind (lift f) args))


(defn dispose
  "Remove and dispose of a follower. JavaScript doesn't have widespread support for weak references yet, so followers
  must be disposed of manually."
  [follower]
  (-dispose follower))


;; =====================================================================================================================
;; Latches
;;
;; Latches are useful for capturing events that occur periodically and transforming them into a follower-like object
;; that will always contain the result of the most recent call.
;; =====================================================================================================================


(defn- call-latch [latch & args]
  (when (.-transform-fn latch)
    (let [old (.-state latch)
          new (apply (.-transform-fn latch) args)]
      (when-not (= old new)
        (set! (.-state latch) new)
        (notify latch old new)))))


(deftype Latch [meta watches transform-fn state]
  IDeref
  (-deref [this]
    state)

  IReset
  (-reset! [this value]
    (call-latch value))

  IMeta
  (-meta [this]
    meta)

  IWithMeta
  (-with-meta [this meta]
    (set! (.-meta this) meta)
    this)

  IPrintWithWriter
  (-pr-writer [a writer opts]
    (-write writer "#<Latch")
    (-write writer ": ")
    (if (satisfies? IPrintWithWriter state)
      (-pr-writer state writer opts)
      (-write writer state))
    (-write writer ">"))

  IWatchable
  (-notify-watches [this old new]
    (notify this old new))
  (-add-watch [this key watch-fn]
    (set! (.-watches this) (assoc watches key watch-fn)))
  (-remove-watch [this key]
    (set! (.-watches this) (dissoc watches key)))

  IDisposable
  (-dispose [this]
    (set! (.-transform-fn this) nil)
    (set! (.-watches this) nil)
    (set! (.-state this) nil))

  ;; Clojure/ClojureScript don't support varadic protocol-fns
  Fn
  IFn
  (-invoke [this]
    (call-latch this))
  (-invoke [this a]
    (call-latch this a))
  (-invoke [this a b]
    (call-latch this a b))
  (-invoke [this a b c]
    (call-latch this a b c))
  (-invoke [this a b c d]
    (call-latch this a b c d))
  (-invoke [this a b c d e]
    (call-latch this a b c d e))
  (-invoke [this a b c d e f]
    (call-latch this a b c d e f))
  (-invoke [this a b c d e f g]
    (call-latch this a b c d e f g))
  (-invoke [this a b c d e f g h]
    (call-latch this a b c d e f g h))
  (-invoke [this a b c d e f g h i]
    (call-latch this a b c d e f g h i))
  (-invoke [this a b c d e f g h i j]
    (call-latch this a b c d e f g h i j))
  (-invoke [this a b c d e f g h i j k]
    (call-latch this a b c d e f g h i j k))
  (-invoke [this a b c d e f g h i j k l]
    (call-latch this a b c d e f g h i j k l))
  (-invoke [this a b c d e f g h i j k l m]
    (call-latch this a b c d e f g h i j k l m))
  (-invoke [this a b c d e f g h i j k l m n]
    (call-latch this a b c d e f g h i j k l m n))
  (-invoke [this a b c d e f g h i j k l m n o]
    (call-latch this a b c d e f g h i j k l m n o))
  (-invoke [this a b c d e f g h i j k l m n o p]
    (call-latch this a b c d e f g h i j k l m n o p))
  (-invoke [this a b c d e f g h i j k l m n o p q]
    (call-latch this a b c d e f g h i j k l m n o p q))
  (-invoke [this a b c d e f g h i j k l m n o p q r]
    (call-latch this a b c d e f g h i j k l m n o p q r))
  (-invoke [this a b c d e f g h i j k l m n o p q r s]
    (call-latch this a b c d e f g h i j k l m n o p q r s))
  (-invoke [this a b c d e f g h i j k l m n o p q r s t]
    (call-latch this a b c d e f g h i j k l m n o p q r s t))
  (-invoke [this a b c d e f g h i j k l m n o p q r s t rest]
    (apply call-latch this a b c d e f g h i j k l m n o p q r s t rest))
  )


(defn latch
  "Creates an object implements IFn and captures the result of applying transform-fn every time it is invoked.
  Implements IWatchable so observers will be notified with the new result every time it is invoked."
  ([transform-fn]
   (latch transform-fn nil))
  ([transform-fn initial-state]
   (Latch. nil nil transform-fn initial-state)))
