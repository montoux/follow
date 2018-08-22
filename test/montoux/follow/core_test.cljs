(ns montoux.follow.core-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [montoux.follow.core :as f]
            [montoux.test.mock :as mock :refer [mock stub]]))


(defn ^:dynamic callback [resource-id key old new]
  (is false "should be mocked"))


(defn callback-var [& args]
  (apply callback args))


(deftest test-topo-sort-reachable
  (testing "sort forest"
    (let [a (f/follow identity (atom 1))
          b (f/follow identity (atom 2))
          c (f/follow identity (atom 3))]
      (is (= [a b c] (f/topo-sort-followers a b c)))
      (is (= [b c a] (f/topo-sort-followers b c a)))
      (is (= [c a b] (f/topo-sort-followers c a b)))

      (is (= [a] (f/topo-sort-followers a)))
      (is (= [b] (f/topo-sort-followers b)))
      (is (= [c] (f/topo-sort-followers c)))
      ))

  (testing "sort path"
    (let [a (f/follow identity (atom 1))
          b (f/follow identity a)
          c (f/follow identity b)]
      (is (= [a b c] (f/topo-sort-followers a b c)))
      (is (= [a b c] (f/topo-sort-followers b c a)))
      (is (= [a b c] (f/topo-sort-followers c a b)))

      (is (= [a b c] (f/topo-sort-followers a)))
      (is (= [b c] (f/topo-sort-followers b)))
      (is (= [c] (f/topo-sort-followers c)))
      ))

  (testing "sort tree"
    (let [a (f/follow identity (atom 1))
          b (f/follow identity a)
          c (f/follow identity a)]
      (is (#{[a b c] [a c b]} (f/topo-sort-followers a b c)))
      (is (#{[a b c] [a c b]} (f/topo-sort-followers a c b)))
      (is (#{[a b c] [a c b]} (f/topo-sort-followers b a c)))
      (is (#{[a b c] [a c b]} (f/topo-sort-followers b c a)))
      (is (#{[a b c] [a c b]} (f/topo-sort-followers c a b)))
      (is (#{[a b c] [a c b]} (f/topo-sort-followers c b a)))

      (is (#{[a b c] [a c b]} (f/topo-sort-followers a)))
      (is (= [b] (f/topo-sort-followers b)))
      (is (= [c] (f/topo-sort-followers c)))
      ))

  (testing "sort dag"
    (let [a (f/follow identity (atom 1))
          b (f/follow identity a)
          c (f/follow identity a)
          d (f/follow + b c)]
      (is (= [a b c d] (f/topo-sort-followers a b c d)))

      (is (#{[a b c d] [a c b d]} (f/topo-sort-followers a)))
      (is (= [b d] (f/topo-sort-followers b)))
      (is (= [c d] (f/topo-sort-followers c)))
      (is (= [d] (f/topo-sort-followers d)))
      ))

  (testing "sort cycle"
    (let [a (f/follow identity (atom 1))
          b (f/follow identity a)
          _ (add-watch b 'a a)]
      (is (thrown-with-msg? js/Error #"^Cycle.*" (f/topo-sort-followers a)))
      ))
  )


(deftest test-notify-list
  (binding [callback (stub nil)]
    (let [state (atom [0])
          a     (f/follow (partial map inc) state)
          b     (f/follow (partial map inc) a)
          c     (f/follow (partial map inc) b)
          d     (f/follow (partial map inc) c)
          e     (f/follow (partial map inc) d)
          ]
      (add-watch a 'test callback)
      (add-watch b 'test callback)
      (add-watch c 'test callback)
      (add-watch d 'test callback)
      (add-watch e 'test callback)

      (is (= [1] @a))
      (is (= [2] @b))
      (is (= [3] @c))
      (is (= [4] @d))
      (is (= [5] @e))

      (reset! state [0 1])
      (is (= [1 2] @a))
      (is (= [2 3] @b))
      (is (= [3 4] @c))
      (is (= [4 5] @d))
      (is (= [5 6] @e))

      (is (= [['test a [1] [1 2]]
              ['test b [2] [2 3]]
              ['test c [3] [3 4]]
              ['test d [4] [4 5]]
              ['test e [5] [5 6]]]
             (mock/calls callback)))
      )))


(deftest test-notify-graph
  (binding [callback (stub nil)]
    (let [!input (atom {:a 0 :b 0 :c 0})
          a      (f/follow :a !input)
          b      (f/follow :b !input)
          c      (f/follow :c !input)
          ab     (f/follow (fn [a b] {:a a :b b}) a b)
          abc    (f/follow (fn [ab c] (assoc ab :c c)) ab c)
          ]
      (add-watch a 'test callback-var)
      (add-watch ab 'test callback-var)
      (add-watch abc 'test callback-var)

      (is (= {:a 0 :b 0 :c 0} @abc))

      (swap! !input #(zipmap (keys %) (map inc (vals %))))
      (is (= {:a 1 :b 1 :c 1} @!input))
      (is (= {:a 1 :b 1 :c 1} @abc))

      (is (= [['test a 0 1]
              ['test ab {:a 0 :b 0} {:a 1 :b 1}]
              ['test abc {:a 0 :b 0 :c 0} {:a 1 :b 1 :c 1}]]
             (mock/calls callback)))
      )))


(deftest test-notify-multiple-inputs
  (binding [callback (stub nil)]
    (let [a (atom 1)
          b (f/lift inc a)
          _ (b)                                             ;; initialise
          c (f/follow inc b)]

      (add-watch b 'test callback-var)
      (add-watch c 'test callback-var)

      (reset! a 2)

      (is (mock/not-called callback))

      (f/update-all c b)

      (is (= [
              ['test b 2 3]
              ['test c 3 4]]
             (mock/calls callback)))
      )))


(deftest test-update-all
  (let [a (atom 1)
        b (atom 1)
        c (atom 1)
        d (f/lift + a b)                                    ;; lift creates a follower but doesn't bind it
        _ (d)                                               ;; initialise - force an update
        e (f/lift + b c)                                    ;; ditto
        _ (e)
        f (f/follow + d e)]

    (add-watch d 'test callback-var)
    (add-watch e 'test callback-var)
    (add-watch f 'test callback-var)

    ;; this test setup allows us to test update-all manually -- changing the atoms will not notify the watches

    (is (= 2 @d))
    (is (= 2 @e))
    (is (= 4 @f))

    (binding [callback (stub nil)]
      (f/update-all d e f)
      (is (mock/not-called callback)))                      ;; no changes to atoms, so no callback notifications

    (binding [callback (stub nil)]
      (reset! a 2)                                          ;; d and f should change
      (f/update-all d e)
      (is (= [
              ['test d 2 3]
              ['test f 4 5]]
             (mock/calls callback))))

    (binding [callback (stub nil)]
      (reset! c 2)                                          ;; e and f should change
      (f/update-all d e)
      (is (= [
              ['test e 2 3]
              ['test f 5 6]]
             (mock/calls callback))))

    (binding [callback (stub nil)]
      (reset! b 2)                                          ;; d, e, and f should change
      (f/update-all f)                                      ;; f's upstream watches don't know they need to recompute
      (is (mock/not-called callback))
      (f/update-all d e f)
      (is (= [
              ['test d 3 4]
              ['test e 3 4]
              ['test f 6 8]]
             (mock/calls callback))))

    (binding [callback (stub nil)]
      (reset! a 3)
      (reset! b 1)
      (reset! c 3)
      (f/update-all d e f)
      (is (mock/not-called callback)))                      ;; although the atoms have changed, the sums have not

    ))


(deftest test-update-suppression
  (let [!input (atom {:x 1 :y 1})
        x      (mock/mock :x)
        !x     (f/follow x !input)
        y      (mock/mock :y)
        !y     (f/follow y !input)
        sum    (mock/mock +)
        !sum   (f/follow sum !x !y)
        ]

    (is (= [1 1 1]
           (map (comp count mock/calls) [x y sum]))
        "every mock is called once when its follower is initialised")

    (swap! !input update :y inc)
    (is (= [2 2 2]
           (map (comp count mock/calls) [x y sum]))
        "x, y, and sum are called when y is incremented because they all depend on watchables that have changed")
    (is (= 3 @!sum))

    (swap! !input assoc :z 1)
    (is (= [3 3 2]
           (map (comp count mock/calls) [x y sum]))
        "x and y are called when input changes, but sum is not because the values it follows have not changed")
    ))


(deftest test-bind-unbind
  (let [a (atom 1)
        b (f/lift + a)]
    (add-watch b 'test callback-var)

    (binding [callback (stub nil)]
      (testing "an unbound follower doesn't fire events and doesn't have a value"
        (is (mock/not-called callback))
        (is (= nil @b))

        (reset! a 2)
        (is (mock/not-called callback))
        (is (= nil @b))))

    (binding [callback (stub nil)]
      (testing "when bound, follower updates it's value and fires events if necessary"
        (f/bind b)

        (is (= [['test b nil 2]]
               (mock/calls callback)))
        (is (= 2 @b))))

    (binding [callback (stub nil)]
      (testing "after bind, events fire as usual"
        (reset! a 3)

        (is (= [['test b 2 3]]
               (mock/calls callback)))
        (is (= 3 @b))))

    (binding [callback (stub nil)]
      (testing "no events fire on/after dispose, no change to follower value"
        (f/dispose b)
        (reset! a 2)

        (is (nil? @b))
        (is (mock/not-called callback))
        (reset! a 3)
        (is (mock/not-called callback))))
    ))


(deftest test-latches
  (let [l (f/latch +)]
    (add-watch l 'test callback-var)

    (binding [callback (stub nil)]
      (testing "an un-initialised latch doesn't have a value"
        (is (mock/not-called callback))
        (is (= nil @l))))

    (binding [callback (stub nil)]
      (testing "calling a latch triggers a notification and updates value"
        (l 1 2 3)
        (is (= 6 @l))
        (is (= ['test l nil 6]
               (mock/last-call callback)))
        (l 5 -1)
        (is (= 4 @l))
        (is (= ['test l 6 4]
               (mock/last-call callback))))))

  (binding [callback (stub nil)]
    (testing "a latch can be initialised with a value"
      (let [l (f/latch + 1)]
        (add-watch l 'test callback-var)
        (is (= 1 @l))
        (is (mock/not-called callback))
        (l 2)
        (is (= 2 @l))
        (is (= ['test l 1 2]
               (mock/last-call callback)))
        )))
  )
