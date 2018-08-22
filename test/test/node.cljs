(ns test.node
  (:require [cljs.test :as test]
            [montoux.follow.core-test]))

(enable-console-print!)
(test/run-all-tests #"montoux\.follow.*test")
