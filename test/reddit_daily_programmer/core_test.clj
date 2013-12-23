(ns reddit-daily-programmer.core-test
  (:require [clojure.test :refer :all]
            [reddit-daily-programmer.core :refer :all]))

(deftest test-easy-146
  (is (= (easy-146 "5 3.7")   "21.748"))
  (is (= (easy-146 "100 1.0") "6.282")))

(run-tests 'reddit-daily-programmer.core-test)
