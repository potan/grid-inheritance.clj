
(ns grid-test
  (:use clojure.test)
  (:use inheritance.grid))

(def grid (make-grid-hierarchy))
(def-grid-node root {} grid)
(defmulti tst (grid-dispatch) :hierarchy #'grid)
(defmethod tst [root] [v] "root")
(defmethod tst [(get-grid-node {:a :b} #'grid)] [v] "a b")
(defmethod tst [(get-grid-node {:a :b :c :d} #'grid)] [v] "a b c d")
(defmethod tst [(get-grid-node {:c :d} #'grid)] [v] "c d")
(def abcd (get-grid-node {:a :b :c :d} #'grid))
(def-grid-node aecd {:a :e :c :d} grid)

(defmulti tst1 (grid-dispatch1) :hierarchy #'grid)
(defmethod tst1 root [v] "root")

(deftest simple-test
  (is (= (tst ((with-grid-node abcd) {})) "a b c d"))
  (is (= (tst (->aecd {})) "c d"))
  (is (= (tst ((with-grid-node {:a :e :g :d} #'grid) {})) "root"))
  (is (= (tst1 ((with-grid-node {:a :e :g :d} #'grid) {})) "root"))
)
