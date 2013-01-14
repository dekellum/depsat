(ns depsat.core-test
  (:use clojure.test
        depsat.core)
  (:import (java.math BigInteger)
           (org.sat4j.specs ISolver)
           (org.sat4j.core Vec VecInt)
           (org.sat4j.minisat SolverFactory)))

(defn- ^VecInt vec-int [coll]
  (VecInt. (int-array coll)))

(defn- ^Vec vec-big [coll]
  (Vec. (to-array (map biginteger coll))))

(defn- ^ISolver sample []
  (let [solver (SolverFactory/newLight)] ; or newDefault
    (.newVar solver 8)
    (.setExpectedNumberOfClauses solver 10)
    (.addClause solver (vec-int [1]))
    (.addClause solver (vec-int [-1 4 3 2]))
    (.addClause solver (vec-int [-1 5 6 7]))
    (.addClause solver (vec-int [-2 5]))
    (.addClause solver (vec-int [-3 5 6]))
    (.addClause solver (vec-int [-4 6 7]))
    (.addClause solver (vec-int [-1 8]))
    (.addClause solver (vec-int [-8 6]))
    (.addExactly solver (vec-int [2 3 4]) 1)
    (.addExactly solver (vec-int [5 6 7]) 1)
    solver))

(defn- models [^ISolver solver]
  (lazy-seq
   (if (.isSatisfiable solver)
     (let [m (.model solver)]
       (.addBlockingClause solver (vec-int (map - m)))
       (cons (filter pos? m)
             (models solver))))))

(deftest test-support
  (testing "vec-int"
    (is (= 3 (.size (vec-int [1 2 3])))))
  (testing "vec-big"
    (is (= 3 (.size (vec-big [1 2 3]))))))

(deftest test-sample
  (let [solver (sample)]
    (is solver)
    (is (= #{ [1, 3, 6, 8] [1, 4, 6, 8] }
           (set (models solver))))))
