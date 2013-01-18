(ns depsat.version-test
  (:use clojure.test
        depsat.version))

(deftest test-segment-compare
  (is (= 0 (segment-compare "a" "a")))
  (is (= 0 (segment-compare nil nil)))
  (is (= 0 (segment-compare   0 nil)))
  (is (= 0 (segment-compare nil   0)))
  (is (= 0 (segment-compare   0   0)))
  (is (= 0 (segment-compare   1   1)))

  (is (= 1 (segment-compare   1 nil)))
  (is (= 1 (segment-compare   2   1)))
  (is (= 1 (segment-compare   0 "a")))
  (is (= 1 (segment-compare nil "a")))
  (is (= 1 (segment-compare   1 "a")))
  (is (= 1 (segment-compare "b" "a")))

  (is (= -1 (segment-compare nil   1)))
  (is (= -1 (segment-compare   1   2)))
  (is (= -1 (segment-compare "a"   0)))
  (is (= -1 (segment-compare "a" nil)))
  (is (= -1 (segment-compare "a"   1)))
  (is (= -1 (segment-compare "a" "b"))))

(deftest test-version-bare-compare
  (is (= 0  (version-bare-compare []    [])))
  (is (= 0  (version-bare-compare [1]   [1])))
  (is (= 0  (version-bare-compare [1 0] [1])))
  (is (= 0  (version-bare-compare [1]   [1 0])))
  (is (pos? (version-bare-compare [1 1] [1 0])))
  (is (pos? (version-bare-compare [1 1] [1])))
  (is (neg? (version-bare-compare [1 0] [1 1])))
  (is (neg? (version-bare-compare [1]   [1 1]))))

(deftest test-version-split-pre
  (is (= [ [1 0] ["alpha" 3] ]
         (version-split-pre [1 0 :- "alpha" 3])))
  (is (= [ [1 0] ] (version-split-pre [1 0]))))

(deftest test-version-compare
  (is (= 0  (version-compare []    [])))
  (is (= 0  (version-compare [nil] [])))
  (is (= 0  (version-compare [1]   [1])))
  (is (= 0  (version-compare [1 0] [1])))
  (is (= 0  (version-compare [1]   [1 0])))
  (is (= 0  (version-compare [1   :- "alpha" 0] [1 :- "alpha" ])))
  (is (= 0  (version-compare [1 0 :- "alpha" 0] [1 :- "alpha" ])))

  (is (pos? (version-compare [1 1] [1 0])))
  (is (pos? (version-compare [1 1] [1])))
  (is (pos? (version-compare [1 0] [1 0 :- "alpha" ])))
  (is (pos? (version-compare [1 0] [1   :- "beta" ])))
  (is (pos? (version-compare [1 :- "alpha" 1] [1 :- "alpha" ])))
  (is (pos? (version-compare [1 :- "alpha" 1] [1 :- "alpha" 0])))

  (is (neg? (version-compare [1 0] [1 1])))
  (is (neg? (version-compare [1] [1 1])))
  (is (neg? (version-compare [1 0 :- 1] [1 0]))) ;FIXME: Invalid?
  (is (neg? (version-compare [1 :- "alpha" 1]   [1 :- "beta" ]))))

(deftest test-version-parse
  (is (= []     (version-parse "")))
  (is (= ["a"]  (version-parse "a")))
  (is (= [1]    (version-parse "1")))
  (is (= [1 33] (version-parse "1.33")))
  (is (= [1 0 :- 1]         (version-parse "1.0-1"))) ;FIXME: Invalid?
  (is (= [1 0 :- "alpha" 1] (version-parse "1.0-alpha.1"))))

(deftest test-vstr-check-ws
  (is (vstr-check-ws "1.3"))
  (is (thrown-with-msg? IllegalArgumentException #"'1.3\[ \\n\]'$"
        (vstr-check-ws "1.3 \n")))
  (is (thrown-with-msg? IllegalArgumentException #"'\[\\t\\t\]1.3'$"
        (vstr-check-ws "\t\t1.3 \n")))
  (is (thrown-with-msg? IllegalArgumentException #"'1.3\[ \]4.9'$"
        (vstr-check-ws "1.3 4.9"))))

;; FIXME: replace these

(defn o>= [v1 v2]
  (>= (version-compare v1 v2) 0))

(defn o< [v1 v2]
  (< (version-compare v1 v2) 0))

(defn o!= [v1 v2]
  (not (= (version-compare v1 v2) 0)))

(deftest test-numeric-comparable
  (is (not (o>= [0 1] [0 2])))
  (is (not (o>= [0 2] [0 2 1])))
  (is (o>= [0 2] [0 2]))
  (is (o>= [0 2 1] [0 2]))
  (is (o>= [0 3] [0 2])))

(def package
  {:name "a"
   :versions [{:version [1 0]
               :deps {"b" [ #(o>= % [0 3]) #( o< % [1]) ]}}]})

(defn- matches? [dep preds]
  (every? identity (map #(% dep) preds)))

(deftest test-range
  (let [preds ((:deps (first (:versions package))) "b")]
    (is (matches? [0 4] preds))))
