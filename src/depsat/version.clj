(ns depsat.version
  (:use (clojure [string :only (split)])))

(defn segment-compare [s1 s2]
  (let [z1 (or s1 0)
        z2 (or s2 0)]
    (if (number? z1)
      (if (number? z2)
        (- z1 z2)
        1)
      (if (number? z2)
        -1
        (compare z1 z2)))))

;; FIXME: future doc: works as expected for non-equal lengths, unlike compare
(defn version-bare-compare [v1 v2]
  (let [f1 (first v1)
        f2 (first v2)
        c (segment-compare f1 f2)]
    (if (and (zero? c) (or f1 f2))
      (recur (next v1) (next v2))
      c)))

(defn version-split-pre [v]
  (let [s (split-with (partial not= :-) v)]
    (list (first s) (next (second s)))))

(defn version-compare [v1 v2]
  (let [[bv1 pv1] (version-split-pre v1)
        [bv2 pv2] (version-split-pre v2)
        c (version-bare-compare bv1 bv2)]
    (if (zero? c)
      (if (nil? pv1)
        (if (nil? pv2)
          0
          1)
        (if (nil? pv2)
          -1
          (version-bare-compare pv1 pv2)))
      c)))

(defn segment-parse [s]
  (if (re-matches #"^\d+$" s)
    (Integer/parseInt s)
    (if (empty? s)
      nil
      s)))

(defn version-parse [^String vstr]
  (let [[b & ps] (split vstr #"-")
        bv (map segment-parse (split b #"\."))
        pvs (map segment-parse (flatten (map #(split % #"\.") ps)))]
    (if (not (empty? pvs))
      (concat bv (cons :- pvs))
      bv)))
