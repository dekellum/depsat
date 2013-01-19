(ns depsat.version
  (:use (clojure [string :only (split)])))

(defn segment-compare [s1 s2]
  "Compares two version segments, returning [POSINT,0,NEGINT] as per
  compare. Numeric segments are compared numerically and take
  precedence over non-numeric segments which are compared
  lexicographically"
  (let [z1 (or s1 0)
        z2 (or s2 0)]
    (if (number? z1)
      (if (number? z2)
        (- z1 z2)
        1)
      (if (number? z2)
        -1
        (compare z1 z2)))))

(defn version-bare-compare [v1 v2]
  "Comparator of parsed version segment sequences, applying
  segment-compare to each segment from left to right. Handles versions
  of non-equal segment length as if the shorter version sequence is
  right padded with zero (0). This function does not consider
  pre-release segments, see version-compare for that."
  (let [f1 (first v1)
        f2 (first v2)
        fcomp (segment-compare f1 f2)]
    (if (and (zero? fcomp) (or f1 f2))
      (recur (next v1) (next v2))
      fcomp)))

(defn version-split-pre [v]
  "Splits sequence v into a list of two sequences, the first
  containing all segments before any pre-release suffix token `:-`, and
  the second containing any pre-release segments, minus the `:-` token."
  (let [s (split-with (partial not= :-) v)]
    (remove nil? (list (first s) (next (second s))))))

(defn version-compare [v1 v2]
  "Comparator of parsed version sequences. First the base version
  segments are compared via version-bare-compare, where any missing
  segments are treated as equal to zero (0). If these are equal,
  presence of a pre-release suffix is considered less-than a
  non-pre-release.  If both have pre-releases suffixes then these are
  in turn compared via version-bare-compare."
  (let [[base-v1 pre-v1] (version-split-pre v1)
        [base-v2 pre-v2] (version-split-pre v2)
        base-comp (version-bare-compare base-v1 base-v2)]
    (if (zero? base-comp)
      (if (nil? pre-v1)
        (if (nil? pre-v2)
          0
          1)
        (if (nil? pre-v2)
          -1
          (version-bare-compare pre-v1 pre-v2)))
      base-comp)))

(def dep-fn-map
  {:gt  >,
   :gte >=,
   :lt  <,
   :lte <=,
   :eq  =,
   :ne  not=})

(defn matches-deps? [version deps]
  "True, if the sequence of dependency qualifiers deps all match the
  provided version."
  (reduce (fn [memo dep]
            (and memo
                 ((dep-fn-map (first dep))
                  (version-compare version (second dep)) 0))) true deps))

(defn segment-parse [s]
  "Tests if a string segment is numeric and if so returns it as a
  parsed integer. Also replaces an empty segment with nil. Otherwise
  returns s unmodified."
  (if (string? s)
    (if (re-matches #"^\d+$" s)
      (Integer/parseInt s)
      (if (empty? s)
        nil
        s))
    s))

(defn map-segments [segs]
  "Maps sequence via segment-parse and removes any nil segments."
  (remove nil? (map segment-parse segs)))

(defn version-parse [^String vstr]
  "Parses version String by splitting on '-' for a pre-release suffix
  and then splitting on '.' and applying segment-parse to all
  segments.  Returns a sequence of version segments with the `:-`
  Keyword prepended to any trailing pre-release segments, i.e.
  `[1 0 :- \"alpha\" 1]`. This parse is generally lenient.  Whitespace
  or other normalization/validation should be handled externally."
  (let [[base & pre] (split vstr #"-")
        base-vs (map-segments (split base #"\."))
        pre-vs  (map-segments (flatten (map #(split % #"\.") pre)))]
    (if (not (empty? pre-vs))
      (concat base-vs (cons :- pre-vs))
      base-vs)))

(defn- subcs ^CharSequence
  ([^CharSequence s start]
     (subcs s start (.length s)))
  ([^CharSequence s start end]
     (let [rend (if (neg? end) (+ (.length s) end) end)]
       (.subSequence s start rend))))

(defn vstr-check-ws [^String vstr]
  (let [ws (re-find #"(?m)^(\S*)(\s+)(\S*)" vstr)]
    (if ws
      (throw (IllegalArgumentException.
              (format "Version string contains whitespace: '%s[%s]%s'"
                      (ws 1)
                      (subcs (pr-str (ws 2)) 1 -1)
                      (ws 3))))
      true)))
