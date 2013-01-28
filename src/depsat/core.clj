(ns depsat.core
  (:import (org.sat4j.pb SolverFactory)
           (org.sat4j.core Vec VecInt))
  (:use    (clojure set)
           depsat.version))

;; http://files.opensuse.org/opensuse/en/b/b9/Fosdem2008-solver.pdf
;; http://cseweb.ucsd.edu/~rjhala/papers/opium.html
;; http://wiki.eclipse.org/Equinox_P2_Resolution

;; Sample package map:
;;   {:name "a"
;;    :versions { [1 0] {:version [1 0]
;;                       :deps {"b" [ [:gte [0 3]] [:lt [1]] ]}}}}

(def ^:dynamic *packages* (ref {}))

(defmacro with-local-packages [& body]
  `(binding [*packages* (ref {})]
    ~@body))

(defn package-key [pack]
  (pack :name))

(defn- versions-to-map [versions]
  (if (not (map? versions))
    (zipmap (map :version versions) versions)
    versions))

(defn merge-package-versions [old-pack pack]
  (into (or (get old-pack :versions) (sorted-map-by version-compare))
        (versions-to-map (get pack :versions))))

(defn merge-package
  ([pack]
     (dosync (alter *packages* (partial merge-package pack))))
  ([pack packages]
     (let [old-pack (packages (package-key pack))]
       (assoc packages
         (package-key pack)
         (assoc
           ;;; In general, new pack replaces old-pack values
           (merge old-pack pack)
           ;;; But versions are merged as a sorted-map, with
           ;;; version keys
           :versions
           (merge-package-versions old-pack pack))))))

(defn- reduce-packages [packs f]
  (reduce (fn [memo pack] (f pack))
          nil
          (if (map? packs) (vals packs) packs)))

(defn merge-packages
  ([packs]
     (dosync (reduce-packages packs #(merge-package %))))
  ([packs packages]
     (reduce-packages packs #(merge-package % packages))))

(defn get-package
  ([name] (@*packages* name))
  ([name version] (get (:versions (@*packages* name)) version)))

(defn clear-packages []
  (dosync (alter *packages* empty)))

(defn dep-to-known-versions [name criteria]
  (filter #(matches-deps? % criteria)
          (keys (get (get-package name) :versions))))

(defn dep-tree [deps]
  (reduce
   (fn [memo [name criteria]]
     (let [versions (dep-to-known-versions name criteria)]
       (concat
        (conj memo [:versions name versions])
        (filter (fn [[_ _ _ deps]] (not (nil? deps)))
                (map (fn [v] [:dep name v (dep-tree (get (get-package name v) :deps))]) versions))
        )))
   nil
   deps))

;; ( [:versions "a" ([1 0])]
;;   [:dep "a" [1 0] ( [:versions "d" ([1 0])]
;;                     [:versions "c" ([2 0] [2 1] [2 2])]
;;                     [:versions "b" ([1 0] [1 1] [1 2])]
;;                     [:dep "b" [1 0] ( [:versions "c" ([2 0])] )]
;;                     [:dep "b" [1 1] ( [:versions "c" ([2 1])])]
;;                     [:dep "b" [1 2] ( [:versions "c" ([2 2])])]
;;                     [:dep "d" [1 0] ( [:versions "c" ([2 1])])])])

;; ( [:versions "a" ([1 0])]
;;   [:dep "a" [1 0] ( [:versions "d" ([1 0])]
;;                     [:versions "c" ([2 0] [2 1] [2 2])]
;;                     [:versions "b" ([1 0] [1 1] [1 2])]
;;                     [:dep "b" [1 0] ( [:versions "c" ([2 0])]
;;                                       [:dep "c" [2 0] ([:versions "e" ([1 0])])])]
;;                     [:dep "b" [1 1] ([:versions "c" ([2 1])])]
;;                     [:dep "b" [1 2] ([:versions "c" ([2 2])])]
;;                     [:dep "c" [2 0] ([:versions "e" ([1 0])])]
;;                     [:dep "d" [1 0] ([:versions "c" ([2 1])])])])

;; --> flatten
;; ( [:versions "a" ([1 0])]
;;   [:dep "a" [1 0] ([:versions "d" ([1 0])]
;;                    [:versions "c" ([2 0] [2 1] [2 2])]
;;                    [:versions "b" ([1 0] [1 1] [1 2])] )]
;;   [:dep "b" [1 0] ([:versions "c" ([2 0])])]
;;   [:dep "b" [1 1] ([:versions "c" ([2 1])])]
;;   [:dep "b" [1 2] ([:versions "c" ([2 2])])]
;;   [:dep "c" [2 0] ([:versions "e" ([1 0])])]
;;   [:dep "d" [1 0] ([:versions "c" ([2 1])])]
;;   [:dep "c" [2 0] ([:versions "e" ([1 0])])] )

;; (((nil {:name "c", :versions ([2 1])})
;;   {:name "d", :versions ([1 0])} nil nil nil
;;   {:name "c", :versions ([2 0] [2 1] [2 2])}
;;   (nil {:name "c", :versions ([2 0])})
;;   (nil {:name "c", :versions ([2 1])})
;;   (nil {:name "c", :versions ([2 2])})
;;   {:name "b", :versions ([1 0] [1 1] [1 2])})
;;  {:name "a", :versions ([1 0])})))

;; (
;;  (
;;   (nil {:name "c", :versions ([2 1])})
;;   {:name "d", :versions ([1 0])}
;;   nil nil nil {:name "c", :versions ([2 0] [2 1] [2 2])}

;;   (nil {:name "c", :versions ([2 0])})
;;   (nil {:name "c", :versions ([2 1])})
;;   (nil {:name "c", :versions ([2 2])})
;;   {:name "b", :versions ([1 0] [1 1] [1 2])})
;;  {:name "a", :versions ([1 0])} )

;; ( ( nil {:name "c", :versions ([2 1])} )
;;   {:name "d", :versions ([1 0])}
;;   )

;; ([:dep "a" [1 0] ( [:dep "d" [1 0] ([:versions "c" ([2 1])])]
;;                    [:versions "d" ([1 0])]
;;                    [:versions "c" ([2 0] [2 1] [2 2])]
;;                    [:dep "b" [1 0] ([:versions "c" ([2 0])])]
;;                    [:dep "b" [1 1] ([:versions "c" ([2 1])])]
;;                    [:dep "b" [1 2] ([:versions "c" ([2 2])])]
;;                    [:versions "b" ([1 0] [1 1] [1 2])] )]
;;  [:versions "a" ([1 0])]))

(defn solver-from-deps [deps]
  (let [solver (SolverFactory/newLight)] ; or newDefault
    ))
