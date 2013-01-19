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

(defn versions-to-map [versions]
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
