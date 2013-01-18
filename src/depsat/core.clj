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

(defn package-key [p]
  (p :name))

(defn versions-to-map [versions]
  (if (not (map? versions))
    (zipmap (map :version versions) versions)
    versions))

(defn merge-package-versions [oldp newp]
  (into (or (get oldp :versions) (sorted-map-by version-compare))
        (versions-to-map (get newp :versions))))

(defn merge-package [p]
  (dosync
   (alter *packages*
          (fn [tpacks]
            (let [old-pack (tpacks (package-key p))]
              (assoc tpacks
                (package-key p)
                (assoc
                  ;;; In general, new package p replaces old-pack values
                  (merge old-pack p)
                  ;;; But versions are merged as a sorted-map, with
                  ;;; version keys
                  :versions
                  (merge-package-versions old-pack p))))))))

(defn get-package
  ([name] (@*packages* name))
  ([name version] (get (:versions (@*packages* name)) version)))

(defn clear-packages []
  (dosync (alter *packages* empty)))
