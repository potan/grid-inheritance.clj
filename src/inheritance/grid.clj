
(ns inheritance.grid)
(use 'clojure.set)

(defmulti subobj? "Subset or subarraymap" (fn [l r] [(type l) (type r)]))

(defmethod subobj?
     [clojure.lang.PersistentHashSet clojure.lang.PersistentHashSet]
     [l r] (clojure.set/subset? l r))

(defmethod subobj?
     [clojure.lang.PersistentArrayMap clojure.lang.PersistentArrayMap]
     [l r] (every? (fn [[k v]] (= v (r k))) l))

(defn register-grid-node "Register node in hierarchy" [h o]
  (let [nl (get (meta h) :grid-hierarchy-cache {})]
   (if-let [s (nl o)]
     [h s]
     (let [s (symbol (str o))
           hn (reduce (fn [h [tr n]]
                  (if (and (subobj? tr o) (not (isa? h s n)))
                    (derive h s n)
                    (if (and (subobj? o tr) (not (isa? h n s)))
                      (derive h n s)
                      h)))
                h nl)]
        [(with-meta hn
                    (assoc (or (meta h) {})
                         :grid-hierarchy-cache (assoc nl o s)))
         s]))))

(defn make-grid-hierarchy "Make new hierarchy of grid-nodes" []
   (let [h (make-hierarchy)]
    (with-meta h (assoc (or (meta h) {}) :grid-hierarchy-cache {}))))

(defn get-grid-node "Get (or generate and register) symbol name of node" [n hv]
   (let [sa (atom nil)]
     (alter-var-root hv (fn [ho]
                         (let [[hn s] (register-grid-node ho n)]
                           (swap! sa (fn [_] s))
                           hn)))
     @sa))

(defn with-grid-node "Mark data by node" [n h]
  (let [s (get-grid-node n h)]
   (fn [v]
      (with-meta v (assoc (or (meta v) {}) :grid-node s)))))

(defn grid-dispatch "Make dispatcher by all args"
        [] (fn [& v] (vec (map (fn [a] (:grid-node (meta a))) v))))
(defn grid-dispatch1 "Make dispatcher by first arg"
        [] (fn [v & _] (:grid-node (meta v))))

