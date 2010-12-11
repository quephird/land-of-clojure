(ns com.quephird.wizard)

(defn third [list]
  (first (next (next list))))

(def *nodes*
  {:living-room '(You are in the living-room. A wizard is snorting loudly on the couch.),
   :garden '(You are in a beautiful garden. There is a well in front of you.),
   :attic '(You are in the attic. There is a giant welding torch in the corner.)})

(def *edges*
  {:living-room '((:garden :west :door) (:attic :upstairs :ladder)),
   :garden '((:living-room :east :door)),
   :attic '((:living-room :downstairs :ladder))})

(def *objects* '(:whiskey-bottle :bucket :frog :chain))

(def *object-locations*
  (atom
    {:whiskey-bottle :living-room,
     :bucket :living-room,
     :chain :garden,
     :frog :garden}))

(def *current-location*
  (atom :living-room))

(defn describe-location [location nodes]
  (nodes location))

(defn describe-path [edge]
  (concat '(There is a) (list (third edge)) '(going) (list (second edge)) '(from here.)))

(defn describe-paths [location edges]
  (apply concat (map describe-path (edges location))))

(defn describe-object [object]
  (concat '(You see a) (list object) '(on the floor.)))

(defn objects-at [location objects object-locations]
  (filter #(= (object-locations %) location) objects))

(defn describe-objects [location objects object-locations]
  (apply concat (map describe-object (objects-at location objects object-locations))))

(defn look []
  (concat
    (describe-location (deref *current-location*) *nodes*)
    (describe-paths (deref *current-location*) *edges*)
    (describe-objects (deref *current-location*) *objects* (deref *object-locations*))))

(defn walk [direction]
  (let [next-edge (first (filter #(= direction (second %)) (*edges* (deref *current-location*))))]
    (if (not (empty? next-edge))
        (do (reset! *current-location* (first next-edge))
            (look))
        '(You cannot go that way.))))

(defn pickup [object]
  (cond
    (some #{object} (objects-at (deref *current-location*) *objects* (deref *object-locations*)))
      (do
        (swap! *object-locations* assoc object :body)
        (list 'You 'are 'now 'carrying 'the `~object))
    :else
      '(You cannot get that.)))

; Ugh... can't use drop here as there is already a drop in clojure.core.
(defn putdown [object]
  (cond
    (some #{object} (objects-at :body *objects* (deref *object-locations*)))
      (do
        (swap! *object-locations* assoc object (deref *current-location*))
        (list 'You 'no 'longer 'have 'the `~object))
    :else
      '(You do not have that.)))

(defn inventory []
  (objects-at :body *objects* (deref *object-locations*)))
