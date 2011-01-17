(ns com.quephird.evolution.main)

(def *width* 75)
(def *height* 30)
(def *entire-map* [0 0 *width* *height*])
(def *jungle* [45 10 10 10])

(def *plant-energy* 80)

(def *reproduction-energy* 200)
(def *animal-translations*
  [[-1 -1] [0 -1] [1 -1] [1 0] [1 1] [0 1] [-1 1] [-1 0]])

(def *plants* (atom {}))

(defn plant-exists? [x y]
  (if (@*plants* [x y])
    true
    false))

(def *animals* (atom []))

(defn animal-exists? [x y]
  (if (> (count (filter #(and (= x (first (:position @%)))
                              (= y (second (:position @%)))) @*animals*)) 0)
    true
    false))

(defn random-plant [region-x region-y region-width region-height]
  (let [plant-pos [(+ region-x (rand-int region-width))
                   (+ region-y (rand-int region-height))]]
    (swap! *plants* assoc plant-pos true)))

(defn add-plants []
  (apply random-plant *jungle*)
  (apply random-plant *entire-map*))


(defstruct animal :position :energy :direction :genes)
(defn make-animal [position-x position-y energy direction]
  (struct animal [position-x position-y] energy direction (vec (map (fn [_] (rand-int 10)) (take 8 (repeat nil))))))

(defn add-animal []
  (reset! *animals* [(atom (make-animal (quot *width* 2) (quot *height* 2) 1000 0))]))


(defn move [a]
  (let [translation (*animal-translations* (:direction @a))]
    (swap! a 
      (fn[_]
        (update-in 
          (update-in @a [:position] #(map + % translation))
          [:energy] dec)))))

(defn turn [a]
  (let [gs (:genes @a)
        x (rand-int (apply + gs))]
    (letfn [(locate-gene [genes x]
              (let [diff (- x (first genes))]
                (if (<= diff 0)
                  0
                  (inc (locate-gene (rest genes) diff)))))]
      (swap! a (fn [_] (assoc-in @a [:direction] (locate-gene gs x)))))))

(defn eat [a]
  (let [animal-position (:position @a)]
    (when (@*plants* animal-position)
      (swap! a (fn[_] (update-in @a [:energy] + *plant-energy*)))
      (swap! *plants* dissoc animal-position))))

; The big difference here is that there is no explicit copy-structure call
; in Clojure. Data structures are immutable in Clojure and functions like 
; assoc-in and update-in always return copies, so we take advantage of them
; here.

(defn reproduce [a]
  (let [e (:energy @a)]
    (when (>= e *reproduction-energy*)
      (swap! a (fn[_] (update-in @a [:energy] #(quot % 2))))
      (let [gene-idx (rand-int 8)
            original-genes (:genes @a)
            mutated-genes (assoc-in original-genes [gene-idx] (max 1 (+ (original-genes gene-idx) (rand-int 3) -1)))]
        (reset! *animals* (conj @*animals* (atom (assoc-in @a [:genes] mutated-genes))))))))


(defn update-world []
  (reset! *animals* (filter #(> (:energy @%) 0) @*animals*))
  (doseq [a @*animals*] (do (turn a) (move a) (eat a) (reproduce a)))
  (add-plants))


(defn draw-cell [x y]
  (cond 
    (animal-exists? x y) (print "M")
    (plant-exists? x y) (print "*")
    :else (print " ")))

(defn draw-world []
  (dotimes [y *height*]
    (do
      (print "|")
      (dotimes [x *width*]
        (do
          (draw-cell x y)))
      (println "|"))))

(defn init-world []
  (add-plants)
  (add-animal))

(defn evolution []
  (init-world)
  (loop [dummy (read-line)]
    (draw-world)
    (update-world)
    (recur (read-line))))
