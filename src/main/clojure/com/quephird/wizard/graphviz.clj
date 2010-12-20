(ns com.quephird.wizard.graphviz
  (:import (java.io BufferedWriter FileWriter))
)

(def *max-label-length* 30)

(defn dot-name [expr]
  (.replaceAll (re-matcher #"\W+" (str expr)) "_"))

(defn dot-label [expr]
  (if expr
    (let [s (str expr)]
      (if (> (count s) *max-label-length*)
        (str (subs s 0 (- *max-label-length* 3)) "...")
        s))
    ""))

(defn nodes->dot [nodes]
  (apply str
    (map #(str
            "\r\n"
            (dot-name (first %))
            "[label=\""
            (dot-label %)
            "\"];")
       nodes)))

(defn edges->dot [edges]
  (apply str
    (map (fn [node]
      (apply str
        (map (fn [edge]
               (str "\r\n"
                    (dot-name (first node))
                    "->"
                    (dot-name (first edge))
                    "[label=\""
                    (dot-label (rest edge))
                    "\"];"
               ))
           (edges (first node)))))
         edges)))

(defn graph->dot [nodes edges]
  (apply str
    "digraph{"
    (nodes->dot nodes)
    (edges->dot edges)
    "\r\n}"))

(defn buffered-writer [filename]
  (BufferedWriter. (FileWriter. filename))
)

(defn create-dot-file [filename nodes edges]
  (with-open [wtr (buffered-writer filename)]
    (.write wtr (graph->dot nodes edges))))
