(ns column-printer.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  (:gen-class))

(defn pad-spaces
  "Creates a list of spaces to pad a line of words"
  [total-spaces len]
  (letfn [(increment-and-rotate
            [nums]
            (concat (rest nums) (list (inc (first nums)))))
          (spaces
            [number-of-spaces]
            (apply str (repeat number-of-spaces " ")))]
    (if (= len 0) (spaces total-spaces)
        (let [group-size (quot total-spaces len)
              left-over (rem total-spaces len)
              num-spaces (repeat len group-size)]
          (if (= left-over 0)
            (map spaces num-spaces)
            (map spaces (last (take (inc left-over) (iterate increment-and-rotate num-spaces)))))))))

(defn justify-column
  "Pads a column with spaces until it is column-length long"
  [column-length, line]
  (letfn [(interleave
            [list-a list-b]
            (if (empty? list-b) list-a
                (cons (first list-a) (cons (first list-b) (interleave (rest list-a) (rest list-b))))))]
    (let [line-length (count (apply str line))
          amount-of-padding (- column-length line-length)
          padding (pad-spaces amount-of-padding (dec (count line)))]
      (str/join "" (interleave line padding)))))

(defn index-at-point
  "Helper function that finds the index of the last whole word at a given point"
  [point line]
  (letfn [(find-index-iterator
            [index len lst]
            (cond
              (= (dec len) point) index
              (> len point) (dec index)
              (empty? lst) 0
              :else (find-index-iterator (inc index)
                                     (inc (+ len (count (first lst))))
                                     (rest lst))))]
    (if (and (= 1 (count line)) (<= point (count (first line))))
      0
      (find-index-iterator 0 0 line))))

(defn column-stream
  "Takes a list of lines and a column lengths, and streams those lines broken up into lines column-length long"
  [column-length lines]
  (if (empty? lines)
    '()
    (let [first-word (first (first lines))]
      (if (> (count first-word) column-length)
        ; This first expression provided to "if" handles the special case where a word is smaller than the column size.
        ; In that case, we don't want word wrap, and instead split the word to fit in the column size and then continue.
        (let [split-word-first (str/join "" (first (split-at column-length first-word)))
              split-word-second (str/join "" (second (split-at column-length first-word)))
              other-words-in-line (rest (first lines))]
          (if (empty? other-words-in-line)
            (lazy-seq (cons (list split-word-first)
                            (column-stream column-length (cons (list split-word-second) (rest lines)))))
            (lazy-seq (cons (list split-word-first)
                            (column-stream column-length (cons (list split-word-second) (cons other-words-in-line (rest lines))))))))
        ; now that the special case is handled, this takes care of the word wrap processing.
        (let [index (index-at-point column-length (first lines))
              column-split (split-at index (first lines))
              first-column (first column-split)
              next-column (second column-split)]
          (if (empty? first-column)
            (lazy-seq (cons next-column (column-stream column-length (rest lines))))
            (lazy-seq (cons first-column (column-stream column-length (cons next-column (rest lines)))))))))))

(defn file-justified-column-stream
  "Opens a text file and creates a stream of word-wrapped lines (column-length long)"
  [column-length filepath]
  (letfn [(split
            ; "this exists only to re-arrange the parameters to str/split for partial application. There's probably a better way to do this."
            [char line]
            (str/split line (re-pattern char)))]
    (let [stream-file ; This function is just composed from the above functions into something useful.
          (comp
           (partial map (partial justify-column column-length))
           (partial filter not-empty) ; TODO: figure out why some empty elements are sneaking into the sequence
           (partial column-stream column-length)
           (partial map (partial split " ")))]
      (stream-file (line-seq (io/reader filepath))))))

(defn line-stream
  "Takes a collection of column streams and combines them into a stream of lines separated by a delimeter"
  [column-length delimeter column-streams]
  (let [default-line (apply str (repeat column-length " "))]
    (letfn [(first-or-line
              [lst]
              (if (empty? lst) default-line (first lst)))
            (rest-or-empty
              [lst]
              (if (empty? lst) '() (rest lst)))]
      (if (every? empty? column-streams) ; if true, the line stream is empty.
        '()
        (lazy-seq (cons (str/join delimeter
                                  (map first-or-line column-streams))
                        (line-stream column-length
                                     delimeter
                                     (map rest-or-empty column-streams))))))))

(defn -main
  [& args]
  (let [line-length (read-string (first args))
        file-paths (rest args)
        column-length (quot line-length (count file-paths))
        column-streams (map (partial file-justified-column-stream column-length) file-paths)
        output-lines (line-stream column-length " | " column-streams)]
    (doseq [line output-lines]
       (println line))))
