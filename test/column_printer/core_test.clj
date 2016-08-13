(ns column-printer.core-test
  (:require [clojure.test :refer :all]
            [column-printer.core :refer :all]))

(deftest test-index-at-point
  (testing "index doesn't increment when point is inside a word"
    (let [words '("There" "was" "a" "ripple" "of" "chimes" "through" "the" "frosty" "air" "as" "Catherine")
          index (index-at-point 3 words)]
      (is (= index 0))))
  (testing "index increments when point is at the last character of a word"
    (let [words '("There" "was" "a" "ripple" "of" "chimes" "through" "the" "frosty" "air" "as" "Catherine")
          index (index-at-point 5 words)]
      (is (= index 1))))
  (testing "index increments when point is one after last character of a word"
    (let [words '("There" "was" "a" "ripple" "of" "chimes" "through" "the" "frosty" "air" "as" "Catherine")
          index (index-at-point 6 words)]
      (is (= index 1))))
  (testing "index increments to two when pointed at the last character of the second word"
    (let [words '("There" "was" "a" "ripple" "of" "chimes" "through" "the" "frosty" "air" "as" "Catherine")
          index (index-at-point 9 words)]
      (is (= index 2))))
  (testing "index is at two when point is at the character after the second word."
    (let [words '("There" "was" "a" "ripple" "of" "chimes" "through" "the" "frosty" "air" "as" "Catherine")
          index (index-at-point 10 words)]
      (is (= index 2))))
  (testing "index is zero when point is past the end of the line"
    (let [words '("There" "was" "a" "ripple" "of" "chimes" "through" "the" "frosty" "air" "as" "Catherine")
          index (index-at-point 80 words)]
      (is (= index 0)))))

(deftest test-column-stream
  (testing "it breaks up long lines into smaller columns"
    (let [lines '(("This" "is" "a" "line" "longer" "than" "the" "column" "width.") ("This" "isn't."))
          stream (column-stream 15 lines)]
      (is (= 4 (count stream)))
      (is (= stream '(("This" "is" "a" "line") ("longer" "than" "the") ("column" "width.") ("This" "isn't."))))))
  (testing "it doesn't word wrap when column length is smaller than word"
    (let [lines '(("pneumonoultramicroscopicsilicovolcanoconiosis") ("is" "a" "lung" "disease"))
          stream (column-stream 10 lines)]
      (is (= 7 (count stream)))
      (is (= stream '(("pneumonoul") ("tramicrosc") ("opicsilico") ("volcanocon") ("iosis") ("is" "a" "lung") ("disease")))))))

(deftest test-justify-column
  (testing "it inserts spaces to reach the desired column length"
    (let [line '("clojure" "is" "pretty" "cool")
          padded-string (justify-column 26 line)]
      (is (= 26 (count padded-string)))
      (is (= padded-string "clojure  is  pretty   cool"))))
  (testing "it pads a blank line"
    (let [line '("")
          padded-string (justify-column 25 line)]
      (is (= 25 (count padded-string)))
      (is (= padded-string "                         ")))))
