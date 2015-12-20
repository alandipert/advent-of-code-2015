#!/usr/bin/env boot

(require '[clojure.java.io :as io]
         '[boot.util       :as util]
         '[boot.cli        :refer [defclifn]])
(import 'clojure.lang.LineNumberingPushbackReader)

(defn read-forms
  "Reads Clojure data from a file and returns a vector of forms."
  [path]
  (with-open [rdr (LineNumberingPushbackReader. (io/reader path))]
    (loop [forms []]
      (let [f (read rdr false ::eof)]
        (if (= f ::eof)
          forms
          (recur (conj forms f)))))))

(defn parse-input
  "Parses today's input file into a map."
  [path]
  (let [forms        (read-forms path)
        replacements (->> (partition 3 forms)
                          (mapcat (juxt first last))
                          (map str)
                          (partition 2))]
    {:rules replacements
     :input (str (last forms))}))

(defn indexes-of
  "Returns a vector of the indexes in s where patt appears."
  [s patt]
  (let [patt-size (count patt)]
    (loop [start 0, idxs []]
      (let [idx (.indexOf s patt start)]
        (if (>= idx 0)
          (recur (inc (+ start patt-size)) (conj idxs idx))
          idxs)))))

(defn replace-at
  "Replaces the character at index n in s1 with s2."
  [s1 n s2]
  (str (subs s1 0 n) s2 (subs s1 (inc n))))

(defn replacements
  "Returns the list of possible substitutions of patt/replacement in s"
  [s patt replacement]
  (map #(replace-at s % replacement) (indexes-of s patt)))

(defclifn -main [f file PATH str "Path to the input file."]
  (if file
    (let [{:keys [rules input]} (parse-input file)]
      (->> (mapcat #(apply replacements input %) rules)
           (into #{})
           count
           println))
    (util/info "file is a required argument.\n")))



