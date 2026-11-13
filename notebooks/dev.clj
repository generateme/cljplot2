(ns dev
  "Builds the cljplot2 notebook book (Clay + Quarto) from `notebooks/` into `docs/` (wiped first).
  Settings live in `clay.edn`, the chapter order in `notebooks/chapters.edn`.

  CLI: `clojure -M:clay`. REPL (`:clay` alias): `(make-book!)`, or `(make-book! {:show true})` to open it."
  (:require [clojure.edn :as edn]
            [scicloj.clay.v2.api :as clay]))

(defn chapters
  "Book chapters from `notebooks/chapters.edn`: notebook paths (relative to `notebooks/`) and
  `{:part title :chapters [path ...]}` parts, in reading order; parts without chapters are left out."
  []
  (into [] (remove #(and (map? %) (empty? (:chapters %))))
        (edn/read-string (slurp "notebooks/chapters.edn"))))

(defn- chapter-count
  [chapters]
  (reduce + (map #(if (map? %) (count (:chapters %)) 1) chapters)))

(defn make-book!
  "Render the book with Clay; `options` are merged over the defaults (no browser, no server).
  Returns Clay's summary."
  ([] (make-book! {}))
  ([options]
   (clay/make! (merge {:source-path (chapters)
                       :book {:title "cljplot2"}
                       :show false
                       :browse false}
                      options))))

(defn -main [& _]
  (let [chapters (chapters)
        {:keys [info]} (make-book!)
        ;; Clay reports a failing notebook of a multi-chapter book only by printing it; its result is then nil
        failed (count (filter nil? (take (chapter-count chapters) info)))]
    (shutdown-agents)
    (when (pos? failed)
      (binding [*out* *err*] (println failed "notebook(s) failed, see the Clay output above."))
      (System/exit 1))
    (System/exit 0)))
