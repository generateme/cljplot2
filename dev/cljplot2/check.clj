(ns cljplot2.check
  "Load every `cljplot2.*` namespace under `src/`; reflection and boxed-math warnings are printed while loading."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- file->ns-symbol
  [^java.io.File file]
  (-> (.getPath file)
      (str/replace #"^src/" "")
      (str/replace #"\.clj$" "")
      (str/replace "/" ".")
      (str/replace "_" "-")
      symbol))

(defn -main [& _]
  (doseq [ns-sym (->> (file-seq (io/file "src"))
                      (filter #(str/ends-with? (.getName ^java.io.File %) ".clj"))
                      (map file->ns-symbol)
                      sort)]
    (println "loading" ns-sym)
    (require ns-sym))
  (shutdown-agents))
