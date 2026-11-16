(ns cljplot2.demo
  "Demo runner: renders the `images` of the demos (`cljplot2.demo.<name>`, see [[demos]]) to
  `results/<name>/<file-name>`, all in one JVM.

  CLI: `clojure -M:demo` (every demo), `clojure -M:demo step8 flip` (chosen demos),
  `clojure -M:demo step11/fig-5-6-800x700.png` (one image). REPL: `(render!)`, `(render! \"step8\" ...)`."
  (:require [clojure.string :as str]
            [clojure2d.core :as c2d]
            [cljplot2.core :as plot]))

(def demos
  "Demo names (namespaces `cljplot2.demo.<name>`), in run order: build steps, then capability demos."
  (into (mapv #(str "step" %) (range 1 12)) ["flip" "positions" "strips" "annotation" "helpers" "smooth" "evolution" "bar" "patterns" "polar" "parallel"]))

(defn- demo-images
  "`images` of the demo `demo` (its namespace is loaded on first use)."
  [demo]
  @(requiring-resolve (symbol (str "cljplot2.demo." demo) "images")))

(defn- unknown!
  [what value known]
  (throw (ex-info (str "Unknown " what " " (pr-str value) ", known: " (str/join ", " known) ".")
                  {:key what :value value :known known})))

(defn- selection
  "`[demo file-name-or-nil]` of a CLI name: `<demo>` (all its images) or `<demo>/<file-name>` (one image)."
  [name]
  (let [[demo file-name :as parts] (str/split name #"/" -1)]
    (when-not (and (<= 1 (count parts) 2) (some #{demo} demos))
      (unknown! :demo name demos))
    (when (and (= 2 (count parts)) (not (some #{file-name} (map first (demo-images demo)))))
      (unknown! :image name (map #(str demo "/" (first %)) (demo-images demo))))
    [demo file-name]))

(defn images
  "`[path plot options]` of the images selected by `names` (every demo when empty), in demo order, each once.
  Throws `ex-info` for an unknown demo or image before anything is rendered."
  [names]
  (let [selected (map selection names)
        whole (set (keep (fn [[demo file-name]] (when-not file-name demo)) selected))
        single (set (filter second selected))
        chosen? (fn [demo file-name] (or (empty? selected) (whole demo) (single [demo file-name])))]
    (vec (for [demo demos
               :when (or (empty? selected) (some #(= demo (first %)) selected))
               [file-name p options] (demo-images demo)
               :when (chosen? demo file-name)]
           [(str "results/" demo "/" file-name) p options]))))

(defn render!
  "Render and save the images selected by `names` (see [[images]]); returns the saved paths."
  [& names]
  (let [start (System/nanoTime)
        paths (mapv (fn [[path p options]]
                      (c2d/save (plot/render p options) path)
                      (println "saved" path)
                      path)
                    (images names))]
    (println (format "%d images in %.1f s" (count paths) (/ (- (System/nanoTime) start) 1e9)))
    paths))

(defn -main [& args]
  (try (apply render! args)
       (shutdown-agents)
       (catch clojure.lang.ExceptionInfo e
         (binding [*out* *err*] (println (ex-message e)))
         (shutdown-agents)
         (System/exit 1))))
