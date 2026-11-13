(ns cljplot2.demo
  "Demo runner: renders the `images` of the step demos (`cljplot2.demo.stepN`) to `results/stepN/<file-name>`,
  all in one JVM.

  CLI: `clojure -M:demo` (every step), `clojure -M:demo step8 step11` (chosen steps),
  `clojure -M:demo step11/fig-5-6-800x700.png` (one image). REPL: `(render!)`, `(render! \"step8\" ...)`."
  (:require [clojure.string :as str]
            [clojure2d.core :as c2d]
            [cljplot2.core :as plot]))

(def steps
  "Step demo names, in run order."
  (mapv #(str "step" %) (range 1 12)))

(defn- step-images
  "`images` of the step demo `step` (its namespace is loaded on first use)."
  [step]
  @(requiring-resolve (symbol (str "cljplot2.demo." step) "images")))

(defn- unknown!
  [what value known]
  (throw (ex-info (str "Unknown " what " " (pr-str value) ", known: " (str/join ", " known) ".")
                  {:key what :value value :known known})))

(defn- selection
  "`[step file-name-or-nil]` of a CLI name: `stepN` (whole step) or `stepN/<file-name>` (one image)."
  [name]
  (let [[step file-name :as parts] (str/split name #"/" -1)]
    (when-not (and (<= 1 (count parts) 2) (some #{step} steps))
      (unknown! :step name steps))
    (when (and (= 2 (count parts)) (not (some #{file-name} (map first (step-images step)))))
      (unknown! :image name (map #(str step "/" (first %)) (step-images step))))
    [step file-name]))

(defn images
  "`[path plot options]` of the images selected by `names` (every step when empty), in step order, each once.
  Throws `ex-info` for an unknown step or image before anything is rendered."
  [names]
  (let [selected (map selection names)
        whole (set (keep (fn [[step file-name]] (when-not file-name step)) selected))
        single (set (filter second selected))
        chosen? (fn [step file-name] (or (empty? selected) (whole step) (single [step file-name])))]
    (vec (for [step steps
               :when (or (empty? selected) (some #(= step (first %)) selected))
               [file-name p options] (step-images step)
               :when (chosen? step file-name)]
           [(str "results/" step "/" file-name) p options]))))

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
