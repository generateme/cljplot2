(ns dev
  "Builds the cljplot2 notebook book (Clay + Quarto) from `notebooks/` into `docs/` (wiped first).
  Settings live in `clay.edn`, the chapter order in `notebooks/chapters.edn`.

  CLI: `clojure -M:clay` (build), `clojure -M:clay serve [port]` (build, then Clay serves `docs/` until stopped,
  default http://localhost:1971/). REPL (`:clay` alias): `(make-book!)`, `(make-book! {:show true})`."
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
  "Render the book with Clay; `options` are merged over the defaults (no browser, no server; `{:show true}` starts
  Clay's server on `docs/`, `:port` chooses its port).
  Returns Clay's summary."
  ([] (make-book! {}))
  ([options]
   (clay/make! (merge {:source-path (chapters)
                       :book {:title "cljplot2"}
                       :show false
                       :browse false}
                      options))))

(defn- port-free?
  "Can a server bind `port` now?"
  [port]
  (try (with-open [_ (java.net.ServerSocket. (int port))] true)
       (catch java.io.IOException _ false)))

(defn- parse-args
  "`{:serve? :port}` of the CLI `args`: none (build only), `serve` or `serve <port>` (build, then serve with Clay;
  `:port` nil - Clay picks the first free port from 1971). Throws `ex-info` for anything else."
  [args]
  (let [usage "Usage: clojure -M:clay [serve [port]]"
        [command port & more] args
        port-number (when port (parse-long port))]
    (cond
      (empty? args) {:serve? false}
      (or (not= "serve" command) (seq more)) (throw (ex-info (str "Unknown arguments " (pr-str args) ". " usage)
                                                             {:key :args :value args}))
      (nil? port) {:serve? true}
      (and port-number (<= 1 port-number 65535)) {:serve? true :port port-number}
      :else (throw (ex-info (str "Port should be a number 1-65535, got " (pr-str port) ". " usage)
                            {:key :port :value port})))))

(defn- exit!
  [code message]
  (shutdown-agents)
  (when message (binding [*out* *err*] (println message)))
  (System/exit code))

(defn -main
  "Build the book; with `serve [port]` keep Clay's server running on `docs/` until stopped (Ctrl-C)."
  [& args]
  (let [{:keys [serve? port]} (try (parse-args args) (catch clojure.lang.ExceptionInfo e (exit! 1 (ex-message e))))
        _ (when (and port (not (port-free? port))) ;; fail before the build wipes docs/
            (exit! 1 (str "Port " port " is in use.")))
        chapters (chapters)
        {:keys [info]} (make-book! (if serve? (cond-> {:show true} port (assoc :port port)) {}))
        ;; Clay reports a failing notebook of a multi-chapter book only by printing it; its result is then nil
        failed (count (filter nil? (take (chapter-count chapters) info)))]
    (cond
      (pos? failed) (exit! 1 (str failed " notebook(s) failed, see the Clay output above."))
      (not serve?) (exit! 0 nil)
      :else (println "Serving the book (Ctrl-C to stop)."))))
