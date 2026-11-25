(ns build
  "Builds, installs and deploys the cljplot2 jar.

  clojure -T:build jar      ; target/cljplot2-<version>.jar with its pom
  clojure -T:build install  ; the jar into the local Maven repository (~/.m2)
  clojure -T:build deploy   ; the jar, signed, to Clojars
  clojure -T:build clean    ; remove target/

  Secrets stay outside the repository. Clojars credentials (the username and a deploy token) come from the
  environment variables `CLOJARS_USERNAME` and `CLOJARS_PASSWORD`, else from the `clojars` server entry of
  `~/.m2/settings.xml`. Deploy signs the jar and the pom with gpg's default key (the only secret key, or `default-key`
  in `~/.gnupg/gpg.conf`), asking for its passphrase."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.build.api :as b]
            [deps-deploy.deps-deploy :as dd]))

(def lib 'org.clojars.generateme/cljplot2)
(def version "0.1.0-SNAPSHOT")
(def ^:private class-dir "target/classes")
(def ^:private jar-file (format "target/%s-%s.jar" (name lib) version))

(def ^:private pom-data
  [[:description "A plotting library for Clojure: plots are plain maps of data, mappings and charts"]
   [:url "https://github.com/generateme/cljplot2"]
   [:licenses
    [:license
     [:name "EPL-2.0"]
     [:url "https://www.eclipse.org/legal/epl-2.0/"]]]
   [:scm
    [:url "https://github.com/generateme/cljplot2"]
    [:connection "scm:git:https://github.com/generateme/cljplot2.git"]
    [:developerConnection "scm:git:ssh://git@github.com/generateme/cljplot2.git"]
    [:tag (str "v" version)]]])

(defn clean
  "Removes `target/`."
  [opts]
  (b/delete {:path "target"})
  opts)

(defn jar
  "Builds the jar (sources and resources, fonts included) and its pom into `target/`."
  [opts]
  (clean opts)
  (let [basis (b/create-basis {})]
    (b/write-pom {:class-dir class-dir :lib lib :version version :basis basis :src-dirs ["src"]
                  :resource-dirs ["resources"] :pom-data pom-data})
    (b/copy-dir {:src-dirs ["src" "resources"] :target-dir class-dir})
    (b/jar {:class-dir class-dir :jar-file jar-file}))
  (println "Built" jar-file)
  opts)

(defn install
  "Builds the jar and installs it into the local Maven repository."
  [opts]
  (jar opts)
  (b/install {:basis (b/create-basis {}) :lib lib :version version :jar-file jar-file :class-dir class-dir})
  (println "Installed" lib version)
  opts)

(defn- settings-credentials?
  "Tests whether `~/.m2/settings.xml` has a `clojars` server entry."
  []
  (let [f (io/file (System/getProperty "user.home") ".m2" "settings.xml")]
    (and (.exists f) (re-find #"<id>\s*clojars\s*</id>" (slurp f)))))

(defn- repository
  "The deps-deploy `:repository` option: `nil` (deps-deploy reads the environment variables) or `\"clojars\"` (the
  settings.xml server entry); throws when neither holds credentials."
  []
  (cond
    (and (System/getenv "CLOJARS_USERNAME") (System/getenv "CLOJARS_PASSWORD")) nil
    (settings-credentials?) "clojars"
    :else (throw (ex-info (str "No Clojars credentials: set CLOJARS_USERNAME and CLOJARS_PASSWORD, or add a clojars "
                               "server to ~/.m2/settings.xml")
                          {}))))

(defn deploy
  "Builds the jar and deploys it, signed with gpg, to Clojars."
  [opts]
  (let [repo (repository)]
    (when (str/blank? (:out (b/process {:command-args ["gpg" "--list-secret-keys"] :out :capture :err :ignore})))
      (throw (ex-info "No gpg secret key to sign with" {})))
    (jar opts)
    (dd/deploy (cond-> {:installer :remote
                        :artifact (b/resolve-path jar-file)
                        :pom-file (b/pom-path {:lib lib :class-dir class-dir})
                        :sign-releases? true}
                 repo (assoc :repository repo))))
  opts)
