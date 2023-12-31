(ns build
  (:refer-clojure :exclude [test])
  (:import [java.time LocalDate])
  (:require
    [clojure.string :as str]
    [clojure.tools.build.api :as b]
    [clojure.string :as str]
    [deps-deploy.deps-deploy :as dd]))

(def lib (quote dv/tick-util))
(def version (str (str/replace (str (LocalDate/now)) "-" ".")))
(def class-dir "target/classes")

(defn test "Run all the tests." [opts]
  (let [basis (b/create-basis {:aliases [:test :clj-tests]})
        cmds (b/java-command
               {:basis     basis
                :main      'clojure.main
                :main-args ["-m" "kaocha.runner"]})
        {:keys [exit]} (b/process cmds)]
    (when-not (zero? exit) (throw (ex-info "Tests failed" {}))))
  opts)

(defn- pom-template [version]
  [[:description "Utils for juxt/tick"]
   [:url "https://github.com/dvingo/tick-util"]
   [:licenses
    [:license
     [:name "MIT"]
     [:url "https://opensource.org/licenses/MIT"]]]
   [:developers [:developer [:name "Daniel Vingo"]]]
   [:scm
    [:url "https://github.com/dvingo/tick-util"]
    [:connection "scm:git:https://github.com/dvingo/tick-util.git"]
    [:developerConnection "scm:git:ssh:git@github.com:dvingo/tick-util.git"]
    [:tag (str "v" version)]]])

(defn- jar-opts [opts]
  (assoc opts
    :lib lib :version version
    :jar-file (format "target/%s-%s.jar" lib version)
    :basis (b/create-basis {})
    :class-dir class-dir
    :target "target"
    :src-dirs ["src/main"]
    :pom-data (pom-template version)))

(defn ci "Run the CI pipeline of tests (and build the JAR)." [opts]
  (test opts)
  (b/delete {:path "target"})
  (let [opts (jar-opts opts)]
    (println "\nWriting pom.xml.")
    (b/write-pom opts)
    (println "\nCopying source.")
    (b/copy-dir {:src-dirs ["src/main"] :target-dir class-dir})
    (println "\nBuilding JAR." (:jar-file opts))
    (b/jar opts))
  opts)

(defn install "Install the JAR locally." [opts]
  (let [opts (jar-opts opts)]
    (b/install opts))
  opts)

(defn deploy "Deploy the JAR to Clojars." [opts]
  (let [{:keys [jar-file] :as opts} (jar-opts opts)]
    (dd/deploy {:installer :remote :artifact (b/resolve-path jar-file)
                :pom-file  (b/pom-path (select-keys opts [:lib :class-dir]))}))
  opts)
