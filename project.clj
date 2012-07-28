;; Lein2 project file.
;; Must manually run `lein2 git-deps` in project folder
(defproject clojure-playground "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-git-deps "0.0.1-SNAPSHOT"]]
  :git-dependencies [["https://github.com/clojure/clojurescript.git"]]
  :source-paths ["src/"
                 ".lein-git-deps/clojurescript/src/clj/"
                 ".lein-git-deps/clojurescript/src/cljs/"]
  :dependencies [[org.clojure/clojure "1.4.0"]])
