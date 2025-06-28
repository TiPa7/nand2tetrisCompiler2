(ns project)

(defproject compiler2 "0.1.0"
            :main compiler2  ; Namespace mit deiner -main-Funktion
            :aot [compiler2] ; Ahead-of-Time-Kompilierung für Java-Export
            :dependencies [[org.clojure/clojure "1.11.1"]])
