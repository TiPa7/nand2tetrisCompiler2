(ns project)

(defproject compiler1 "0.1.0"
            :main compiler1  ; Namespace mit deiner -main-Funktion
            :aot [compiler1] ; Ahead-of-Time-Kompilierung für Java-Export
            :dependencies [[org.clojure/clojure "1.11.1"]])
