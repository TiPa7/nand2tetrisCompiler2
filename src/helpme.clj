(ns helpme
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PROJECT 10: Compiler I
;
; The Jack Compiler consist of two main modules:
; I. The Syntax Analyzer ("Lexer")
; II. Code generator


; This weeks project only implements the lexer.
; The lexer itself uses two main modules:

; I. Tokenizer - Splits given code into tokens. Those are atomic pieces of code.
; II. Parser - Checks, whether a grammar can generate a given list of tokens
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; PART 1: TOKENIZING


(defn remove-block-comments
  "Entfernt alle /* ... */ Kommentare aus dem Text."
  [text]
  (clojure.string/replace text #"/\*[\s\S]*?\*/" ""))

(def keyword-regex #"class|constructor|function|method|field|static|var|int|char|boolean|void|true|false|null|this|let|do|if|else|while|return")

(def keyword-constant-regex #"true|false|null|this")

(def symbol-regex #"\{|\}|\(|\)|\[|\]|\.|,|;|\+|-|\*|/|&|\||<|>|=|~")

; recognizes integers between 0 and 32726
(def integer-constant-regex #"[0-9]|[0-9][0-9]|[0-9][0-9][0-9]|[0-9][0-9][0-9][0-9]|[0-3][0-2][0-7][0-2][0-6]")

(def string-constant-regex #"\"[^\"\n]*\"")

(def identifier-regex #"[a-zA-Z_]+[a-zA-Z0-9_]*")



; NOTE: The RegEx used in this method has 3 cases:
;  1. \"[^\"]*\"                      This mean any string constant of the form "..."
;  2. [{}()\[\].;,+\-*\/&|<>=\~]      This means EXACTLY ONE special character
;  3. [^\s{}()\[\].;,+\-*\/&|<>=\~]+  This means an arbitrary amount of NON special characters (as indicated by negation ^)
(defn pre-tokenize
  "Liest Datei, entfernt Kommentare und Whitespaces (außer in Strings), tokenisiert"
  [filepath]
  (let [file-content (slurp filepath)
        no-block-comments (remove-block-comments file-content)
        lines (clojure.string/split-lines no-block-comments)
        token-regex #"\"[^\"]*\"|[{}()\[\].;,+\-*\/&|<>=\~]|[^\s{}()\[\].;,+\-*\/&|<>=\~]+"]
    (->> lines
         (map #(clojure.string/replace % #"//.*" "")) ; Zeilenkommentare entfernen
         (map clojure.string/trim)
         (remove clojure.string/blank?)
         (mapcat #(re-seq token-regex %)))))


(pre-tokenize "src/10/ArrayTest/Main.jack")


;(clojure.string/split "class Main {" #"\s+")
;


;; NOTE: Used RegEx has TWO cases:
;;  1. [^{}()\[\].;,+\-*\/&|<>=\~]+
;;   Matches arbitrary amount of characters that are not special symbols (not is declared with ^)
;;  2. [{}()\[\].;,+\-*\/&|<>=\~]
;;   Matches EXACTLY ONE special character
;(defn finalize-tokens
;  [s]
;  (or (re-seq #"[^{}()\[\].;,+\-*\/&|<>=\~]+|[{}()\[\].;,+\-*\/&|<>=\~]" s)
;      '()))





(defn tokenize
  "Tokenizes given text from filepath"
  [filepath]
  (replace {"<" "&lt;" ">" "&gt;"} (pre-tokenize filepath)))


(tokenize "src/10/ArrayTest/Main.jack")


(defn is-int? [s]
  (try
    (Integer/parseInt s)
    true
    (catch NumberFormatException _ false)))


(defn get-type
  [s]
  (let [keywords #{"class" "constructor" "function" "method" "field" "static" "var" "int" "char" "boolean"
                   "void" "true" "false" "null" "this" "let" "do" "if" "else" "while" "return"}
        symbols #{"{" "}" "(" ")" "[" "]" "." "," ";" "+" "-" "*" "/" "&" "|" "&lt;" "&gt;" "<" ">" "=" "~"}]
    (if (contains? keywords s)
      "keyword"
      (if (contains? symbols s)
        "symbol"
        (if (clojure.string/includes? s "\"")
          "stringConstant"
          (if (is-int? s)
            "integerConstant"
            "identifier"))))))

(defn prune-string
  "Removes quotes in string"
  [s]
  (clojure.string/trim (clojure.string/replace s "\"" "")))

(defn tokenize-to-xml
  [filepath]
  (let [tokenList (tokenize filepath)]
    (concat
      (list "<tokens>")
      (map
        (fn [s]
          (let [type (get-type s)]
            (str "<" type ">" (prune-string s) "</" type ">")))
        tokenList)
      (list "</tokens>"))
    ))


(tokenize-to-xml "src/10/ArrayTest/Main.jack")

; Testmethode

(defn write-tokenizer-to-file
  [file-path]
  (let [output-path (str/replace file-path #"\.jack$" "Tout.xml")]
    (spit output-path (str/join "\n" (tokenize-to-xml file-path)))))

(write-tokenizer-to-file "src/10/ArrayTest/Main.jack")

; diff -w MainT.xml und MainTout.xml
; gibt nichts mehr zurück, d.h. gleich

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; PART 2: PARSING
; We only really need to compute on the result of the tokenize function and the getType function.
; The rest is boilerplate code or necessary for test purposes.


; Idee bei *-Vorkommen:
; Die Ableitung dieser Regeln kann quasi leer sein beim Vergleich mit der Ausgabe des Tokenizers.
; Eine *-Regel erzeugt also immer ein Kind von sich selbst mit.
; Beispiel: classVarDec |- static type varName; classVarDec
; Falls der Tokenizer nur eine Deklaration gefunden hat, so stößt das hintere classVarDec auf etwas anderes.
; Völlig egal was es ist: Wir akzeptieren.
; Aufpassen: es gibt einige Regeln, wo wir MINDESTENS EIN Vorkommnis haben.
; In diesem Fall müssen wir neue Regeln einführehn.
; Konvention: Das erste Vorkommnis heißt wie in der Grammatik, die folgenden haben den Suffix "Opt"
;

(use 'clojure.walk)

; Forward-Deklarationen
(declare compile-class)
(declare compile-class-var-dec)
(declare compile-subroutine-dec)
(declare compile-subroutine-body)

(declare compile-statements)
(declare compile-let-statement)
(declare compile-if-statement)
(declare compile-while-statement)
(declare compile-do-statement)
(declare compile-return-statement)
(declare compile-statements)
(declare compile-subroutine-body)
(declare compile-subroutine-dec)
(declare compile-class-var-dec)
(declare compile-class)
(declare compile-term)
(declare compile-expression)
(declare compile-expression-list)
(declare compile-term)
(declare compile-simple-term)
(declare compile-subroutine-call)
(declare compile-var-dec)
(declare compile-parameter-list)

; PART 2: PARSING
; We only really need to compute on the result of the tokenize function and the getType function.
; The rest is boilerplate code or necessary for test purposes.

; Forward-Deklarationen
(declare compile-class)
(declare compile-class-var-dec)
(declare compile-subroutine-dec)
(declare compile-subroutine-body)

(declare compile-statements)
(declare compile-let-statement)
(declare compile-if-statement)
(declare compile-while-statement)
(declare compile-do-statement)
(declare compile-return-statement)
(declare compile-statements)
(declare compile-subroutine-body)
(declare compile-subroutine-dec)
(declare compile-class-var-dec)
(declare compile-class)
(declare compile-term)
(declare compile-expression)
(declare compile-expression-list)
(declare compile-term)
(declare compile-simple-term)
(declare compile-subroutine-call)
(declare compile-var-dec)
(declare compile-parameter-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; STATEMENTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn compile-let-statement
  [[current & remaining :as full-code] xml-lines [caller & c :as callers]]
  (cond
    (nil? current) (caller full-code xml-lines c) ; Terminierung bei leerer Liste
    (= current "let") (recur remaining (into xml-lines ["<letStatement>\n" "<keyword>" "let" "</keyword>\n"]) callers)
    (re-matches identifier-regex current) (recur remaining (into xml-lines ["<identifier>" current "</identifier>\n"]) callers)
    (= current "[") (compile-expression remaining (into xml-lines ["<symbol>" "[" "</symbol>\n"]) (into [compile-let-statement] callers))
    (= current "]") (recur remaining (into xml-lines ["<symbol>" "]" "</symbol>\n"]) callers)
    (= current "=") (compile-expression remaining (into xml-lines ["<symbol>" "=" "</symbol>\n"]) (into [compile-let-statement] callers))
    (= current ";") (caller remaining (into xml-lines ["<symbol>" current "</symbol>\n" "</letStatement>\n"]) c)))


(defn compile-if-statement
  [[current & remaining :as full-code] xml-lines [caller & c :as callers]]
  (cond
    (nil? current) (caller full-code xml-lines c) ; Terminierung bei leerer Liste
    (= current "if") (recur remaining (into xml-lines ["<ifStatement>\n" "<keyword>" "if" "</keyword>\n"]) callers)
    (= current "(") (compile-expression remaining (into xml-lines ["<symbol>" current "</symbol>\n"]) (into [compile-if-statement] callers))
    (= current ")") (recur remaining (into xml-lines ["<symbol>" current "</symbol>\n"]) callers)
    (= current "{") (compile-statements remaining (into xml-lines ["<symbol>" current "</symbol>\n"]) (into [compile-if-statement] callers))
    (and (not= (first remaining) "else") (= current "}"))
    (caller remaining (into xml-lines ["<symbol>" current "</symbol>\n" "</ifStatement>\n"]) c)
    (and (= (first remaining) "else") (= current "}"))
    (compile-statements (rest (rest remaining))
                        (into xml-lines ["<symbol>" "}" "</symbol>\n" "<keyword>" "else" "</keyword>\n" "<symbol>" "{" "</symbol>"])
                        (into [compile-if-statement] callers))))

(defn compile-while-statement
  [[current & remaining :as full-code] xml-lines [caller & c :as callers]]
  (cond
    (nil? current) (caller full-code xml-lines c) ; Terminierung bei leerer Liste
    (= current "while") (recur remaining (into xml-lines ["<whileStatement>\n" "<keyword>" "while" "</keyword>\n"]) callers)
    (= current "(") (compile-expression full-code xml-lines (into [compile-while-statement] callers))
    (= current ")") (recur remaining (into xml-lines ["<symbol>" current "</symbol>\n"]) callers)
    (= current "{") (compile-statements remaining (into xml-lines ["<symbol>" current "</symbol>\n"]) (into [compile-while-statement] callers))
    (= current "}") (caller remaining (into xml-lines ["<symbol>" current "</symbol>\n" "</whileStatement>\n"]) c)))

(defn compile-do-statement
  [[current & remaining :as full-code] xml-lines [caller & c :as callers]]
  (cond
    (nil? current) (caller full-code xml-lines c) ; Terminierung bei leerer Liste
    (= current "do") (recur remaining (into xml-lines ["<doStatement>\n" "<keyword>" "do" "</keyword>\n"]) callers)
    (= current ";") (caller remaining (into xml-lines ["<symbol>" current "</symbol>\n" "</doStatement>\n"]) c)
    (re-matches identifier-regex current) (compile-subroutine-call full-code xml-lines (into [compile-do-statement] callers))))

(defn compile-return-statement
  [[current & remaining :as full-code] xml-lines [caller & c :as callers]]
  (cond
    (nil? current) (caller full-code xml-lines c) ; Terminierung bei leerer Liste
    (= current "return") (compile-expression remaining (into xml-lines ["<returnStatement>\n" "<keyword>" "return" "</keyword>\n"]) (into [compile-return-statement] callers))
    (= current ";") (caller remaining (into xml-lines ["<symbol>" current "</symbol>\n" "</returnStatement>\n"]) c)
    (re-matches identifier-regex current) (compile-expression full-code xml-lines (into [compile-return-statement] callers))))

(defn compile-statements
  [[current & remaining :as full-code] xml-lines [caller & c :as callers]]
  (cond
    (nil? current) (caller full-code xml-lines c) ; Terminierung bei leerer Liste
    (= "let" current) (compile-let-statement full-code xml-lines (into [compile-statements] callers))
    (= "if" current) (compile-if-statement full-code xml-lines (into [compile-statements] callers))
    (= "while" current) (compile-while-statement full-code xml-lines (into [compile-statements] callers))
    (= "do" current) (compile-do-statement full-code xml-lines (into [compile-statements] callers))
    (= "return" current) (compile-return-statement full-code xml-lines (into [compile-statements] callers))
    :else (caller full-code xml-lines c))) ; Kein Statement, zurück zum Caller

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; EXPRESSIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def op-regex #"^([+\-*/&|<>=]|&lt;|&gt;)$")
(def unary-op-regex #"-|~")

(defn compile-expression
  [[current & remaining :as full-code] xml-lines [caller & c :as callers]]
  (cond
    (nil? current) (caller full-code xml-lines c) ; Terminierung bei leerer Liste
    (= "(" current) (compile-expression remaining (into xml-lines ["<symbol>" current "</symbol>\n" "<expression>\n"]) callers)
    (= ")" current) (caller remaining (into xml-lines ["</expression>\n" "<symbol>" current "</symbol>\n"]) c)
    (and (re-matches identifier-regex current) (= "[" (first remaining)))
    (compile-expression (rest remaining) (into xml-lines ["<expression>\n" "<identifier>" current "</identifier>\n" "<symbol>" "[" "</symbol>\n"]) (into [compile-expression] callers))
    (= "]" current) (caller remaining (into xml-lines ["<symbol>" current "</symbol>\n" "</term>\n"]) c)
    (and (re-matches identifier-regex current) (or (= "(" (first remaining)) (= "." (first remaining))))
    (compile-subroutine-call full-code (into xml-lines ["<term>\n"]) (into [compile-expression] callers))
    (or (re-matches integer-constant-regex current)
        (re-matches string-constant-regex current)
        (re-matches keyword-constant-regex current)
        (re-matches identifier-regex current))
    (compile-simple-term full-code xml-lines (into [compile-expression] callers))
    (re-matches op-regex current) (recur remaining (into xml-lines [(format "<symbol>%s</symbol>\n" current)]) callers)
    :else (caller full-code (into xml-lines ["</expression>\n"]) c)))

(defn compile-simple-term
  [[current & remaining :as full-code] xml-lines [caller & c :as callers]]
  (let [tagged-term
        (cond
          (re-matches integer-constant-regex current)
          ["<term>\n" "<integerConstant>" current "</integerConstant>\n" "</term>\n"]
          (re-matches string-constant-regex current)
          ["<term>\n" "<stringConstant>" current "</stringConstant>\n" "</term>\n"]
          (re-matches keyword-constant-regex current)
          ["<term>\n" "<keywordConstant>" current "</keywordConstant>\n" "</term>\n"]
          (re-matches identifier-regex current)
          ["<term>\n" "<identifier>" current "</identifier>\n" "</term>\n"])]
    (caller remaining (into xml-lines tagged-term) c)))

(defn compile-subroutine-call
  [[current & remaining :as full-code] xml-lines [caller & c :as callers]]
  (cond
    (nil? current) (caller full-code xml-lines c) ; Terminierung bei leerer Liste
    (= current "(") (compile-expression-list remaining (into xml-lines ["<symbol>" current "</symbol>\n" "<expressionList>\n"]) (into [compile-subroutine-call] callers))
    (= current ".") (recur remaining (into xml-lines ["<symbol>" current "</symbol>\n"]) callers)
    (re-matches identifier-regex current) (recur remaining (into xml-lines ["<identifier>" current "</identifier>\n"]) callers)
    :else (caller full-code (into xml-lines ["</term>\n"]) c)))

(defn compile-expression-list
  [[current & remaining :as tokens] xml-lines [caller & c :as callers]]
  (letfn [(continue [tokens xml-lines callers]
            (if (and (seq tokens) (= (first tokens) ","))
              (let [[_ & rem] tokens
                    xml' (conj xml-lines "<symbol>,</symbol>\n<expression>\n")]
                (compile-expression rem xml' (into [continue] callers)))
              (let [xml' (conj xml-lines "</expressionList>\n")]
                (caller tokens xml' c))))]
    (if (empty? tokens)
      (caller tokens (conj xml-lines "</expressionList>\n") c)
      (compile-expression tokens
                          (conj xml-lines "<expressionList>\n")
                          (into [continue] callers)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PROGRAM STRUCTURE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn compile-subroutine-body
  ([full-code xml-lines]
   (let [[current & remaining :as code] full-code]
     (cond
       (nil? current) xml-lines ; Terminierung bei leerer Liste
       (= "{" current) (compile-subroutine-body remaining (into xml-lines ["<subroutineBody>\n" "<symbol>" current "</symbol>\n"]))
       (= "var" current) (compile-var-dec code xml-lines)
       (= "}" current) (into xml-lines ["<symbol>" current "</symbol>\n" "</subroutineBody>\n"])
       :else
       (compile-statements code xml-lines [compile-subroutine-body]))))

  ([full-code xml-lines [caller & c :as callers]]
   (let [[current & remaining] full-code]
     (if (nil? current)
       (caller full-code xml-lines (rest callers))
       (compile-subroutine-body full-code xml-lines)))))

(defn compile-subroutine-dec
  [[current & remaining :as full-code] xml-lines]
  (cond
    (nil? current) xml-lines ; Terminierung bei leerer Liste
    (re-matches #"constructor|function|method" current) (compile-subroutine-dec remaining (into xml-lines ["<subroutineDec>\n" "<keyword>" current "</keyword>\n"]))
    (re-matches #"void|int|char|boolean" current) (compile-subroutine-dec remaining (into xml-lines ["<keyword>" current "</keyword>\n"]))
    (re-matches identifier-regex current) (compile-subroutine-dec remaining (into xml-lines ["<identifier>" current "</identifier>\n"]))
    (= "(" current) (compile-parameter-list full-code xml-lines)
    (= "{" current) (compile-subroutine-body full-code (into xml-lines [])))) ; Hier xml-lines übergeben

(defn compile-class-var-dec
  [[current & remaining :as full-code] xml-lines]
  (cond
    (nil? current) xml-lines ; Terminierung bei leerer Liste
    (re-matches #"static|field" current) (compile-class-var-dec remaining (into xml-lines ["<classVarDec>\n" "<keyword>" current "</keyword>\n"]))
    (re-matches #"int|char|boolean" current) (compile-class-var-dec remaining (into xml-lines ["<keyword>" current "</keyword>\n"]))
    (re-matches identifier-regex current) (compile-class-var-dec remaining (into xml-lines ["<identifier>" current "</identifier>\n"]))
    (= "," current) (compile-class-var-dec remaining (into xml-lines ["<symbol>" current "</symbol>\n"]))
    (= ";" current) (compile-class remaining (into xml-lines ["<symbol>" current "</symbol>\n" "</classVarDec>\n"]))))

(defn compile-class
  [[current & remaining :as full-code] xml-lines]
  (cond
    (empty? full-code) (into xml-lines ["</class>"]) ; Terminierung bei leerer Liste
    (= current "}") (into xml-lines ["<symbol>" "}" "</symbol>\n" "</class>"])
    (= current "class") (compile-class remaining (into xml-lines ["<class>\n" "<keyword>" "class" "</keyword>\n"]))
    (= current "{") (compile-class remaining (into xml-lines ["<symbol>" "{" "</symbol>\n"]))
    (re-matches #"static|field" current) (compile-class-var-dec full-code xml-lines)
    (re-matches #"constructor|function|method" current) (compile-subroutine-dec full-code xml-lines)
    (re-matches identifier-regex current) (compile-class remaining (into xml-lines ["<identifier>" current "</identifier>\n"]))))

(defn compile-parameter-list
  [[current & remaining :as full-code] xml-lines]
  (cond
    (nil? current) xml-lines ; Terminierung bei leerer Liste
    (= "(" current) (compile-parameter-list remaining (into xml-lines ["<symbol>" current "</symbol>\n" "<parameterList>\n"]))
    (= ")" current) (into xml-lines ["</parameterList>\n" "<symbol>" current "</symbol>\n"])
    (re-matches #"int|char|boolean" current) (compile-parameter-list remaining (into xml-lines ["<keyword>" current "</keyword>\n"]))
    (re-matches identifier-regex current) (compile-parameter-list remaining (into xml-lines ["<identifier>" current "</identifier>\n"]))
    (= "," current) (compile-parameter-list remaining (into xml-lines ["<symbol>" current "</symbol>\n"]))))

(defn compile-var-dec
  [[current & remaining :as full-code] xml-lines]
  (cond
    (nil? current) xml-lines ; Terminierung bei leerer Liste
    (= "var" current) (compile-var-dec remaining (into xml-lines ["<varDec>\n" "<keyword>" current "</keyword>\n"]))
    (re-matches #"int|char|boolean" current) (compile-var-dec remaining (into xml-lines ["<keyword>" current "</keyword>\n"]))
    (re-matches identifier-regex current) (compile-var-dec remaining (into xml-lines ["<identifier>" current "</identifier>\n"]))
    (= "," current) (compile-var-dec remaining (into xml-lines ["<symbol>" current "</symbol>\n"]))
    (= ";" current) (into xml-lines ["<symbol>" current "</symbol>\n" "</varDec>\n"])))



;; Testing

(def test-10-tokens (tokenize "src/10/ArrayTest/Main.jack"))

test-10-tokens

(def test-10-compiled (compile-class test-10-tokens []))

test-10-compiled

(def test-10-result (str/join test-10-compiled))

test-10-result



