(ns compiler1
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; STATEMENTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn compile-let-statement
  [[current & remaining :as full-code] xml-lines [caller & c :as callers]]
  (cond
    (nil? current) (caller full-code xml-lines c)           ; EDIT

    (= current "let") (recur remaining (into xml-lines ["<letStatement>\n" "<keyword>" "let" "</keyword>\n"]) callers)
    (re-matches identifier-regex current) (recur remaining (into xml-lines ["<identifier>" current "</identifier>\n"]) callers)

    ;We expect an expression after this
    (= current "[") (compile-expression remaining (into xml-lines ["<symbol>" "[" "</symbol>\n"]) (into [compile-let-statement] callers))

    ;We expect a = after this, not an expression
    (= current "]") (recur remaining (into xml-lines ["<symbol>" "]" "</symbol>\n"]) callers)

    ;We expect an expression after this
    (= current "=") (compile-expression remaining (into xml-lines ["<symbol>" "=" "</symbol>\n"]) (into [compile-let-statement] callers))

    ;Exiting let statement
    ; TODO: Wenn caller compile-subroutine-body ist, dann dürfen wir nur mit 2 Parametern aufrufen
    (= current ";") (caller remaining (into xml-lines ["<symbol>" current "</symbol>\n" "</letStatement>\n"]) c)
    ))


(defn compile-if-statement
  [[current & remaining :as full-code] xml-lines [caller & c :as callers]]
  (cond
    (= current "if") (recur remaining (into xml-lines ["<ifStatement>\n" "<keyword>" "if" "</keyword>\n"]) callers)

    ;We expect an expression after this one
    (= current "(") (compile-expression remaining (into xml-lines ["<symbol>" current "</symbol>\n"]) (into [compile-if-statement] callers))

    (= current ")") (recur remaining (into xml-lines ["<symbol>" current "</symbol>\n"]) callers)
    (= current "{") (compile-statements remaining (into xml-lines ["<symbol>" current "</symbol>\n"]) (into [compile-if-statement] callers))


    ;Exiting if statement without else to whoever contained the if statement
    (and (not= (first remaining) "else")
         (= current "}"))
    (cond
      (= caller compile-if-statement) (compile-if-statement remaining (into xml-lines ["<symbol>" current "</symbol>\n" "</ifStatement>\n"]) c)
      (= caller compile-while-statement) (compile-while-statement remaining (into xml-lines ["<symbol>" current "</symbol>\n" "</ifStatement>\n"]) c)
      (= caller compile-subroutine-body) (compile-subroutine-body remaining (into xml-lines ["<symbol>" current "</symbol>\n" "</ifStatement>\n"]))
      (= caller compile-statements) (compile-statements remaining (into xml-lines ["<symbol>" current "</symbol>\n" "</whileStatement>\n"]) c)
      )

    ; We found an "else" so we process the }, the else and the { and then statements
    (and (= (first remaining) "else")
         (= current "}"))
    (compile-statements
      (rest (rest remaining))
      (into xml-lines ["<symbol>" "}" "</symbol>\n" "<keyword>" "else" "</keyword>\n" "<symbol>" "{" "</symbol>"])
      (into [compile-if-statement] callers))

    )
  )


(defn compile-while-statement
  [[current & remaining :as full-code] xml-lines [caller & c :as callers]]
  (cond
    (= current "while") (recur remaining (into xml-lines ["<whileStatement>\n" "<keyword>" "while" "</keyword>\n"]) callers)
    (= current "(") (compile-expression full-code xml-lines (into [compile-while-statement] callers))
    (= current ")") (recur remaining (into xml-lines ["<symbol>" current "</symbol>\n"]) callers)
    (= current "{") (compile-statements remaining (into xml-lines ["<symbol>" current "</symbol>\n"]) (into [compile-while-statement] callers))
    (= current "}") (caller remaining (into xml-lines ["<symbol>" current "</symbol>\n" "</whileStatement>\n"]) c)
    )
  )



(defn compile-do-statement
  [[current & remaining :as full-code] xml-lines [caller & c :as callers]]
  (cond
    (= current "do") (recur remaining (into xml-lines ["<doStatement>\n" "<keyword>" "do" "</keyword>\n"]) callers)
    (= current ";") (caller remaining (into xml-lines ["<symbol>" current "</symbol>\n" "</doStatement>\n"]) c)
    (re-matches identifier-regex current) (compile-subroutine-call full-code xml-lines (into [compile-do-statement] callers))))


(defn compile-return-statement
  [[current & remaining :as full-code] xml-lines [caller & c :as callers]]
  (cond
    (= current "return") (compile-expression remaining (into xml-lines ["<returnStatement>\n" "<keyword>" "return" "</keyword>\n"]) (into [compile-return-statement] callers))
    (= current ";") (caller remaining (into xml-lines ["<symbol>" current "</symbol>\n" "</whileStatement>\n"]) c)
    (re-matches identifier-regex current) (compile-expression full-code xml-lines (into [compile-return-statement] callers)))
  )


(defn compile-statements
  [[current & remaining :as full-code] xml-lines [caller & c :as callers]]

  (cond
    ;TODO: Muss da in callers noch ein compile-statements rein??
    (= "let" current) (compile-let-statement full-code xml-lines (into [compile-statements] callers))
    (= "if" current) (compile-if-statement full-code xml-lines (into [compile-statements] callers))
    (= "while" current) (compile-while-statement full-code xml-lines (into [compile-statements] callers))
    (= "do" current) (compile-do-statement full-code xml-lines (into [compile-statements] callers))
    (= "return" current) (compile-return-statement full-code xml-lines (into [compile-statements] callers)))

  (caller full-code xml-lines c)
  )

;;Todo: caller oder callback um in statements zu entscheiden, wer aufgerufen hat
;; Wenn ein statement statements aufruft haben wir sonst das Problem, dass wir in subroutineBody gehen
;; bevor wir mit einem letStatement zB fertig sind, weil if/while selber statements hat



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; EXPRESSIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def op-regex #"^([+\-*/&|<>=]|&lt;|&gt;)$")
(def unary-op-regex #"-|~")



(defn compile-expression
  [[current & remaining :as full-code] xml-lines [caller & c :as callers]]
  (println "in compile-expression, callers sind:" (apply str callers))
  (cond

    ; Fall 1: (expression)
    ; expression muss danach nicht vorbei sein, kann auch op kommen
    (= "(" current) (compile-expression remaining (into xml-lines ["<symbol>" current "</symbol>\n" "<expression>\n"]) callers)
    (= ")" current) (caller remaining (into xml-lines ["</expression>" "<symbol>" current "</symbol>\n"]) c)

    ; Fall 2: varName [expression]
    ; expression muss danach nicht vorbei sein, kann auch op kommen
    (and (re-matches identifier-regex current)
         (= "[" (first remaining)))
    (compile-expression (rest remaining) (into xml-lines ["<expression>\n" "<identifier>" current "</identifier>\n"  "<symbol>" "[" "</symbol>\n"]) (into [compile-expression] callers))
    (= "]" current) (caller remaining (into xml-lines ["<symbol>" current "</symbol>\n" "</term>"]) c)

    ; Fall 3 und 4: subroutineCall
    (and (re-matches identifier-regex current)
         (or
           (= "(" (first remaining))
           (= "." (first remaining))
           )) (compile-subroutine-call full-code (into xml-lines ["<term>\n"]) (into [ compile-expression] callers))

    ; Fall 5: integerConstant, stringConstant, keywordConstant, varName
    (or (re-matches integer-constant-regex current)
        (re-matches string-constant-regex  current)
        (re-matches keyword-constant-regex current)
        (re-matches identifier-regex       current))
    (into xml-lines
          (compile-simple-term full-code xml-lines (into [compile-expression] callers)))

    ;; Fall 6: Operator (für (op term)*)
    ;; TODO: &lt; wird nicht erkannt?!?!?
    (re-matches op-regex current)
    (recur remaining (into xml-lines [(format "<symbol>%s</symbol>\n" current)]) callers)

    ;; Sonst: Ausdruck beenden und an caller full-code übergeben und expression-Tag beenden

    :else
    (caller full-code (into xml-lines ["</expression>"]) c)
    ))


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

    ; Eigentlich unnötig, da simpleTerm nur von compile-expression genutzt wird aber wir halten es hier konsistent
    (caller remaining (into xml-lines tagged-term) c)
    ))


(defn compile-subroutine-call
  [[current & remaining :as full-code] xml-lines [caller & c :as callers]]

  (cond
    ; Öffnende Klammer löst compile-expression-list aus, diese muss bei ")" zurückkehren zu caller
    (= current "(")
    (compile-expression-list remaining (into xml-lines ["<symbol>" current "</symbol>\n" "<expression>"]) (into [compile-subroutine-call] callers))

    (= current ".") (recur remaining (into xml-lines ["<symbol>" current "</symbol>\n"]) callers)

    (re-matches identifier-regex current) (recur remaining (into xml-lines ["<identifier>" current "</identifier>\n"]) callers)

    ;; in allen anderen Fällen rekursiver Aufruf auf GANZEN CODE, beende Term
    :else
    (caller full-code (into xml-lines ["</term>"]) c)))


;; Note: Diese Lösung ist etwas verwirrend, wenn man HOF selten sieht:
;;  Wir übergeben an einen initial compile-expression-Aufruf eine neue callback-Funktion (continue)
;;  Diese checkt dann, ob wir ein "," als nächstes haben oder nicht.
;;  Falls ja, so ruft sie wieder ein compile-expression auf mit sich selbst als callback-Funktion
;;  Falls nein, so ruft die Callback-Funktion seinen caller auf
(defn compile-expression-list

  [[current & remaining :as tokens]
   xml-lines
   [caller & c :as callers]]

  (letfn [(continue
            ;; continue bekommt denselben Parameter-Satz wie compile-expression
            [[next & rem :as toks] xml-lines [caller' & c' :as callers']]
            (if (= next ",")
              ;; Komma: ins XML schreiben und erneut compile-expression mit continue (also sich selbst)
              (let [xml' (conj xml-lines "<symbol>,</symbol>\n<expression>\n")] ; NOTE: Müssen expression selber öffnen!!!!
                (compile-expression rem xml' continue))
              ;; kein Komma: schließe die Liste an expressions und zurück zum ursprünglichen caller
              (let [xml' (conj xml-lines "</expressionList>\n")]
                (caller' toks xml' c'))))]

    ;; Einstieg: öffnendes Tag, erste Expression parsen
    (compile-expression tokens
                        (conj xml-lines "<expressionList>\n")
                        (into [continue] callers))))




;(defn compile-expression
;  [[current & remaining :as full-code] xml-lines [caller & c :as callers]]
;  (cond
;    ; Problem: Wo öffnen und schließen wir expression-Tag
;    (re-matches integer-constant-regex current) (recur remaining (into xml-lines ["<expression>\n" "<term>\n" "<integerConstant>" current "</integerConstant>\n" "</term>\n"]))
;    (re-matches string-constant-regex current) (recur remaining (into xml-lines ["<expression>\n" "<term>\n" "<stringConstant>" current "</stringConstant>\n" "</term>\n"]))
;    (re-matches #"true|false|null|this" current) (recur remaining ) (recur remaining (into xml-lines ["<expression>\n" "<term>\n" "<keywordConstant>" current "</keywordConstant>\n" "</term>\n"]))
;
;    (= "(" current) (recur remaining ) (compile-expression
;                                         remaining
;                                         (into xml-lines ["<expression>\n" "<term>\n" "<keywordConstant>" current "</keywordConstant>\n" "</term>\n"])
;                                         (into ["expression"] callers))
;    (=)
;
;
;
;
;    )
;  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PROGRAM STRUCTURE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn compile-subroutine-body
  ([full-code xml-lines]
   (let [[current & remaining :as code] full-code]
     (cond
       (= "{" current) (compile-subroutine-body remaining (into xml-lines ["<subroutineBody>\n" "<symbol>" current "</symbol>\n"]))

       (= "var" current) (compile-var-dec code xml-lines)

       (= "}" current) (compile-class remaining (into xml-lines ["<symbol>" current "</symbol>\n" "</subroutineBody>\n"]))

       :else
       (compile-statements code xml-lines [compile-subroutine-body]))))

  ;; 3-Parameter-Arity: Nötig für callback der statements (diese wissen nicht, we sie aufgerufen hat)
  ([full-code xml-lines callers]
   (compile-subroutine-body full-code xml-lines)))



(defn compile-subroutine-dec
  "Returns XML code"
  [[current & remaining :as full-code] xml-lines]
  (cond
    (re-matches #"constructor|function|method" current) (compile-subroutine-dec remaining (into xml-lines ["<subroutineDec>\n" "<keyword>" current "</keyword>\n"]))
    (re-matches #"void|int|char|boolean" current) (compile-subroutine-dec remaining (into xml-lines ["<keyword>" current "</keyword>\n"]))
    (re-matches identifier-regex current) (compile-subroutine-dec remaining (into xml-lines ["<identifier>" current "</identifier>\n"])) ; Fall type UND Fall subroutineName
    (= "(" current) (compile-parameter-list full-code xml-lines)
    (= "{" current) (compile-subroutine-body full-code xml-lines)

    ))



(defn compile-class-var-dec
  "Returns XML code"
  [[current & remaining :as full-code] xml-lines]
  (cond
    (re-matches #"static|field" current) (compile-class-var-dec remaining (into xml-lines ["<classVarDec>\n" "<keyword>" current "</keyword>\n"]))
    (re-matches #"int|char|boolean" current) (compile-class-var-dec remaining (into xml-lines ["<keyword>" current "</keyword>\n"]))
    (re-matches identifier-regex current) (compile-class-var-dec remaining (into xml-lines ["<identifier>" current "</identifier>\n"]))
    (= "," current) (compile-class-var-dec remaining (into xml-lines ["<symbol>" current "</symbol>\n"]))
    (= ";" current) (compile-class remaining (into xml-lines ["<symbol>" current "</symbol>\n"]))
    ))

(defn compile-class
  "Returns XML code "
  [[current & remaining :as full-code] xml-lines]
  (cond
    (empty? full-code) xml-lines  ; Terminierung bei leerer Token-Liste
    (= current "}") (into xml-lines ["<symbol>" "}" "</symbol>\n" "</class>"]) ;; Base case ending the compilation
    (= current "class") (compile-class remaining (into xml-lines ["<class>\n" "<keyword>" "class" "</keyword>\n"]))
    (= current "{") (compile-class remaining (into xml-lines ["<symbol>" "{" "</symbol>\n"]))
    (re-matches #"static|field" current) (compile-class-var-dec full-code xml-lines)
    (re-matches #"constructor|function|method" current) (compile-subroutine-dec full-code xml-lines)
    (re-matches identifier-regex current) (compile-class remaining (into xml-lines ["<identifier>" current "</identifier>\n"]))
    ))

(defn compile-parameter-list
  "Returns XML code"
  [[current & remaining :as full-code] xml-lines]
  (cond
    (= "(" current) (compile-parameter-list remaining (into xml-lines ["<symbol>" current "</symbol>\n <parameterList>\n"]))

    ;Exiting parameter list recursion, going back to subroutine-dec
    (= ")" current) (compile-subroutine-dec remaining (into xml-lines ["</parameterList>\n" "<symbol>" current "</symbol>\n" ]))
    ;next two are type
    (re-matches #"int|char|boolean" current) (compile-parameter-list remaining (into xml-lines ["<keyword>" current "</keyword>\n"]))
    (re-matches identifier-regex current) (compile-parameter-list remaining (into xml-lines ["<identifier>" current "</identifier>\n"]))

    (= "," current) (compile-parameter-list remaining (into xml-lines ["<symbol>" current "</symbol>\n"]))
    )
  )

(defn compile-var-dec
  "Returns XML code"
  [[current & remaining :as full-code] xml-lines]
  (cond
    (= "var" current) (compile-var-dec remaining (into xml-lines ["<varDec>\n" "<keyword>" current "</keyword>\n"]))
    (re-matches #"int|char|boolean" current) (compile-var-dec remaining (into xml-lines ["<keyword>" current "</keyword>\n"]))
    ; type AND varName
    (re-matches identifier-regex current) (compile-var-dec remaining (into xml-lines ["<identifier>" current "</identifier>\n"]))
    (= "," current) (compile-var-dec remaining (into xml-lines ["<symbol>" current "</symbol>\n"]))
    (= ";" current) (compile-subroutine-body remaining (into xml-lines ["<symbol>" current "</symbol>\n"]))
    ))






;; Testing

(def test-10-tokens (tokenize "src/10/ArrayTest/Main.jack"))

(compile-class test-10-tokens [])







































;
;(defmulti grammar (fn [s] s))
;(defmethod grammar "classVarDec" [s] (list "static|field" "type" "varNameOblFirst" ";")) ;;Achtung: (, varName)* fehlt noch!!!
;(defmethod grammar "varNameOblFirst" [s] (list "," "varName")) ;; Aufuf von check auf varNameOblfirst muss auf jeden Fall varName erzeugen und varName-Aufruf auch
;(defmethod grammar "varName" [s] (list "," "varName"))
;(defmethod grammar "type" [s] (list "int|char|boolean|className"))
;
;;; void|type direkt ausgetippt
;(defmethod grammar "subroutineDec" [s] (list "constructor|function|method" "void|int|char|boolean|identifier" "subroutineName" "(" "parameterList" ")" "subroutineBody"))
;
;(defmethod grammar "parameterList" [s] (list "type" "varNameOblFirst"))
;(defmethod grammar "subroutineBody" [s] (list "{" "varDec" "statements" "}"))
;(defmethod grammar "varDec" (list "var" "type" "varName"))
;(defmethod grammar "className" [s] (list "identifier"))
;(defmethod grammar "subroutineName" [s] (list "identifier"))
;(defmethod grammar "varName" [s] (list "identifier"))
;
;(defmethod grammar "statements" [s] (list "statement"))
;(defmethod grammar "letStatement" [s] (list "let" "varName" "([ expression ])?" "=" "expression" ";"))
;(defmethod grammar "ifStatement" [s] (list "if" "(" "expression" ")" "{" "statements" "}" "(else { statements })?"))
;(defmethod grammar "whileStatement" [s] (list "while" "(" "expression" ")" "{" "statements" "}"))
;(defmethod grammar "doStatement" [s] (list "do" "subroutineCall" ";"))
;(defmethod grammar "ReturnStatement" [s] (list "return" "expression" ";"))
;
;(defmethod grammar :default [s] s)
;
;
;
;
;(defmulti match
;  "Checks the found token against a grammar token"
;          (fn [found grammar] (get-type grammar)))
;
;(defmethod match "keyword" [found grammar] (= found grammar))
;(defmethod match "symbol" [found grammar] (= found grammar))
;(defmethod match "integerConstant" [found grammar] (is-int? found))
;(defmethod match "stringConstant" [found grammar] true)
;(defmethod match "identifier" [found grammar] (not (= (first found) "_")))
;
;(defmethod match :default [found grammar] false)
;
;
;;(defn compile-expression
;;  [[token & restly-found :as found-tokens]
;;   ;[grammar-token & restly-grammar-tokens :as grammar-tokens]
;;   ]
;;  (if (= token "(")                                         ;; Case (expression)
;;    (let [restly-found (compile-expression restly-found)]
;;      (if (= ")" (first restly-found))
;;        (rest restly-found)
;;        (throw (Exception. "Expression is faulty!, ( was never closed!"))))
;;    (let [lookahead1 (first (rest found-tokens))]
;;      (if (= "[" lookahead1)
;;        (let [restly-found (compile-expression restly-found)]
;;          (if (= "]" (first restly-found))
;;            (rest restly-found)
;;            (throw (Exception. "Expression is faulty!, [ was never closed!"))))
;;        (if (and (match token "subroutineName") (= "(" lookahead1))
;;          (let [restly-found (compile-expression-list (rest (rest found-tokens)))]
;;            (if (= ")" (first restly-found))
;;              (rest restly-found)
;;              (throw (Exception. "Expression is faulty!, ( was never closed in subroutineCall!"))))
;;          (if (match token )
;;            then
;;            else))))
;;      ))
;
;
;(defn check-grammar
;  "Compares expected token of grammar with token found in jack-File
;   where:
;   found-tokens are the actual tokens (found by Tokenizer in Jack-file),
;   grammar-token is the expected next token/symbol of grammar.
;
;   Returns whether found tokens can be generated by grammar.
;
;   The function may use the grammar to generate terminal symbols and
;   consume more tokens from "
;  [[token & restly-found :as found-tokens]
;   [grammar-token & restly-grammar-tokens :as grammar-tokens]]
;
;  (if (= grammar-token (grammar grammar-token))
;
;    ;Terminalsymbol erreicht in Generierung
;    (if (match token grammar-token)
;      (recur restly-found restly-grammar-tokens)
;      (if (clojure.string/includes? grammar-token "|")
;        (let [possibilities (map grammar (str/split grammar-token #"\|"))  ;habe sichergestellt, dass map grammar keine | mehr enthalten kann!
;              case (filter (partial match token) grammar)]
;          (if (= 1 (count case))
;            (rest found-tokens)
;            (throw (Exception. (str "Token not in cases or multiple matches (| matching)" (str/join possibilities ",")) )))
;          )
;        false))
;
;    ;Grammatik noch erweiterbar auf Symbol
;    (if (= token "statement")
;      (let [which-statement (str (first found-tokens) "Statement")] ;i.e. generating with lookahead e.g. "letStatement"
;        (recur found-tokens (into (list which-statement) (rest grammar-tokens)))) ; replace statement with letStatement, ifStatement etc.
;
;      (if (= token "expression")
;        then
;        else)
;      )))
;
;(list "a" "b")
;
;(defn parse
;  [file-path]
;  (let [tokens (tokenize file-path)]
;    (loop [unchecked-tokens tokens
;           generated (list "class" "className" "{" "classVarDec" "subroutineDec" "}")
;           state ""]
;      )
;    )
;  )
;
;
;
;(defn init-state
;  "Returns default state for the compiler for given filepath"
;  [filepath]
;  (let [fileSplit (tokenize filepath)]
;    (atom {:index 0
;           :file fileSplit
;           :n (count fileSplit)})))