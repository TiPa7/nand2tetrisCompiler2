(ns compiler2
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
  (replace {"<" "&lt;"
            ">" "&gt;"
            "&" "&amp;"} (pre-tokenize filepath)))


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
  "Returns [remaining-code xml-lines]"
  [[current & remaining :as full-code] xml-lines]
  (cond
    ; Edit: Einfach zurückkehren
    (nil? current) [full-code xml-lines]

    (= current "let") (recur remaining (into xml-lines ["<letStatement>\n" "<keyword>" "let" "</keyword>\n"]) )
    (re-matches identifier-regex current) (recur remaining (into xml-lines ["<identifier>" current "</identifier>\n"]) )

    ;We expect an expression after this
    ; Edit: Mit Rest selber aufrufen
    (= current "[") (apply compile-let-statement (compile-expression remaining (into xml-lines ["<symbol>" "[" "</symbol>\n"])))

    ;We expect a = after this, not an expression
    (= current "]") (recur remaining (into xml-lines ["<symbol>" "]" "</symbol>\n"]))
    ; Note: Das ) macht expression iwie schon
    (= current ")") (recur remaining xml-lines)

    ;We expect an expression after this
    ; Edit: Mit Rest selber aufrufen
    (= current "=") (apply compile-let-statement (compile-expression remaining (into xml-lines ["<symbol>" "=" "</symbol>\n" ])))

    ; Edit: Exit einfach mit Rückgabe!
    (= current ";") [remaining (conj xml-lines "<symbol>;</symbol>\n" "</letStatement>\n")]
    ))


(defn compile-if-statement
  [[current & remaining :as full-code] xml-lines]
  (cond
    (= current "if") (recur remaining (into xml-lines ["<ifStatement>\n" "<keyword>" "if" "</keyword>\n"]) )

    ;We expect an expression after this one
    ; Edit: Mit Rest selber aufrufen
    (= current "(") (apply compile-if-statement (compile-expression remaining (into xml-lines ["<symbol>" current "</symbol>\n"]) ))

    (= current ")") (recur remaining (into xml-lines ["<symbol>" current "</symbol>\n"]))
    ; Edit: Mit Rest selber aufrufen
    (= current "{") (apply compile-if-statement (compile-statements remaining (into xml-lines ["<symbol>" current "</symbol>\n" "<statements>\n"]) ))


    ;Exiting if statement without else to whoever contained the if statement
    ; EDIT: Einfach zurückkehren!!
    (and (not= (first remaining) "else")
         (= current "}"))
    [remaining (into xml-lines ["<symbol>" "}" "</symbol>\n" "</ifStatement>\n"])]

    ; We found an "else" so we process the }, the else and the { and then statements
    (and (= (first remaining) "else")
         (= current "}"))
    (apply compile-if-statement (compile-statements
                                  (rest (rest remaining))
                                  (into xml-lines ["<symbol>" "}" "</symbol>\n" "<keyword>" "else" "</keyword>\n" "<symbol>" "{" "</symbol>\n" "<statements>\n"])
                                  ))

    )
  )


(defn compile-while-statement
  [[current & remaining :as full-code] xml-lines]
  (cond
    (= current "while") (recur remaining (into xml-lines ["<whileStatement>\n" "<keyword>" "while" "</keyword>\n"]) )
    ; Edit: Mit Rest selber aufrufen
    (= current "(") (apply compile-while-statement (compile-expression remaining (into xml-lines ["<symbol>(</symbol>\n"]) ))
    (= current ")") (recur remaining (into xml-lines ["<symbol>" current "</symbol>\n"]) )
    ; Edit: Mit Rest selber aufrufen
    (= current "{") (apply compile-while-statement (compile-statements remaining (into xml-lines ["<symbol>" current "</symbol>\n" "<statements>\n"]) ))
    ; Edit: Eifnach zurückkehren
    (= current "}") [remaining (conj xml-lines "<symbol>}</symbol>\n" "</whileStatement>\n")]
    )
  )



(defn compile-do-statement
  [[current & remaining :as full-code] xml-lines]
  (cond
    (= current "do") (recur remaining (into xml-lines ["<doStatement>\n" "<keyword>" "do" "</keyword>\n"]))
    (= current ")") (recur remaining (into xml-lines ["<doStatement>\n" "<keyword>" "do" "</keyword>\n"]))
    ; Edit: Einfach zurückkehren
    (= current ";") [remaining (conj xml-lines "<symbol>;</symbol>\n" "</doStatement>\n")]
    ; Edit: Mit Rest selber aufrufen
    (re-matches identifier-regex current) (apply compile-do-statement (compile-subroutine-call full-code xml-lines))))


(defn compile-return-statement
  [[current & remaining :as full-code] xml-lines]
  (cond
    ; Edit: Mit rest selber aufrufen
    (= current "return") (recur remaining (into xml-lines ["<returnStatement>\n" "<keyword>" "return" "</keyword>\n"]))
    ; Edit: Einfach zurückkehren
    (= current ";") [remaining (conj xml-lines "<symbol>;</symbol>\n" "</returnStatement>\n")]
    ; Edit: Mit Rest selber aufrufen
    (re-matches identifier-regex current) (apply compile-return-statement (compile-expression full-code xml-lines )))
  )


(defn compile-statements
  "Returns [remaining-code xml-lines]"
  [[current & remaining :as full-code] xml-lines]

  (cond
    (= "let" current) (apply compile-statements (compile-let-statement full-code xml-lines ))
    (= "if" current) (apply compile-statements (compile-if-statement full-code xml-lines ))
    (= "while" current) (apply compile-statements (compile-while-statement full-code xml-lines ))
    (= "do" current) (apply compile-statements (compile-do-statement full-code xml-lines ))
    (= "return" current) (apply compile-statements (compile-return-statement full-code xml-lines ))

    ; Edit: Einfach zurückkehren mit [remaining-code xml-lines]
    :else
    [full-code (into xml-lines ["</statements>\n"])])
  )

;;Todo: caller oder callback um in statements zu entscheiden, wer aufgerufen hat
;; Wenn ein statement statements aufruft haben wir sonst das Problem, dass wir in subroutineBody gehen
;; bevor wir mit einem letStatement zB fertig sind, weil if/while selber statements hat



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; EXPRESSIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def op-regex #"^([+\-*/&|<>=]|&lt;|&gt;|&amp;)$")
(def unary-op-regex #"-|~")



;(defn compile-expression
;  "Returns [remaining-code xml-lines]"
;  [[current & remaining :as full-code] xml-lines]
;  ;(println "in compile-expression, callers sind:" (apply str callers))
;  (cond
;
;    ; Edit: Einfach zurückkehren
;    ; Fall 1: (expression)
;    ; expression muss danach nicht vorbei sein, kann auch op kommen
;    (= "(" current) (recur remaining (into xml-lines ["<symbol>" current "</symbol>\n" "<expression>\n"]))
;    (= ")" current) [full-code (into xml-lines ["</term>\n" "</expression>\n" ;"<symbol>" ")" "</symbol>\n"
;                                                ])]
;    ;TODO: ) Muss IMMER vom Aufrufer geschlossen werden wegen expressionList!!!!!
;
;
;    ; Edit: Einfach zurückkehren
;    ; Fall 2: varName [expression]
;    ; expression muss danach nicht vorbei sein, kann auch op kommen
;    ; Edit: Hier musste ein term vorne dran !!
;    (and (re-matches identifier-regex current)
;         (= "[" (first remaining)))
;    (recur (rest remaining) (into xml-lines ["<term>\n" "<identifier>" current "</identifier>\n"  "<symbol>" "[" "</symbol>\n" "<expression>\n"]) )
;
;    ; NOTE: Das schließt den Term nicht :shrug:
;    ; NOTE: Rückgabe statt recur :shrug:
;    (= "]" current) [remaining (into xml-lines ["</term>\n" "</expression>\n" "<symbol>" current "</symbol>\n"])]
;
;    ; Edit: Sich selbst mit Rest aufrufen
;    ; Fall 3 und 4: subroutineCall
;    (and (re-matches identifier-regex current)
;         (or
;           (= "(" (first remaining))
;           (= "." (first remaining))
;           )) (apply compile-expression (compile-subroutine-call full-code (into xml-lines ["<term>\n"])))
;
;    ; Edit: Selbst mit Rest aufrufen
;    ; Fall 5: integerConstant, stringConstant, keywordConstant, varName
;    (or (re-matches integer-constant-regex current)
;        (re-matches string-constant-regex  current)
;        (re-matches keyword-constant-regex current)
;        (re-matches identifier-regex       current))
;    (apply compile-expression (compile-simple-term full-code xml-lines ))
;
;    ;; Fall 6: Operator (für (op term)*)
;    (re-matches op-regex current)
;    (recur remaining (into xml-lines [(format "<symbol>%s</symbol>\n" current)]))
;    ; <term> dran oder nicht?!?!?
;    ; Dran,
;
;    ;; EDIT: Einfach zurückkehren
;    :else
;    [full-code (into xml-lines ["</term>\n" "</expression>\n"])]
;    ))

;; TODO: Fehler fixxen (siehe Beschreibung unten bei Testfall)
;(defn compile-term
;  "Returns [rest-tokens xml-lines] after consuming a term."
;  [[current & remaining :as full-code] xml-lines]
;  (cond
;    ;; Fall 1: ( expression )
;    (= "(" current)
;    (let [[rest-after-expr expr-xml] (compile-expression remaining [])]
;      (recur (rest rest-after-expr)
;             (into xml-lines
;                   (concat
;                     ["<term>\n" "<symbol>(</symbol>\n"     ;"<expression>\n"
;                      ]
;                     expr-xml
;                     [
;                      ;"</expression>\n"
;                     "<symbol>)</symbol>\n"]))))
;
;    ;; Fall 2: varName[ expression ]
;    (and (re-matches identifier-regex current)
;         (= "[" (first remaining)))
;    (let [[inner-rest expr-xml] (compile-expression (rest remaining) [])]
;      (apply compile-term [(rest inner-rest)
;                           (into xml-lines
;                                 (concat
;                                   ["<term>\n"
;                                    (format "<identifier>%s</identifier>\n" current)
;                                    "<symbol>[</symbol>\n"]
;                                   expr-xml
;                                   ["<symbol>]</symbol>\n"]))]))
;
;    ;; Fall 3 + 4: subroutineCall
;    (and (re-matches identifier-regex current)
;         (or (= "(" (first remaining))
;             (= "." (first remaining))))
;    (apply compile-term (compile-subroutine-call full-code
;                                                 (into xml-lines ["<term>\n"])))
;
;    ;; Fall 3.5 und 4.5: Ende eines subroutine Calls und damit auch eines Terms
;    ;(= ")" current) (recur remaining (into xml-lines ["<symbol>" ")" "</symbol>\n"]))
;
;    ;; Fall 5: simple term
;    (or (re-matches integer-constant-regex current)
;        (re-matches string-constant-regex  current)
;        (re-matches keyword-constant-regex current)
;        (re-matches identifier-regex       current))
;    (apply compile-term (compile-simple-term full-code xml-lines))
;
;    ;; Fall 6: unaryOp regex
;    ;; TODO: Fixxen
;    (re-matches unary-op-regex current)
;    (apply compile-term (compile-term remaining (into xml-lines ["<term>\n" "<symbol>" current "</symbol>\n"])))
;
;    :else
;    ;; kein Term gefunden
;    [full-code (into xml-lines ["</term>\n"])]))

(defn compile-term
  "Returns [rest-tokens xml-lines] after consuming a term."
  [[current & remaining :as full-code] xml-lines]
  (cond
    ;; Fall 1: ( expression )
    (= "(" current)
    (let [[rest-after-expr expr-xml] (compile-expression remaining [])]
      [(rest rest-after-expr)
       (into xml-lines
             (concat
               ["<term>\n" "<symbol>(</symbol>\n"]
               expr-xml
               ["<symbol>)</symbol>\n" "</term>\n"]))])

    ;; Fall 2: varName[ expression ]
    (and (re-matches identifier-regex current)
         (= "[" (first remaining)))
    (let [[inner-rest expr-xml] (compile-expression (rest remaining) [])]
      [(rest inner-rest)
       (into xml-lines
             (concat
               ["<term>\n"
                (format "<identifier>%s</identifier>\n" current)
                "<symbol>[</symbol>\n"]
               expr-xml
               ["<symbol>]</symbol>\n" "</term>\n"]))])

    ;; Fall 3: subroutineCall
    (and (re-matches identifier-regex current)
         (or (= "(" (first remaining))
             (= "." (first remaining))))
    (let [[rest-tokens call-xml] (compile-subroutine-call full-code [])]
      [rest-tokens
       (into xml-lines
             (concat ["<term>\n"] call-xml ["</term>\n"]))])

    ;; Fall 5: simple term
    (or (re-matches integer-constant-regex current)
        (re-matches string-constant-regex  current)
        (re-matches keyword-constant-regex current)
        (re-matches identifier-regex       current))
    (compile-simple-term full-code xml-lines)

    ;; Fall 6: unaryOp regex
    (re-matches unary-op-regex current)
    (let [[rest-tokens inner-xml] (compile-term remaining [])]
      [rest-tokens
       (into xml-lines
             (concat
               ["<term>\n" "<symbol>" current "</symbol>\n"]
               inner-xml
               ["</term>\n"]))])

    :else
    ;; kein Term gefunden
    [full-code (into xml-lines ["</term>\n"])]))

(defn compile-expression
  "Returns [rest-tokens xml-lines] after consuming an expression: term (op term)*."
  [[current & remaining :as full-code] xml-lines]
  (let [xml-opened (into xml-lines ["<expression>\n"])
        [rest-tks term-xml] (compile-term full-code [])
        xml-init (into xml-opened term-xml)]
    (loop [[c & rem :as code] rest-tks
           xml xml-init]
      ;; Falls: operator gefolgt von term
      (if (and c (re-matches op-regex c))
        (let [[next-rest term2-xml] (compile-term rem [])
              op-xml (format "<symbol>%s</symbol>\n" c)
              xml-with-op (conj xml op-xml)
              xml-new (into xml-with-op term2-xml)]
          (recur next-rest xml-new))
        ;; Ausdruck beendet
        [code (conj xml "</expression>\n")]))))


(defn compile-simple-term
  "Returns [remaining-code xml-lines]"
  [[current & remaining :as full-code] xml-lines]
  (let [tagged-term
        (cond
          (re-matches integer-constant-regex current)
          ["<term>\n" "<integerConstant>" current "</integerConstant>\n" "</term>\n"]

          (re-matches string-constant-regex current)
          ["<term>\n" "<stringConstant>" (prune-string current) "</stringConstant>\n" "</term>\n"]

          (re-matches keyword-constant-regex current)
          ["<term>\n" "<keyword>" current "</keyword>\n" "</term>\n"]

          (re-matches identifier-regex current)
          ["<term>\n" "<identifier>" current "</identifier>\n" "</term>\n"])]

    [remaining (into xml-lines tagged-term)]
    ))


(defn compile-subroutine-call
  "Returns [remaining-code xml-lines]"
  [[current & remaining :as full-code] xml-lines]

  (cond
    ; Edit: Mit Rest selbst aufrufen
    ; Öffnende Klammer löst compile-expression-list aus, diese muss bei ")" zurückkehren
    (= current "(")
    (apply compile-subroutine-call
           (compile-expression-list remaining (into xml-lines ["<symbol>" current "</symbol>\n"]) ))

    (= current ".") (recur remaining (into xml-lines ["<symbol>" current "</symbol>\n"]) )

    (re-matches identifier-regex current) (recur remaining (into xml-lines ["<identifier>" current "</identifier>\n"]) )

    (= ")" current) (recur remaining (into xml-lines ["<symbol>" ")" "</symbol>\n"]))

    ; Edit: Einfach zurückkehren
    :else
    [full-code xml-lines]
    ; (conj xml-lines "</term>\n") ?
    ; </term> nicht, wenn wir aus ) kommen?!?!
    ))


;; Note: Diese Lösung ist etwas verwirrend, wenn man HOF selten sieht:
;;  Wir übergeben an einen initial compile-expression-Aufruf eine neue callback-Funktion (continue)
;;  Diese checkt dann, ob wir ein "," als nächstes haben oder nicht.
;;  Falls ja, so ruft sie wieder ein compile-expression auf mit sich selbst als callback-Funktion
;;  Falls nein, so ruft die Callback-Funktion seinen caller auf
;(defn compile-expression-list
;  "Returns [remaining-code xml-lines]"
;  [[current & remaining :as tokens]
;   xml-lines]
;
;  ; TODO: Das letzte <symbol> ) </symbol> entfernen, erst das </expression> dran, dann das
;  ;  Außerdem: Falls expressionList leer ist, wird trotzdem <expression></expression> geschrieben :/
;
;  (letfn [(continue
;            ;; continue bekommt denselben Parameter-Satz wie compile-expression
;            [[next & rem :as toks] xml-lines]
;            (if (= next ",")
;              ;; Komma: ins XML schreiben und erneut compile-expression mit continue (also sich selbst)
;              (let [xml' (conj xml-lines "<symbol>,</symbol>\n<expression>\n")] ; NOTE: Müssen expression selber öffnen!!!!
;                (compile-expression rem xml'))
;              ;; kein Komma: schließe die Liste an expressions und zurück zum ursprünglichen caller
;              ;; EDIT: Einfach zurückkehren!!
;              (let [xml' (into xml-lines ["</expressionList>\n" "<symbol>" ")" "</symbol>\n"])]
;                [toks xml'])))]
;
;    ;; Einstieg: öffnendes Tag, erste Expression parsen
;    ;; EDIT: Apply auf continue aufrufen
;    (apply continue (compile-expression tokens
;                                        (into xml-lines ["<expressionList>\n" "<expression>\n"])
;                                        ))))


(defn compile-expression-list
  "Returns [remaining-code xml-lines]"
  [tokens xml-lines]
  (let [start-xml (conj xml-lines "<expressionList>\n")]
    (if (= (first tokens) ")")
      [(rest tokens) (into start-xml ["</expressionList>\n" "<symbol>)</symbol>\n"])]
      ;; Mindestens eine Expression vorhanden
      (loop [toks tokens
             xml  start-xml]
        (let [[after-expr xml-after-expr] (compile-expression toks xml)]
          (if (not= (first after-expr) ",")
            ;; Kein weiteres Komma -> Ende der Liste
            [after-expr (into xml-after-expr ["</expressionList>\n"])]
            ;; Komma gefunden -> nächste Expression verarbeiten
            (let [[_comma & after-comma] after-expr]
              (recur after-comma
                     (conj xml-after-expr "<symbol>,</symbol>\n")))))))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PROGRAM STRUCTURE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn compile-subroutine-body
  "Returns [remaining-code xml-lines]"
  ([full-code xml-lines]
   (let [[current & remaining :as code] full-code]
     (cond
       (= "{" current) (recur remaining (into xml-lines ["<subroutineBody>\n" "<symbol>" current "</symbol>\n"]))

       ; Edit: Sich selber mit Rest aufrufen
       (= "var" current) (apply compile-subroutine-body (compile-var-dec code xml-lines))

       ; Edit: Einfach zurückkehren!
       (= "}" current) [remaining (into xml-lines ["<symbol>" current "</symbol>\n" "</subroutineBody>\n" "</subroutineDec>\n"])]

       ; Edit: Sich selbst mit Rest aufrufen
       ; TODO: compile-statements ändern!
       :else
       (apply compile-subroutine-body (compile-statements code (into xml-lines ["<statements>\n"]))))))

  ;; 3-Parameter-Arity: Nötig für callback der statements (diese wissen nicht, we sie aufgerufen hat)
  ([full-code xml-lines callers]
   (compile-subroutine-body full-code xml-lines)))



(defn compile-subroutine-dec
  "Returns [remaining-code xml-lines]"
  [[current & remaining :as full-code] xml-lines]
  (cond
    (re-matches #"constructor|function|method" current) (recur remaining (into xml-lines ["<subroutineDec>\n" "<keyword>" current "</keyword>\n"]))
    (re-matches #"void|int|char|boolean" current) (recur remaining (into xml-lines ["<keyword>" current "</keyword>\n"]))
    (re-matches identifier-regex current) (recur remaining (into xml-lines ["<identifier>" current "</identifier>\n"])) ; Fall type UND Fall subroutineName

    ; Edit: compile-subroutine-dec echt höher in Hierarchie
    ; TODO: Die beiden Methoden anpassen!!!
    ;  Geht das recur?
    (= "(" current) (apply compile-subroutine-dec (compile-parameter-list full-code xml-lines))
    (= "{" current) (apply compile-subroutine-dec (compile-subroutine-body full-code xml-lines))
    :else [full-code xml-lines] ; ACHTUNG: compile-subroutine-body schließt </subroutineDec> da wir mehrere hintereinander haben KÖNNTEN
    ))



(defn compile-class-var-dec
  "Returns [remaining-code xml-lines]"
  [[current & remaining :as full-code] xml-lines]

  (cond
    (re-matches #"static|field" current) (recur remaining (into xml-lines ["<classVarDec>\n" "<keyword>" current "</keyword>\n"]))
    (re-matches #"int|char|boolean" current) (recur remaining (into xml-lines ["<keyword>" current "</keyword>\n"]))
    (re-matches identifier-regex current) (recur remaining (into xml-lines ["<identifier>" current "</identifier>\n"]))
    (= "," current) (recur remaining (into xml-lines ["<symbol>" current "</symbol>\n"]))
    ; Finishing class-var-dec and returning to caller (compile-class)
    (= ";" current) [remaining (into xml-lines ["<symbol>" current "</symbol>\n" "</classVarDec>\n"])]
    )

  )

(defn compile-class
  "Returns XML code - Top level compilation function"
  [[current & remaining :as full-code] xml-lines]
  ;[[cl cl-name brace & remaining :as full-code] xml-lines]

  ;(when (and
  ;        (= cl "class")
  ;        (re-matches identifier-regex cl-name)
  ;        (= brace "{"))
  ;  (concat
  ;    ["<class>\n"
  ;     "<keyword>" class "</keyword>\n"
  ;     "<identifier>" cl-name "</identifier>\n"
  ;     "<symbol>" "{" "</symbol>\n"]
  ;    (compile-class-var-dec remaining [])
  ;    (compile-subroutine-dec remaining [])
  ;    ["</class>"])
  ;  )


  (cond
    (empty? full-code) xml-lines  ; Terminierung bei leerer Token-Liste
    (= current "}") (into xml-lines ["<symbol>" "}" "</symbol>\n" "</class>"]) ;; Base case ending the compilation
    (= current "class") (recur remaining (into xml-lines ["<class>\n" "<keyword>" "class" "</keyword>\n"]))
    (= current "{") (recur remaining (into xml-lines ["<symbol>" "{" "</symbol>\n"]))

    ; Edit: Compile class echt höher in Hierarchie als compile-class-var-dec
    ; compile-class-var-dec muss zurückgeben: [remaining-code xml-lines]
    (re-matches #"static|field" current) (apply compile-class (compile-class-var-dec full-code xml-lines))

    ; Edit: Compile class echt höher in Hierarchie als compile-subroutine-dec
    ; compile-subroutine-dec muss zurückgeben: [remaining-code xml-lines]
    (re-matches #"constructor|function|method" current) (apply compile-class (compile-subroutine-dec full-code xml-lines))
    (re-matches identifier-regex current) (recur remaining (into xml-lines ["<identifier>" current "</identifier>\n"]))
    )
  )

(defn compile-parameter-list
  "Returns [remaining-code xml-lines]"
  [[current & remaining :as full-code] xml-lines]
  (cond
    (= "(" current) (recur remaining (into xml-lines ["<symbol>" current "</symbol>\n <parameterList>\n"]))

    ;Edit: Statt compile-subroutine-dec aufrufen einfach zurückkehren mit [remaining-code xml-lines]
    (= ")" current) [remaining (into xml-lines ["</parameterList>\n" "<symbol>" current "</symbol>\n"])]

    ;next two are type
    (re-matches #"int|char|boolean" current) (recur remaining (into xml-lines ["<keyword>" current "</keyword>\n"]))
    (re-matches identifier-regex current) (recur remaining (into xml-lines ["<identifier>" current "</identifier>\n"]))

    (= "," current) (recur remaining (into xml-lines ["<symbol>" current "</symbol>\n"]))
    )
  )

(defn compile-var-dec
  "Returns XML code"
  [[current & remaining :as full-code] xml-lines]
  (cond
    (= "var" current) (recur remaining (into xml-lines ["<varDec>\n" "<keyword>" current "</keyword>\n"]))
    (re-matches #"int|char|boolean" current) (recur remaining (into xml-lines ["<keyword>" current "</keyword>\n"]))
    ; type AND varName
    (re-matches identifier-regex current) (recur remaining (into xml-lines ["<identifier>" current "</identifier>\n"]))
    (= "," current) (recur remaining (into xml-lines ["<symbol>" current "</symbol>\n"]))
    ; Edit: Einfach zurückkehren!
    (= ";" current) [remaining (into xml-lines ["<symbol>" current "</symbol>\n" "</varDec>\n"])]
    ))


;;;;;;;;;;;;;;;;;;
;;; Aus Compiler 1
;;;;;;;;;;;;;;;;;;


(defn result-xml-to-file
  [file-path coll]
  (let [output-path (str/replace file-path #"\.jack$" "out.xml")]
    (spit output-path (apply str coll))))
; oder (str/join coll)

(defn compile-jack
  [filepath]
  (let [tokens (tokenize filepath)
        compiled (compile-class tokens [])]
    (result-xml-to-file filepath compiled)))

(defn -main [& args]
  (write-tokenizer-to-file (first args))
  (compile-jack (first args)))































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