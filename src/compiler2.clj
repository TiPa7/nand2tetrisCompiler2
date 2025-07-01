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

(defn pr2
  "Print + Return"
  [x]
  (prn x)
  x)


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
(declare param-to-xml)
(declare compile-statements)
(declare compile-statement)
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; EXPRESSIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def op-regex #"^([+\-*/&|<>=]|&lt;|&gt;|&amp;)$")
(def unary-op-regex #"-|~")


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
    )
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; STATEMENTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn compile-statement
  "Returns xml-lines"
  [[current & remaining :as full-code]]

  (cond
    ; Todo anpassen
    (= "let" current)  (compile-let-statement full-code)
    (= "if" current)  (compile-if-statement full-code)
    (= "while" current)  (compile-while-statement full-code)
    (= "do" current)  (compile-do-statement full-code)
    (= "return" current) (compile-return-statement full-code)
    ))

(defn group-statements
  [tokens]
  (loop [tokens tokens
         groups []
         current-group nil
         current-type nil
         brace-depth 0]
    (if (empty? tokens)
      (if current-group
        (conj groups current-group)
        groups)
      (let [token     (first tokens)
            remaining (rest tokens)]
        (cond
          ;; Neues Statement starten
          (and (nil? current-group)
               (#{"let" "if" "do" "while" "return"} token))
          (recur remaining
                 groups
                 [token]
                 token
                 (if (#{"if" "while"} token) 0 0))

          ;; Innerhalb eines Statements
          current-group
          (let [new-group        (conj current-group token)
                next-brace-depth (cond
                                   (and (#{"if" "while"} current-type)
                                        (= token "{")) (inc brace-depth)
                                   (and (#{"if" "while"} current-type)
                                        (= token "}")) (dec brace-depth)
                                   :else           brace-depth)
                finished?        (case current-type
                                   "let"    (= token ";")
                                   "do"     (= token ";")
                                   "return" (= token ";")
                                   "if"     (and (= token "}")
                                                 (= next-brace-depth 0)
                                                 (not (and (seq remaining)
                                                           (= (first remaining) "else"))))
                                   "while"  (and (= token "}")
                                                 (= next-brace-depth 0))
                                   false)
                new-groups       (if finished? (conj groups new-group) groups)
                next-state       (if finished?
                                   {:groups        new-groups
                                    :current-group nil
                                    :current-type  nil
                                    :brace-depth   0}
                                   {:groups        new-groups
                                    :current-group new-group
                                    :current-type  current-type
                                    :brace-depth   next-brace-depth})]
            (recur remaining
                   (:groups next-state)
                   (:current-group next-state)
                   (:current-type next-state)
                   (:brace-depth next-state)))

          ;; Kein aktives Statement (ignorieren)
          :else
          (recur remaining groups nil nil 0))))))



(defn extract-until-balanced
  [tokens]
  (loop [tokens tokens
         depth 1
         extracted []]
    (if (empty? tokens)
      (throw (Exception. "Unbalanced braces"))
      (let [token (first tokens)]
        (cond
          (and (= token "}") (= depth 1))
          [extracted (rest tokens)]

          (= token "{")
          (recur (rest tokens) (inc depth) (conj extracted token))

          (= token "}")
          (recur (rest tokens) (dec depth) (conj extracted token))

          :else
          (recur (rest tokens) depth (conj extracted token)))))))

; NOTE: Starte bei 0, weil wir im Code auch die öffnende { noch haben
(defn extract-until-balanced-full-method
  [tokens]
  (loop [tokens tokens
         depth 0
         extracted []]
    (if (empty? tokens)
      (throw (Exception. "Unbalanced braces"))
      (let [token (first tokens)]
        (cond
          (and (= token "}") (= depth 1))
          [extracted (rest tokens)]

          (= token "{")
          (recur (rest tokens) (inc depth) (conj extracted token))

          (= token "}")
          (recur (rest tokens) (dec depth) (conj extracted token))

          :else
          (recur (rest tokens) depth (conj extracted token)))))))

(declare compile-expression)


(defn compile-let-statement
  "Returns xml-lines"
  [[lett var-name bracket-or-equal :as full-code]]
  (let [start-xml [(str "<letStatement>\n")
                   (str "<keyword> let </keyword>\n")
                   (str "<identifier> " var-name " </identifier>\n")]
        end-xml [(str "</letStatement>\n")]]
    (if (= "=" bracket-or-equal)
      ; Fall 1: Einfache Zuweisung (ohne Array-Index)
      (let [eq-xml ["<symbol>=</symbol>\n"]
            [rest-after-expr expr-xml] (compile-expression (drop 3 full-code) [])
            semicolon (first rest-after-expr)
            semi-xml ["<symbol>;</symbol>\n"]
            xml-lines (vec (concat start-xml eq-xml expr-xml semi-xml end-xml))]
        xml-lines)

      ; Fall 2: Array-Index-Zuweisung
      (if (= "[" bracket-or-equal)
        (let [open-bracket-xml ["<symbol>[</symbol>\n"]
              [rest-after-index index-expr-xml] (compile-expression (drop 3 full-code) [])
              close-bracket (first rest-after-index)
              close-bracket-xml ["<symbol>]</symbol>\n"]
              rest-after-bracket (rest rest-after-index)
              eq-sign (first rest-after-bracket)
              eq-xml ["<symbol>=</symbol>\n"]
              [rest-after-expr expr-xml] (compile-expression (rest rest-after-bracket) [])
              semicolon (first rest-after-expr)
              semi-xml ["<symbol>;</symbol>\n"]
              xml-lines (vec (concat start-xml
                                     open-bracket-xml
                                     index-expr-xml
                                     close-bracket-xml
                                     eq-xml
                                     expr-xml
                                     semi-xml
                                     end-xml))]
          xml-lines)))))


(compile-let-statement ["let" "i" "[" "5" "]" "=" "3" ";"])


(defn compile-if-statement
  "Returns [remaining-tokens xml-lines]"
  [[if-token & remaining :as tokens]]
  (when (not= if-token "if")
    (throw (Exception. "Not an if statement")))

  (let [start-xml [(str "<ifStatement>\n")
                   (str "<keyword> if </keyword>\n")]

        ; Process opening parenthesis
        open-paren (first remaining)
        _ (when (not= open-paren "(")
            (throw (Exception. (str "Expected ( after if, found " open-paren))))
        open-paren-xml ["<symbol>(</symbol>\n"]

        ; Process expression inside ()
        [rest-after-expr expr-xml] (compile-expression (rest remaining) [])
        close-paren (first rest-after-expr)
        _ (when (not= close-paren ")")
            (throw (Exception. (str "Expected ) after expression, found " close-paren))))
        close-paren-xml ["<symbol>)</symbol>\n"]

        ; Process opening brace for if block
        rest-after-paren (rest rest-after-expr)
        open-brace (first rest-after-paren)
        _ (when (not= open-brace "{")
            (throw (Exception. (str "Expected { after ), found " open-brace))))
        open-brace-xml ["<symbol>{</symbol>\n"]

        ; Extract if-block tokens (without outer braces)
        [if-block-tokens rest-after-if] (extract-until-balanced (rest rest-after-paren))
        ; Group into individual statements
        if-statement-groups (group-statements if-block-tokens)
        ; Compile each statement group
        if-statements-xml (vec  (concat ["<statements>\n"]
                                        (reduce concat (mapcat compile-statement if-statement-groups))
                                        ["</statements>\n"]))
        close-brace-xml ["<symbol>}</symbol>\n"]

        ; Check for else clause
        [else-xml rest-final] (if (and (seq rest-after-if)
                                       (= (first rest-after-if) "else"))
                                (let [else-keyword ["<keyword> else </keyword>\n"]
                                      rest-after-else (rest rest-after-if)
                                      else-open-brace (first rest-after-else)
                                      _ (when (not= else-open-brace "{")
                                          (throw (Exception. (str "Expected { after else, found " else-open-brace))))
                                      else-open-brace-xml ["<symbol>{</symbol>\n"]

                                      ; Extract else-block tokens
                                      [else-block-tokens else-rest] (extract-until-balanced (rest rest-after-else))
                                      ; Group into individual statements
                                      else-statement-groups (group-statements else-block-tokens)
                                      ; Compile each statement group
                                      else-statements-xml (vec (concat ["<statements>\n"]
                                                                       (mapcat compile-statement else-statement-groups)
                                                                       ["</statements>\n"]))
                                      else-close-brace-xml ["<symbol>}</symbol>\n"]
                                      else-xml (vec (concat else-keyword
                                                            else-open-brace-xml
                                                            else-statements-xml
                                                            else-close-brace-xml))]
                                  [else-xml else-rest])
                                [[] rest-after-if])

        ; Build final XML
        xml-lines (vec (concat start-xml
                               open-paren-xml
                               expr-xml
                               close-paren-xml
                               open-brace-xml
                               if-statements-xml
                               close-brace-xml
                               else-xml
                               ["</ifStatement>\n"]))]
     xml-lines))


(compile-if-statement ["if" "(" "true" ")" "{" "let" "i" "=" "1" ";" "}"])


(defn compile-while-statement
  "Returns xml-lines"
  [[while-token & remaining :as tokens]]
  (when (not= while-token "while")
    (throw (Exception. "Not a while statement")))

  (let [start-xml [(str "<whileStatement>\n")
                   (str "<keyword> while </keyword>\n")]

        ; Process opening parenthesis
        open-paren (first remaining)
        open-paren-xml ["<symbol>(</symbol>\n"]
        ; Process expression inside ()
        [rest-after-expr expr-xml] (compile-expression (rest remaining) [])
        close-paren (first rest-after-expr)
        close-paren-xml ["<symbol>)</symbol>\n"]

        ; Process opening brace for while block
        rest-after-paren (rest rest-after-expr)
        open-brace (first rest-after-paren)
        open-brace-xml ["<symbol>{</symbol>\n"]
        ; Extract while-block tokens (without outer braces)
        [while-block-tokens rest-after-while] (extract-until-balanced (rest rest-after-paren))
        ; Group into individual statements
        while-statement-groups (group-statements while-block-tokens)
        ; Compile each statement group
        while-statements-xml (vec (concat ["<statements>\n"] ;NOTE Hier schlägt was fehl in der Rekursion
                                          (flatten (mapcat compile-statement while-statement-groups))
                                          ["</statements>\n"]))
        ; Process closing brace (already handled by extract-until-balanced, so we just output it)
        close-brace-xml ["<symbol>}</symbol>\n"]
        ; Build final XML
        xml-lines (vec (concat start-xml
                               open-paren-xml
                               expr-xml
                               close-paren-xml
                               open-brace-xml
                               while-statements-xml
                               close-brace-xml
                               ["</whileStatement>\n"]))]
    xml-lines))

(compile-while-statement ["while" "(" "i" "<" "5" ")" "{" "let" "i" "=" "i" "-" "1" ";" "}"])

(defn compile-do-statement
  "Returns [remaining-tokens xml-lines]"
  [[do & remaining :as tokens]]
  (let [start-xml [(str "<doStatement>\n")
                   (str "<keyword> do </keyword>\n")]
        [after-call call-xml] (compile-subroutine-call remaining [])
        semi-xml ["<symbol> ; </symbol>\n"]
        end-xml ["</doStatement>\n"]
        xml-lines (vec (concat start-xml
                               call-xml
                               semi-xml
                               end-xml))]
     xml-lines))

(compile-do-statement ["do" "Output" "." "print" "(" "5" "," "6" ")" ";"])


(defn compile-return-statement
  "Returns [remaining-tokens xml-lines]"
  [[return-token & remaining :as tokens]]
  (when (not= return-token "return")
    (throw (Exception. "Not a return statement")))

  (let [start-xml [(str "<returnStatement>\n")
                   (str "<keyword> return </keyword>\n")]
        ; Check if there's an expression before semicolon
        [rest-after-expr expr-xml] (if (not= (first remaining) ";")
                                     (compile-expression remaining [])
                                     [remaining []])
        semicolon (first rest-after-expr)
        semicolon-xml ["<symbol>;</symbol>\n"]
        xml-lines (vec (concat start-xml
                               expr-xml
                               semicolon-xml
                               ["</returnStatement>\n"]))]
     xml-lines))

(compile-return-statement ["return" "i" "+" "5" ";"])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; PROGRAM STRUCTURE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn split-coll-by-sep
  "Teilt die Collection `coll` in Sub-Collections auf,
   wobei bei jedem Auftreten von `sep` eine neue Sub-Collection begonnen wird.
   Das Trennzeichen selbst bleibt jeweils am Ende jeder Sub-Collection erhalten."
  [coll sep]
  (let [groups
        (reduce
          (fn [acc x]
            (let [acc' (update acc (dec (count acc)) conj x)]
              (if (= x sep)
                (conj acc' [])
                acc')))
          [[]]
          coll)]
    (if (empty? (peek groups))
      (pop groups)
      groups)))

(group-statements ["let" "i" "=" "1" ";" "if" "(" "true" ")" "{" "let" "i" "=" "2" ";" "}" "while" "(" "2" "<" "3" ")" "{" "let" "i" "=" "2" ";" "let" "i" "=" "3" ";" "}"])
; [["let" "i" "=" "1" ";"]
; ["if" "(" "true" ")" "{" "let" "i" "=" "2" ";" "}"]
; ["while" "(" "2" "<" "3" ")" "{" "let" "i" "=" "2" ";" "let" "i" "=" "3" ";" "}"]]

(defn var-dec-to-xml
  [[var type name & r]]
  (let [start-xml ["<varDec>\n" "<keyword>var</keyword>\n"]
        type-xml  (if (re-matches #"int|char|boolean" type)
                    ["<keyword>" type "</keyword>\n"]
                    ["<identifier>" type "</identifier>\n"])
        name-xml  ["<identifier>" name "</identifier>\n"]
        middle    (loop [[t1 t2 & re] r
                         xml []]
                    (if (= ";" t1)
                      (into xml ["<symbol>;</symbol>\n"])
                      (recur re
                             (into xml
                                   ["<symbol>,</symbol>\n"
                                    "<identifier>" t2 "</identifier>\n"]))))
        end-xml   ["</varDec>\n"]]
    (concat start-xml type-xml name-xml middle end-xml)))

(var-dec-to-xml ["var" "int" "i" "," "sum" "," "z" ";"])

(defn compile-subroutine-body
  "Returns xml-lines"
  [[open-curly & r]]
  (let [; Jeweils closing } des bodys wegnehmen
        var-decs-code (pr2 (take-while #(not (re-matches #"let|if|while|do|return" %)) (butlast r)))
        statements-code (pr2 (drop-while #(not (re-matches #"let|if|while|do|return" %)) (butlast r)))
        start-xml ["<subroutineBody>\n" "<symbol>{</symbol>\n"]
        end-xml ["<symbol>}</symbol>\n" "</subroutineBody>\n"]
        var-decs-split-code (pr2 (split-coll-by-sep var-decs-code ";"))
        var-decs-xml (pr2 (reduce concat (map var-dec-to-xml var-decs-split-code)))
        ;; TODO: group-statements korrekt??
        statements-split-code (pr2 (group-statements statements-code))
        ;; Todo compile statements anpassen
        statements-start ["<statements>\n"]
        statements-xml (reduce concat (map compile-statement statements-split-code))
        statements-end ["</statements>\n"]
        xml (concat start-xml var-decs-xml statements-start statements-xml statements-end end-xml)]
    xml))


(defn group-subroutine-decs
  "Teilt die Collection `coll` in Sub-Collections auf.
   Eine neue Sub-Collection wird jeweils immer dann begonnen,
   wenn ein Separator (\"function\", \"method\", \"constructor\") auftaucht.
   Das Separator-Element selbst steht dabei am Anfang der neuen Sub-Collection."
  [coll]
  (let [seps #{"function" "method" "constructor"}]
    (reduce
      (fn [acc x]
        (if (seps x)
          (conj acc [x])
          (update acc (dec (count acc)) conj x)))
      []
      coll)))

(group-subroutine-decs
  ["function" "int"    "Main"     "(" ")" "{" "}"
   "method"   "String" "minor"    "(" ")" "{"])
;; => [["function" "int" "Main" "(" ")" "{" "}"]
;;     ["method"   "String" "minor" "(" ")" "{"]]


(defn param-to-xml
  [[type name comma]]
  (if (re-matches #"int|char|boolean" type)
    ["<keyword>" type "</keyword>\n" "<identifier>" name "</identifier>\n" "<symbol>,</symbol>\n"]
    ["<identifier>" type "</identifier>\n" "<identifier>" name "</identifier>\n" "<symbol>,</symbol>\n"]))

(defn compile-parameter-list
  "Returns xml-lines"
  ; [[current & remaining :as full-code] xml-lines]
  [r]
  (let [params-code (butlast r)
        start-xml ["<symbol>" "(" "</symbol>\n" "<parameterList>\n"]
        sep-code (split-coll-by-sep params-code ",")
        param-xml (butlast (reduce concat (map param-to-xml sep-code))) ;butlast entfern das "<symbol>,<symbol> des letzen Mappings!
        end-xml ["</parameterList>\n" "<symbol>" ")" "</symbol>\n"]]
    (concat start-xml param-xml end-xml)))

(defn split-coll-at-index
  "Teilt `coll` so auf, dass alle Elemente bis inklusive `idx`
   in der ersten, und der Rest in der zweiten Coll landen."
  [coll idx]
  (split-at (inc idx) coll))

(defn compile-single-subroutine-dec
  [[what type subroutineName open-parantheses & r]]
  (let [closing-parantheses-index (.indexOf r ")")
        [parameter-list-code subroutine-body-code] (split-coll-at-index r closing-parantheses-index)
        start-xml ["<subroutineDec>\n" "<keyword>" what "</keyword>\n"]
        type-xml (if (re-matches #"void|int|char|boolean" type)
                   ["<keyword>" type "</keyword>\n"]
                   ["<identifier>" type "</identifier>\n"])
        name-xml ["<identifier>" subroutineName "</identifier>\n"]
        parameter-list-xml (pr2 (compile-parameter-list parameter-list-code))

        ; TODO: compile-subroutine-body anpassen, compile-parameter-list klappt
        subroutine-body-xml (pr2 (compile-subroutine-body subroutine-body-code))
        end-xml ["</subroutineDec>\n"]]
    (concat start-xml type-xml name-xml parameter-list-xml subroutine-body-xml end-xml)
    )
  )

(.indexOf ["(" "int" "x" ")" "{" ")"] ")")

(defn compile-subroutine-dec
  "Returns [remaining-code xml-lines]"
  [[current & remaining :as full-code] xml-lines]
  (let [; } der Klasse wird vom Aufrufer entfernt!
        subroutine-dec-code (into [] (drop-while #(not (re-matches #"function|method|constructor" %)) full-code))
        blocks (pr2 (into [] (group-subroutine-decs subroutine-dec-code)))
        dec-xml (reduce concat (map compile-single-subroutine-dec blocks))]
    dec-xml
    )

  ;(cond
  ;  (re-matches #"constructor|function|method" current) (recur remaining (into xml-lines ["<subroutineDec>\n" "<keyword>" current "</keyword>\n"]))
  ;  (re-matches #"void|int|char|boolean" current) (recur remaining (into xml-lines ["<keyword>" current "</keyword>\n"]))
  ;  (re-matches identifier-regex current) (recur remaining (into xml-lines ["<identifier>" current "</identifier>\n"])) ; Fall type UND Fall subroutineName
  ;
  ;  (= "(" current) (apply compile-subroutine-dec (compile-parameter-list full-code xml-lines))
  ;  (= "{" current) (apply compile-subroutine-dec (compile-subroutine-body full-code xml-lines))
  ;  :else [full-code xml-lines] ; ACHTUNG: compile-subroutine-body schließt </subroutineDec> da wir mehrere hintereinander haben KÖNNTEN
  ;  )
  )

(compile-subroutine-dec ["function" "int" "a" "(" ")" "{" "}" "method" "int" "b" "(" "int" "x" "," "int" "y" ")" "{" "}"] [])

;;;; classVarDec

(defn compile-single-class-var-dec
  "Returns XML for one class-var-dec"
  [[what type name & r]]
  (if (nil? what)
    []
    (let [start ["<classVarDec>\n"]
          what-xml ["<keyword>" what "</keyword>\n"]
          type-xml (if (re-matches #"int|char|boolean" type) ["<keyword>" type "</keyword>\n"] ["<identifier>" type "</identifier>\n"])
          var-name-xml ["<identifier>" name "</identifier>\n"]
          middle (loop [[t1 t2 & re] r
                        xml []]
                   (if (= ";" t1)
                     (into xml ["<symbol>;</symbol>\n"])
                     (recur re (into xml ["<symbol>" "," "</symbol>\n" "<identifier>" t2 "</identifier>\n"])))
                   )
          end ["</classVarDec>\n"]]
      (into start (into what-xml (into type-xml (into var-name-xml (into middle end))))))))

(compile-single-class-var-dec ["static" "int" "hello" "," "world" "," "is" "," "outdated" ";"])


(defn compile-class-var-decs
  "Returns xml-lines"
  [[current & remaining :as full-code] xml-lines]

  (let [class-var-dec-code (into [] (take-while #(not (re-matches #"function|method|constructor" %)) full-code))
        blocks (into [] (split-coll-by-sep class-var-dec-code ";"))
        dec-xml (into [] (reduce concat (map compile-single-class-var-dec blocks)))]
    dec-xml))

(defn compile-class
  "Returns XML code - Top level compilation function"
  ;[[current & remaining :as full-code] xml-lines]
  [[cl cl-name brace & remaining :as full-code] xml-lines]

  (when (and
          (= cl "class")
          (re-matches identifier-regex cl-name)
          (= brace "{"))
    (let [class-var-dec-xml (doall (compile-class-var-decs (butlast remaining) []))
          subroutine-dec-xml (doall (compile-subroutine-dec (butlast remaining) []))]
      ; Mit butlast jeweils das closing } entfernen

      (concat
        ["<class>\n"
         "<keyword>" "class" "</keyword>\n"
         "<identifier>" cl-name "</identifier>\n"
         "<symbol>" "{" "</symbol>\n"]
        class-var-dec-xml
        subroutine-dec-xml
        ["<symbol>}</symbol>\n" "</class>\n"]))))


(compile-parameter-list ["(" "int" "x" "," "int" "y" "," "SquareGame" "squareGame" ")"])



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
        compiled (doall (compile-class tokens []))]
    (result-xml-to-file filepath compiled)))

(defn -main [& args]
  (write-tokenizer-to-file (first args))
  (compile-jack (first args)))


;;;;;;;;;;;;;;;;;
;; ArrayTest
;;;;;;;;;;;;;;;;;

; Klappt nach dem Refactoring ENDLICH
; diff -w ArrayTest/Main.xml ArrayTest/Mainout.xml
(compile-jack "src/10/ArrayTest/Main.jack")

;;;;;;;;;;;;;;;;;;;;;;;
;; ExpressionLessSquare
;;;;;;;;;;;;;;;;;;;;;;;

; Klappt auch direkt
; diff -w ExpressionLessSquare/Main.xml ExpressionLessSquare/Mainout.xml
(compile-jack "src/10/ExpressionLessSquare/Main.jack")


; keyword int keyword vom ersten Parameter in einer ParameterList eines constructors fehlt
; diff -w ExpressionLessSquare/Square.xml ExpressionLessSquare/Squareout.xml
(compile-jack "src/10/ExpressionLessSquare/Square.jack")

; diff -w ExpressionLessSquare/SquareGame.xml ExpressionLessSquare/SquareGameout.xml
; TODO: Illegal Argument Exception
; method void run macht Probleme??
; statements-xml in compile-subroutine-body
; flatten statt reduce concat does the trick (in compile-while statement, die inner statements werden ja gemappt)
(compile-jack "src/10/ExpressionLessSquare/SquareGame.jack")


;;;;;;;;;;;;;;;;;
;; Square
;;;;;;;;;;;;;;;;;


; diff -w Square/Main.xml Square/Mainout.xml
(compile-jack "src/10/Square/Main.jack")

; diff -w Square/Square.xml Square/Squareout.xml
(compile-jack "src/10/Square/Square.jack")

; diff -w Square/SquareGame.xml Square/SquareGameout.xml
(compile-jack "src/10/Square/SquareGame.jack")



; Alles diffs:
; diff -w ArrayTest/Main.xml ArrayTest/Mainout.xml
; diff -w ExpressionLessSquare/Main.xml ExpressionLessSquare/Mainout.xml
; diff -w ExpressionLessSquare/Square.xml ExpressionLessSquare/Squareout.xml
; diff -w ExpressionLessSquare/SquareGame.xml ExpressionLessSquare/SquareGameout.xml
; diff -w Square/Main.xml Square/Mainout.xml
; diff -w Square/Square.xml Square/Squareout.xml
; diff -w Square/SquareGame.xml Square/SquareGameout.xml





;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; FÜR COMPILER 2 ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(let [test-tokens ["int" "i" "," "String" "s" ")" "{" "static" "int" "x" ";"]
;      argument-list (vec (take-while #(not= ")" %) test-tokens))
;      restly-code (second (split-at (count argument-list) test-tokens))]
;  [argument-list restly-code]
;  )

(defn gen-tab-for-func
  "Generates a symbol table for one function/method"
  [[_ type subroutine-name open-paren & remaining :as code]]
  (let [[param-list-tokens after-params] (split-at (count (take-while #(not= ")" %) remaining))
                                                   remaining)
        ;; Remove trailing ")" from after-params
        after-params' (rest after-params)

        ;; Process parameters: group into [type name] pairs
        param-pairs (->> param-list-tokens
                         (remove #(= "," %))
                         (partition 2))
        table-with-args (reduce (fn [tab [idx [typ name]]]
                                  (assoc tab name {:type typ :kind "argument" :# idx}))
                                {}
                                (map-indexed vector param-pairs))
        [body-tokens after-body] (extract-until-balanced-full-method after-params')
        var-decls (filter #(= "var" (first %))
                          (split-coll-by-sep (rest body-tokens) ";"))
        process-var-dec (fn [tab [var-keyword var-type & names-and-commas]]
                          (let [;; Remove the trailing semicolon from names-and-commas
                                names-and-commas' (butlast names-and-commas)
                                var-names (remove #(= "," %) names-and-commas')
                                count (count (filter #(= (:kind %) "var") (vals tab)))]
                            (reduce (fn [acc name]
                                      (assoc acc name {:type var-type
                                                       :kind "var"
                                                       :# count}))
                                    tab
                                    var-names)))
        final-table (reduce process-var-dec table-with-args var-decls)]
    [final-table after-body]))



; Das Beispiel klappt
(gen-tab-for-func ["function" "int" "coolFunc" "(" "int" "x" "," "int" "y" ")" "{" "var" "int" "a" "," "b" ";" "var" "int" "c" ";" "}"])


(defn generate-symbol-table
  "Returns symbol table"
  [full-code]
  (loop [tables {:class {}}
         [current & remaining :as full-code] full-code
         inline-kind ""
         inline-type ""
         segment-counters {"static" 0 "field" 0}]
    (cond
      (nil? current) tables

      (= "static" current) (recur (assoc-in tables [:class (second remaining)] {:kind "static" :type (first remaining) :# (get segment-counters "static")})
                                  (rest (rest remaining))
                                  "static"
                                  (first remaining)
                                  (update segment-counters "static" inc))
      (= "field" current) (recur (assoc-in tables [:class (second remaining)] {:kind "field" :type (first remaining) :# (get segment-counters "field")})
                                 (rest (rest remaining))
                                 "field"
                                 (first remaining)
                                 (update segment-counters "field" inc))

      (= "," current) (recur (assoc-in tables [:class (first remaining)] {:type inline-type :kind inline-kind :# (get segment-counters inline-kind)})
                             (rest remaining)
                             inline-kind
                             inline-type
                             (update segment-counters inline-kind inc))

      (re-matches #"function|method|constructor" current)
      (let [[func-tab rem-code] (gen-tab-for-func full-code)
            new-tables (assoc tables (second remaining) func-tab)]
        (recur new-tables rem-code "" "" segment-counters)
        )

      :else
      (recur tables remaining "" "" segment-counters)
      )
    )
  )

(def array-test-tokens (vec (tokenize "src/10/ArrayTest/Main.jack")))
(def expression-less-test-tokens (vec (tokenize "src/10/ExpressionLessSquare/Main.jack")))
(def expression-less-test-tokens2 (vec (tokenize "src/10/ExpressionLessSquare/Square.jack")))

; Sieht korrekt aus
; {:class {},
; "main" {"a" {:type "Array", :kind "var", :# 0},
;         "length" {:type "int", :kind "var", :# 1},
;         "i" {:type "int", :kind "var", :# 2},
;         "sum" {:type "int", :kind "var", :# 3}}}
(generate-symbol-table array-test-tokens)

; Sieht korrekt aus
; {:class {"test" {:kind "static", :type "boolean", :# 0}},
; "main" {"game" {:type "SquareGame", :kind "var", :# 0}},
; "more" {"b" {:type "boolean", :kind "var", :# 0}}}
;(generate-symbol-table expression-less-test-tokens)


;(generate-symbol-table expression-less-test-tokens2)



(defn compile-jack
  [filepath]
  (let [tokens (tokenize filepath)
        sym-table (generate-symbol-table tokens)
        compiled (compile-class tokens [])]
    (result-xml-to-file filepath compiled)))



