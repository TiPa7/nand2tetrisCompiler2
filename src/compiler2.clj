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
  (clojure.string/replace s "\"" ""))

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
(declare compile-statement)
(declare compile-let-statement)
(declare compile-if-statement)
(declare compile-while-statement)
(declare compile-do-statement)
(declare compile-return-statement)
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

(defn kind->segment [kind]
  (case kind
    "static" "static"
    "field"  "this"
    "var"    "local"
    "argument" "argument"
    (throw (ex-info (str "Unknown segment kind: " kind)
                    {:kind kind}))))


; NOTE: Mostly checked
(defn compile-term
  "Returns [rest-tokens vm-lines] after consuming a term."
  [[current & remaining :as full-code] fn-name class-name symbol-table]
  (cond
    ;; Fall 1: ( expression )
    (= "(" current)
    (let [[rest-after-expr expr-vm]
          (compile-expression remaining fn-name class-name symbol-table)]
      [(rest rest-after-expr) expr-vm])

    ;; Fall 2: varName[ expression ] - KORRIGIERT
    (and (re-matches identifier-regex current)
         (= "[" (first remaining)))
    (let [var-name current
          full-var-name (str class-name "." var-name)
          ;; Suche zuerst im Subroutine-Scope, dann im Klassen-Scope
          var-info (or (get-in symbol-table [fn-name full-var-name])
                       (get-in symbol-table [:class full-var-name]))
          _ (when (nil? var-info)
              (throw (ex-info (str "Variable nicht gefunden: " full-var-name)
                              {:variable current :fn-name fn-name :class-name class-name})))
          segment (kind->segment (:kind var-info))

          ;; Überspringe "[" und kompiliere den Index-Ausdruck
          [rest-after-index index-vm] (compile-expression (rest remaining) fn-name class-name symbol-table)]

      [(rest rest-after-index)
       (concat index-vm
               [(str "push " segment " " (:# var-info) "\n")
                "add\n"
                "pop pointer 1\n"
                "push that 0\n"])])

    ;; Fall 3: subroutineCall
    (and (re-matches identifier-regex current)
         (or (= "(" (first remaining))
             (= "." (first remaining))))
    (compile-subroutine-call full-code fn-name class-name symbol-table)

    ;; Fall 4: simple term
    (or (re-matches integer-constant-regex current)
        (re-matches string-constant-regex current)
        (re-matches keyword-constant-regex current)
        (re-matches identifier-regex current))
    (compile-simple-term full-code fn-name class-name symbol-table)

    ;; Fall 5: unaryOp term
    (re-matches unary-op-regex current)
    (let [[rest-tokens inner-vm] (compile-term remaining fn-name class-name symbol-table)
          op-cmd ({"-" "neg\n" "~" "not\n"} current)]
      [rest-tokens
       (concat inner-vm [op-cmd])])

    :else
    ;; kein Term gefunden
    [full-code []]))

(def vm-operators
  {"+" "add\n"
   "-" "sub\n"
   "*" "call Math.multiply 2\n"
   "/" "call Math.divide 2\n"
   "&" "and\n"
   "&amp;" "and\n"
   "|" "or\n"
   "<" "lt\n"
   "&lt;" "lt\n"
   ">" "gt\n"
   "&gt;" "gt\n"
   "=" "eq\n"})

; TODO: Prüfen
(defn compile-expression
  "Returns [rest-tokens vm-lines] after consuming an expression: term (op term)*."
  [tokens fn-name class-name symbol-table]
  (let [[rest-tks term-vm] (compile-term tokens fn-name class-name symbol-table)]
    (loop [tokens rest-tks
           vm-code term-vm]
      (if-let [op-token (first tokens)]
        (if (re-matches op-regex op-token)
          (let [next-tokens (rest tokens)
                [rest-after-term term-vm-code] (compile-term next-tokens fn-name class-name symbol-table)
                operator-cmd (get vm-operators op-token)]
            (recur rest-after-term (concat vm-code term-vm-code [operator-cmd])))
          [tokens vm-code]) ; Kein Operator -> Ende
        [tokens vm-code])))) ; Keine Tokens mehr -> Ende

; NOTE: Mostly checked
(defn compile-simple-term
  "Returns [remaining-code vm-lines]"
  [[current & remaining :as full-code] fn-name class-name symbol-table]
  (cond
    ;Integer einfach pushen
    (re-matches integer-constant-regex current)
    [remaining [(str "push constant " current "\n")]]

    ;String schrittweise aus Chars aufbauen
    (re-matches string-constant-regex current)
    (let [pruned-str (prune-string current)
          str-length (count pruned-str)]
      [remaining
       (concat
         [(str "push constant " str-length "\n")
          "call String.new 1\n"]
         (mapcat (fn [c]
                   [(str "push constant " (int c) "\n")
                    "call String.appendChar 2\n"])
                 pruned-str))])

    (re-matches keyword-constant-regex current)
    (case current
      "true"  [remaining ["push constant 0\n" "not\n"]]
      "false" [remaining ["push constant 0\n"]]
      "null"  [remaining ["push constant 0\n"]]
      "this"  [remaining ["push pointer 0\n"]])

    (re-matches identifier-regex current)
    (let [full-var-name (str class-name "." current)
          var-info (if (get-in symbol-table [fn-name full-var-name])
                     (get-in symbol-table [fn-name full-var-name])
                     (get-in symbol-table [:class full-var-name]))
          segment (kind->segment (:kind var-info))]
      [remaining [(str "push " segment " " (:# var-info) "\n")]])

    :else
    [full-code []]))

; TODO: Überprüfen
(defn compile-expression-list
  "Returns [rest-tokens vm-lines arg-count]"
  [tokens fn-name class-name symbol-table]
  (loop [tokens tokens
         vm-code []
         arg-count 0]
    (if (or (empty? tokens) (= ")" (first tokens)))
      [tokens vm-code arg-count]
      (let [[tokens-after-expr vm-expr] (compile-expression tokens fn-name class-name symbol-table)
            new-count (inc arg-count)
            new-vm (concat vm-code vm-expr)]
        (cond
          (and (seq tokens-after-expr) (= "," (first tokens-after-expr)))
          (recur (rest tokens-after-expr) new-vm new-count)
          :else
          [tokens-after-expr new-vm new-count])))))

; TODO: Überprüfen
(defn compile-subroutine-call
  "Returns [remaining-tokens vm-lines]"
  [[current & remaining :as tokens] fn-name class-name symbol-table]
  (let [;; Hilfsfunktion für Variablen-Lookup
        get-var-info (fn [name]
                       (let [full-name   (str class-name "." name)
                             local-scope (get symbol-table fn-name)]
                         (or (get local-scope full-name)
                             (get-in symbol-table [:class full-name]))))
        ;; Hilfsfunktion für Push-Befehl (verwendet kind->segment)
        push-var-cmd (fn [var-info]
                       (let [kind-str (:kind var-info)
                             segment  (kind->segment kind-str)
                             index    (:# var-info)]
                         (str "push " segment " " index "\n")))]
    (if (and (re-matches identifier-regex current)
             (= "(" (first remaining)))
      ;; Fall 1: Direkter Aufruf (Methode der aktuellen Klasse)
      (let [sub-name            current
            [tokens-after-expr vm-expr n-args]
            (compile-expression-list (rest remaining) fn-name class-name symbol-table)
            tokens-after-close   (rest tokens-after-expr)
            full-name            (str class-name "." sub-name)]
        [tokens-after-close
         (concat
           ["push pointer 0\n"]      ; Aktuelles Objekt pushen
           vm-expr
           [(str "call " full-name " " (inc n-args) "\n")])])
      ;; Fall 2: Aufruf mit Klasse oder Objekt
      (let [[first-ident dot ident-sub]         (take 3 tokens)
            tokens-after-name                   (drop 3 tokens)
            [tokens-after-expr vm-expr n-args]
            (compile-expression-list (rest tokens-after-name)
                                     fn-name class-name symbol-table)
            tokens-after-close                  (rest tokens-after-expr)
            var-info                            (get-var-info first-ident)]
        (if var-info
          ;; Objektaufruf: Objekt pushen und Methode aufrufen
          (let [class-nm  (:type var-info)
                full-name (str class-nm "." ident-sub)]
            [tokens-after-close
             (concat
               [(push-var-cmd var-info)]  ; Objekt pushen
               vm-expr
               [(str "call " full-name " " (inc n-args) "\n")])])
          ;; Statischer Aufruf: Kein Objekt pushen
          (let [full-name (str first-ident "." ident-sub)]
            [tokens-after-close
             (concat
               vm-expr
               [(str "call " full-name " " n-args "\n")])]))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; STATEMENTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn compile-statement
  "Returns xml-lines"
  [[current & remaining :as full-code] fn-name class-name symbol-table]

  (cond
    ; Todo anpassen für VM-Code Generierung
    (= "let" current)  (compile-let-statement full-code fn-name class-name symbol-table)
    (= "if" current)  (compile-if-statement full-code fn-name class-name symbol-table)
    (= "while" current)  (compile-while-statement full-code fn-name class-name symbol-table)
    (= "do" current)  (compile-do-statement full-code fn-name class-name symbol-table)
    (= "return" current) (compile-return-statement full-code fn-name class-name symbol-table)
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


(defn compile-let-statement
  "Returns [remaining-tokens vm-lines]"
  [[lett var-name next-token & rest-tokens :as tokens] fn-name class-name symbol-table]

  ;; Lookup variable in symbol table
  (let [get-var-info (fn [name]
                       ; Clojure's or ist hier praktisch :D
                       (or (get-in symbol-table [fn-name (str class-name "." name)])
                           (get-in symbol-table [:class (str class-name "." name)])))
        var-info    (get-var-info var-name)
        kind-str    (:kind var-info)
        segment     (kind->segment kind-str)
        var-index   (str (:# var-info))]

    (if (= "=" next-token)

      ;; Case 1: Simple assignment (ohne Array-Index)
      (let [[rest-after-expr vm-expr] (compile-expression rest-tokens fn-name class-name symbol-table)
            rest-after-semi       (rest rest-after-expr)]
        [rest-after-semi
         (concat vm-expr
                 [(str "pop " segment " " var-index "\n")])])

      ;; Case 2: Array assignment
      (when (= "[" next-token)
        (let [[rest-after-index vm-index] (compile-expression rest-tokens fn-name class-name symbol-table)
              rest-after-bracket       (rest rest-after-index)
              [rest-after-expr vm-expr] (compile-expression (rest rest-after-bracket)
                                                            fn-name class-name symbol-table)
              rest-after-semi          (rest rest-after-expr)]

          [rest-after-semi
           (concat
             vm-index             ; Wert der expression in []
             [(str "push " segment " " var-index "\n")           ; Array-Basisadresse (also *(varName))
              "add\n"]
             vm-expr              ; Ausdruck auswerten
             ["pop temp 0\n"
              "pop pointer 1\n"     ; THAT setzen
              "push temp 0\n"
              "pop that 0\n"])]     ; Wert (also das mit vm-expr berechnete) ins Array schreiben
          )))))



;(compile-let-statement ["let" "i" "[" "5" "]" "=" "3" ";"])


(defn compile-if-statement
  "Returns [remaining-tokens vm-lines]"
  [[if-token & remaining :as tokens] fn-name class-name symbol-table]
  (let [[_open-paren            & rest-after-open]     remaining

        [rest-after-expr vm-expr] (compile-expression rest-after-open fn-name class-name symbol-table)

        [close-paren             & rest-after-close]    rest-after-expr

        [open-brace              & rest-after-brace]    rest-after-close

        [if-block-tokens rest-after-if-block]            (extract-until-balanced rest-after-brace)
        if-statement-groups                             (group-statements if-block-tokens)
        vm-if-block                                      (reduce concat
                                                                 (map #(compile-statement % fn-name class-name symbol-table)
                                                                      if-statement-groups))

        ;; Neue Labels mit korrekter Logik
        true-label  (str "IF_TRUE_" (gensym))
        false-label (str "IF_FALSE_" (gensym))
        end-label   (str "IF_END_" (gensym))

        [else? else-vm-block rest-after-else]
        (if (and (seq rest-after-if-block)
                 (= (first rest-after-if-block) "else"))
          (let [[_else-kw             & rest-after-else-kw]    rest-after-if-block
                [_open-brace2         & rest-after-open2]      rest-after-else-kw
                [else-block-tokens    rest-after-else-block] (extract-until-balanced rest-after-open2)
                else-statement-groups                       (group-statements else-block-tokens)
                else-vm-block                                (reduce concat
                                                                     (map #(compile-statement % fn-name class-name symbol-table)
                                                                          else-statement-groups))]
            [true else-vm-block rest-after-else-block])
          [false [] rest-after-if-block])

        ;; Korrigierte VM-Code Generierung
        vm-code
        (if else?
          ;; Mit else-Zweig
          (concat
            vm-expr
            [(str "if-goto " true-label "\n")   ; Wenn wahr, springe zum if-Block
             (str "goto " false-label "\n")     ; Sonst springe zum else-Block
             (str "label " true-label "\n")]
            vm-if-block
            [(str "goto " end-label "\n")       ; Nach if-Block zu Ende springen
             (str "label " false-label "\n")]   ; Else-Label setzen
            else-vm-block
            [(str "label " end-label "\n")])    ; End-Label setzen

          ;; Ohne else-Zweig
          (concat
            vm-expr
            [(str "if-goto " true-label "\n")   ; Wenn wahr, springe zum if-Block
             (str "goto " false-label "\n")     ; Sonst springe zum Ende
             (str "label " true-label "\n")]
            vm-if-block
            [(str "label " false-label "\n")]))] ; Ende-Label setzen

    [rest-after-else vm-code]))



;(compile-if-statement ["if" "(" "true" ")" "{" "let" "i" "=" "1" ";" "}"])


(defn compile-while-statement
  "Returns [remaining-tokens vm-lines]"
  [[while-token & remaining :as tokens] fn-name class-name symbol-table]
  (assert (= "while" while-token) "Expected 'while' keyword")

  (let [start-label (str "WHILE_START_" (gensym))
        end-label (str "WHILE_END_" (gensym))

        ;; Öffnende Klammer überspringen
        [_open-paren & rest-after-open] remaining
        _ (assert (= "(" _open-paren))

        ;; Bedingungsausdruck kompilieren
        [rest-after-expr vm-expr] (compile-expression rest-after-open fn-name class-name symbol-table)

        ;; Schließende Klammer überspringen
        [close-paren & rest-after-close] rest-after-expr
        _ (assert (= ")" close-paren))

        ;; Öffnende geschweifte Klammer überspringen
        [open-brace & rest-after-brace] rest-after-close
        _ (assert (= "{" open-brace))

        ;; While-Block-Tokens extrahieren
        [while-block-tokens rest-after-while] (extract-until-balanced rest-after-brace)

        ;; While-Block-Statements gruppieren und kompilieren
        while-statement-groups (group-statements while-block-tokens)
        vm-while-body (reduce concat (map #(compile-statement % fn-name class-name symbol-table) while-statement-groups))

        ;; VM-Code für While-Schleife generieren
        vm-code
        (concat
          [(str "label " start-label "\n")] ; Schleifenanfang
          vm-expr                     ; Bedingung auswerten
          ["not\n"]                     ; Negieren, wir wollen ja ausführen, solange bspw. (i<3) ist und springen, falls nicht wahr
          [(str "if-goto " end-label "\n")  ; Wenn false (0), springe zum Ende
           vm-while-body              ; Schleifenkörper
           (str "goto " start-label "\n")   ; Zurück zum Anfang springen
           (str "label " end-label "\n")])] ; Schleifenende

    [rest-after-while vm-code]))

;(compile-while-statement ["while" "(" "i" "<" "5" ")" "{" "let" "i" "=" "i" "-" "1" ";" "}"])

(defn compile-do-statement
  "Returns [remaining-tokens vm-lines]"
  [[do-token & remaining :as tokens] fn-name class-name symbol-table]

  (let [[tokens-after-call vm-call] (compile-subroutine-call remaining fn-name class-name symbol-table)
        rest-after-semi (rest tokens-after-call)]

    [rest-after-semi
     (concat vm-call
             ["pop temp 0\n"])]))

;(compile-do-statement ["do" "Output" "." "print" "(" "5" "," "6" ")" ";"])


(defn compile-return-statement
  "Returns [remaining-tokens vm-lines]"
  [[return-token & remaining :as tokens] fn-name class-name symbol-table]
  (assert (= "return" return-token) "Expected 'return' keyword")

  (let [;; Prüfen, ob ein Ausdruck vorhanden ist
        [rest-after-expr vm-expr] (if (not= (first remaining) ";")
                                    (compile-expression remaining fn-name class-name symbol-table)
                                    [remaining []])

        ;; Semikolon konsumieren
        semicolon (first rest-after-expr)
        _ (assert (= ";" semicolon) (str "Expected ';' but got " semicolon))
        rest-after-semi (rest rest-after-expr)

        ;; VM-Code generieren
        vm-code (if (empty? vm-expr)
                  ;; Kein Ausdruck (void-Methode) - 0 pushen
                  ["push constant 0\n" "return\n"]

                  ;; Mit Ausdruck - generierten Code + return
                  (concat vm-expr ["return\n"]))]

    [rest-after-semi vm-code]))

;(compile-return-statement ["return" "i" "+" "5" ";"])





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


(defn compile-subroutine-body
  "Returns xml-lines"
  [[open-curly & r] fn-name class-name symbol-table]
  (let [; Jeweils closing } des bodys wegnehmen
        var-decs-code (pr2 (take-while #(not (re-matches #"let|if|while|do|return" %)) (butlast r)))
        statements-code (pr2 (drop-while #(not (re-matches #"let|if|while|do|return" %)) (butlast r)))

        ; varDecs sind uns jetzt egal, das abstrahieren wir mit "function Main.main 3" direkt, Typsicherheit haben wir nicht
        ;var-decs-split-code (pr2 (split-coll-by-sep var-decs-code ";"))
        ;var-decs-xml (pr2 (reduce concat (map var-dec-to-xml var-decs-split-code)))
        statements-split-code (pr2 (group-statements statements-code))

        statements-vm-code (flatten (map #(compile-statement % fn-name class-name symbol-table) statements-split-code))]
    statements-vm-code))


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


(defn split-coll-at-index
  "Teilt `coll` so auf, dass alle Elemente bis inklusive `idx`
   in der ersten, und der Rest in der zweiten Coll landen."
  [coll idx]
  (split-at (inc idx) coll))

(defn compile-single-subroutine-dec
  [class-name symbol-table [what type subroutineName open-parantheses & r]]
  (let [cnt (count (get symbol-table subroutineName))
        local-var-count (count (filter #(= "var" (:kind %))
                                       (vals (get symbol-table subroutineName))))
        start-vm-code ["function " (str class-name "." subroutineName) " " local-var-count "\n"]

        ;; TODO: Etwas weiteren Code vorher
        ;;  method -> lade this push argument 0, pop pointer 0
        ;;  constructor -> push constant nFields, call Memory.alloc 1, pop pointer 0
        ;;  Reminder: pop pointer 0 setzt this
        ;;            pop pointer 1 setzt that
        middle-code (if (= "method" what)
                      ["push argument 0\n" "pop pointer 0\n"]
                      (if (= "constructor" what)
                        [(str "push constant " (count (filter #(= "field" (:kind %))
                                                              (vals (:class symbol-table)))) "\n") "call Memory.alloc 1\n" "pop pointer 0\n"]
                        []))
        closing-parantheses-index (.indexOf r ")")
        [parameter-list-code subroutine-body-code] (split-coll-at-index r closing-parantheses-index)

        subroutine-body-vm-code (pr2 (compile-subroutine-body subroutine-body-code subroutineName class-name symbol-table))]
    (into start-vm-code (into middle-code subroutine-body-vm-code))
    )
  )

(count {:a 1 :b 2})

(.indexOf ["(" "int" "x" ")" "{" ")"] ")")

(defn compile-subroutine-dec
  "Returns vm-code"
  [[current & remaining :as full-code] class-name symbol-table]
  (let [subroutine-dec-code (into [] (drop-while #(not (re-matches #"function|method|constructor" %)) full-code))
        blocks (pr2 (into [] (group-subroutine-decs subroutine-dec-code)))
        declarations-vm-code (pr2 (reduce concat (map (partial compile-single-subroutine-dec class-name symbol-table) blocks)))
        useless (print 1)]
    declarations-vm-code
    ))

;(compile-subroutine-dec
;  ["function" "int" "a" "(" ")" "{" "}" "method" "int" "b" "(" "int" "x" "," "int" "y" ")" "{" "}"] [])

;;;; classVarDec

;(defn compile-single-class-var-dec
;  "Returns XML for one class-var-dec"
;  [[what type name & r]]
;  (if (nil? what)
;    []
;    (let [start ["<classVarDec>\n"]
;          what-xml ["<keyword>" what "</keyword>\n"]
;          type-xml (if (re-matches #"int|char|boolean" type) ["<keyword>" type "</keyword>\n"] ["<identifier>" type "</identifier>\n"])
;          var-name-xml ["<identifier>" name "</identifier>\n"]
;          middle (loop [[t1 t2 & re] r
;                        xml []]
;                   (if (= ";" t1)
;                     (into xml ["<symbol>;</symbol>\n"])
;                     (recur re (into xml ["<symbol>" "," "</symbol>\n" "<identifier>" t2 "</identifier>\n"])))
;                   )
;          end ["</classVarDec>\n"]]
;      (into start (into what-xml (into type-xml (into var-name-xml (into middle end))))))))

;(compile-single-class-var-dec ["static" "int" "hello" "," "world" "," "is" "," "outdated" ";"])



;(compile-parameter-list ["(" "int" "x" "," "int" "y" "," "SquareGame" "squareGame" ")"])










;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; FÜR COMPILER 2 ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; NOTE: Bei method beginnen die Argumente bei 1 und "this" bekommt den 0-ten Eintrag
; EDIT: Kleiner Fehler, es wurde type mit "method" verglichen statt what
(defn gen-tab-for-func
  "Generates a symbol table for one function/method"
  [[what type subroutine-name open-paren & remaining :as code] class-name]
  (let [[param-list-tokens after-params]
        (split-at (count (take-while #(not= ")" %) remaining))
                  remaining)
        after-params' (rest after-params)
        param-pairs   (->> param-list-tokens
                           (remove #(= "," %))
                           (partition 2))
        ;; Generate indices based on subroutine type
        n-params     (count param-pairs)
        [indices param-pairs']
        (if (= "method" what)
          ;; For methods: add 'this' as first parameter
          [(range 0 (inc n-params))
           (cons [class-name "this"] param-pairs)]
          ;; For functions/constructors: use as-is
          [(range 0 n-params)
           param-pairs])

        table-with-args
        (reduce (fn [tab [idx [typ name]]]
                  (assoc tab (str class-name "." name)
                             {:type typ :kind "argument" :# idx}))
                {}
                (map vector indices param-pairs'))

        [body-tokens after-body]
        (extract-until-balanced-full-method after-params')
        var-decls
        (filter #(= "var" (first %))
                (split-coll-by-sep (rest body-tokens) ";"))

        process-var-dec
        (fn [tab [var-keyword var-type & names-and-commas]]
          (let [names-and-commas' (butlast names-and-commas)
                var-names         (remove #(= "," %) names-and-commas')
                ;; Bestimme den nächsten freien Index basierend auf der aktuellen Tabelle
                start-index       (count (filter #(= (:kind %) "var") (vals tab)))]
            ;; Erstelle für jede Variable einen eigenen Eintrag mit aufsteigendem Index
            (loop [new-tab tab
                   idx      start-index
                   [name & rest-names] var-names]
              (if name
                (recur (assoc new-tab (str class-name "." name)
                                      {:type var-type :kind "var" :# idx})
                       (inc idx)
                       rest-names)
                new-tab))))

        final-table
        (reduce process-var-dec table-with-args var-decls)]
    [final-table after-body]))



; Das Beispiel klappt
(gen-tab-for-func ["function" "int" "coolFunc" "(" "int" "x" "," "int" "y" ")" "{" "var" "int" "a" "," "b" ";" "var" "int" "c" ";" "}"] "Main")


(defn generate-symbol-table
  "Returns symbol table"
  [full-code class-name]
  (loop [tables {:class {}}
         [current & remaining :as full-code] full-code
         inline-kind ""
         inline-type ""
         segment-counters {"static" 0 "field" 0}]
    (cond
      (nil? current) tables

      (= "static" current) (recur (assoc-in tables [:class (str class-name "." (second remaining))] {:kind "static" :type (first remaining) :# (get segment-counters "static")})
                                  (rest (rest remaining))
                                  "static"
                                  (first remaining)
                                  (update segment-counters "static" inc))
      (= "field" current) (recur (assoc-in tables [:class (str class-name "." (second remaining))] {:kind "field" :type (first remaining) :# (get segment-counters "field")})
                                 (rest (rest remaining))
                                 "field"
                                 (first remaining)
                                 (update segment-counters "field" inc))

      (= "," current) (recur (assoc-in tables [:class (str class-name "." (first remaining))] {:type inline-type :kind inline-kind :# (get segment-counters inline-kind)})
                             (rest remaining)
                             inline-kind
                             inline-type
                             (update segment-counters inline-kind inc))

      (re-matches #"function|method|constructor" current)
      (let [[func-tab rem-code] (gen-tab-for-func full-code class-name)
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


(generate-symbol-table array-test-tokens "Main")

;(generate-symbol-table expression-less-test-tokens "Main")


;(generate-symbol-table expression-less-test-tokens2)
;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; compile-class
;;;;;;;;;;;;;;;;;;;;;;;;

(defn compile-class
  "Returns vm code - Top level compilation function"
  ;[[current & remaining :as full-code] xml-lines]
  [[cl class-name brace & remaining :as full-code]]

  (when (and
          (= cl "class")
          (re-matches identifier-regex class-name)
          (= brace "{"))
    (let [symbol-table (generate-symbol-table full-code class-name)
          ;class-var-dec-vm-code (doall (compile-class-var-decs (butlast remaining) symbol-table))
          subroutine-dec-vm-code (pr2 (compile-subroutine-dec (butlast remaining) class-name symbol-table))]      ; Mit butlast jeweils das closing } entfernen
      subroutine-dec-vm-code
      )))


;;;;;;;;;;;;;;;;;;
;;; Aus Compiler 1
;;;;;;;;;;;;;;;;;;
(defn result-vm-to-file
  [file-path coll]
  (let [output-path (str/replace file-path #"\.jack$" ".vm")]
    (spit output-path (apply str coll))))
; oder (str/join coll)

(defn compile-jack
  [filepath]
  (let [tokens (tokenize filepath)
        compiled (compile-class tokens)]
    (result-vm-to-file filepath compiled)))

(defn -main [& args]
  (write-tokenizer-to-file (first args))
  (compile-jack (first args)))


;;;;;;;;;;;;;;;;;
;; ArrayTest
;;;;;;;;;;;;;;;;;

; diff -w ArrayTest/Main.jack ArrayTest/Mainout.jack
; Diff enthält nur labels, Ausführung klappt, sobald man die Mainout.jack in Main.jack umbenennt
;(compile-jack "src/10/ArrayTest/Main.jack")

;;;;;;;;;;;;;;;;;;;;;;;
;; ExpressionLessSquare - kompiliert teilweise nicht, daher auslassen
;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;
;; Square - kann auch ausgeführt werden :)
;;;;;;;;;;;;;;;;;

; diff -w Square/Main.xml Square/Mainout.xml
;(compile-jack "src/10/Square/Main.jack")

; diff -w Square/Square.xml Square/Squareout.xml
;(compile-jack "src/10/Square/Square.jack")

; diff -w Square/SquareGame.xml Square/SquareGameout.xml
;(compile-jack "src/10/Square/SquareGame.jack")



; Alles diffs:
; diff -w ArrayTest/Main.vm ArrayTest/Mainout.vm
; diff -w ExpressionLessSquare/Main.xml ExpressionLessSquare/Mainout.xml
; diff -w ExpressionLessSquare/Square.xml ExpressionLessSquare/Squareout.xml
; diff -w ExpressionLessSquare/SquareGame.xml ExpressionLessSquare/SquareGameout.xml
; diff -w Square/Main.xml Square/Mainout.xml
; diff -w Square/Square.xml Square/Squareout.xml
; diff -w Square/SquareGame.xml Square/SquareGameout.xml




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; NEUE TESTS FÜR PROJEKT 11
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Klappt direkt, identisch zu Average aus Projekt 10
;(compile-jack "src/11/Average/Main.jack")

; Klappt direkt
;(compile-jack "src/11/ComplexArrays/Main.jack")

; Klappt scheinbar, Inhalt der RAM-Zellen 8001-8016 identisch zur Vorgabe-VM-Datei
; Kann die RAM-Zellen aber nur in der Online-Variante setzen
;(compile-jack "src/11/ConvertToBin/Main.jack")


; Das war in der gen-tab-for-func ein Fehler beim Abgleich, ob method oder function (statt Vergleich mir what war mit type)
(do
  ; diff 11/PongVorgabeVM/Ball.vm 11/Pong/Ball.vm
  (compile-jack "src/11/Pong/Ball.jack")

  ; diff 11/PongVorgabeVM/Bat.vm 11/Pong/Bat.vm
  (compile-jack "src/11/Pong/Bat.jack")

  ; diff 11/PongVorgabeVM/Main.vm 11/Pong/Main.vm
  (compile-jack "src/11/Pong/Main.jack")

  ; diff 11/PongVorgabeVM/PongGame.vm 11/Pong/PongGame.vm
  (compile-jack "src/11/Pong/PongGame.jack"))


; diff 11/Seven/Main.vm 11/SevenVorgabeVM/Main.vm
(compile-jack "src/11/Seven/Main.jack")


(do
  ; diff 11/SquareVorgabeVM/Main.vm 11/Square/Main.vm
  (compile-jack "src/11/Square/Main.jack")

  ; diff 11/SquareVorgabeVM/Square.vm 11/Square/Square.vm
  (compile-jack "src/11/Square/Square.jack")

  ; diff 11/SquareVorgabeVM/SquareGame.vm 11/Square/SquareGame.vm
  (compile-jack "src/11/Square/SquareGame.jack"))

