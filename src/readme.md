# Anleitung zum Ausführen der "VM Implementation - Version 2" 


## A. Mit Kommandozeile 

### 1. Clojure installieren 
MacOS: ```brew install clojure/tools/clojure``` <br>
Windows weiß ich nicht :/

### 2. Navigiere in den Ordner vm2 selber

### 3. Führe aus: 
```clojure -M -m vm1 "<path1> <path2>""```

Die Pfade sollten dabei relativ zum momentanen Ordner (vm1) sein. Die benötigten Tests liegen auch alle in diesem Projekt.<br>
Sofern der angegeben Pfad ein ORDNER ist, so wird eine `.asm` aus ALLEN `.vm`-Dateien in diesem erzeugt **MIT** Bootstrap-Code. <br>
Sofern der angegeben Pfad eine DATEI ist, so wird eine `.asm` nur aus dieser erzeugt **OHNE** Bootstrap-Code.

### 4. Die .asm-Dateien liegen jeweils auf Ebene der .vm-Dateien bzw. in dem Ordern der angegeben wird

