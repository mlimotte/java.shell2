# java.shell2

A Clojure library to facilitate launching of sub-processes


WIP WIP WIP WIP
WORK IN PROGRESS


## Usage

Add to leiningen :dependencies
  [???  java.shell2 "0.1.0"]

(require '[clojure.java.shell2 :as sh])

### Use (clojure.java.shell2/sh) to start a process.

user=> (import 'java.io.StringReader)
user=> (require '[clojure.string :as string])
user=> (:out (sh "wc" "-l" :in (StringReader. "line1\nline2\n") :err :pass :out #(string/trim (slurp %))))
"2"

### Piping

*You can pipe out from a process to a Clojure function and back to a process.*

##### Example

(pipe
  (sh "ls" "-l")
  (sh "sed" "p")
  (wrap-text-lines #(filter (partial re-find #"project.clj") %))
  ;(sh "wc" "-l")
  (wrap count :in :line-seq :out :forward)
  (sh "cat")
  )



## License

Distributed under the Eclipse Public License, the same as Clojure.
