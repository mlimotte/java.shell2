# java.shell2

A Clojure library to facilitate launching of sub-processes and piping (streaming) data.

## Features

* A declarative syntax for defining new processes to specify input, output, encoding, and other bevhavior
* Handling for common use-cases (i.e. pass stdout/err of the process to the same destination as the parent, merge stderr of the process to stdout, output directly to a File, etc)
* The pipe macro handles all the complexity of managing multipe streams and threads for stremaing data
through multiple processes and clojure functions.
* backward compatible with existing code that uses clojure.java.shell (i.e. a drop-in replacement)

## Examples

#### Running a process

```clojure
user=> (use 'clojure.java.shell2)
user=> (import 'java.io.StringReader)  ;input can be a Stream, Reader, File, byte[] or String
user=> (def input (StringReader. "line1\nline2\n"))
```

simple example, calling unix sort on some input data
```clojure
user=> (sh "sort" :in input)
{:err "" :exit 0 :out "line1\nline2\n"}
```

More complex example: count the lines of input, pass the stderr to stderr
of the JVM, and specify a fn to capture the output and post-process it.
```clojure
user=> (require '[clojure.string :as string])
user=> (sh "wc" "-l" :in input :err :pass
                     :out #(string/trim (slurp %)))
{:err nil :exit 0 :out "2"}
user=> (:out *1)
"2"
```

#### Streaming data through process/fns

A simple example, my-filter-fn would be a a-arg function called with an
InputStream to read the output of the previous process and an OutputStream
to write results.  Note that the data is streamed through these functions
asynchronously.
```clojure
(pipe
  (sh "cat" :in input)
  my-filter-fn
  (sh "wc" "-l"))
```

Another example, using the convenience wrapper functions.  wrap-text-lines,
for example, wraps a function that receives a line-seq as it's only output,
and the return value of the fn is used as the output.  The RV is expected
to be a seq of lines which are streamed to the outstream separated by the
system line separator.
```clojure
(pipe
  (sh "ls" "-l")
  (sh "sed" "p")
  (wrap-text-lines #(filter (partial re-find #"project.clj") %))
  (sh "wc" "-l")
  (wrap count :in :line-seq :out :forward)
  (sh "cat"))
```

## Usage

Add to leiningen

`:dependencies [com.climate/java.shell2 "0.1.0"]`

Processes are started with clojure.java.shell2/sh.  Multiple sh invocations
and fn calls can be coordinated with clojure.java.shell2/pipe.  Inside the
pipe, fn arguments receive two args [in out].  But you can use the wrapper
convenience functions (wrap and wrap-text-lines) to change this.

See the docstrings for details.

## Compatability

Tested with Clojure 1.2 - 1.5.1

## Credit

This work is an extension of clojure.java.shell by Chris Houser and Stuart Halloway.

clojure.java.shell2 was developed at The Climate Corporation.  Thanks to climate.com for 
allowing it to be open-sourced.

## License

Distributed under the Eclipse Public License, the same as Clojure.
