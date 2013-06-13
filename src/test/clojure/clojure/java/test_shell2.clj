;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.java.test-shell2
  (:use clojure.test
        [clojure.java.shell2 :as sh])
  (:require [clojure.java.io :as io]
            [clojure.string :as s])
  (:import (java.io File StringWriter PrintWriter)))

(def platform-enc (.name (java.nio.charset.Charset/defaultCharset)))
(def default-enc "UTF-8")

(deftest test-parse-args
  (are [x y] (= x y)
       [[] {:in-enc default-enc :out-enc default-enc :out :capture :err :capture :dir nil :env nil}] (#'sh/parse-args [])
       [["ls"] {:in-enc default-enc :out-enc default-enc :out :capture :err :capture :dir nil :env nil}] (#'sh/parse-args ["ls"])
       [["ls" "-l"] {:in-enc default-enc :out-enc default-enc :out :capture :err :capture :dir nil :env nil}] (#'sh/parse-args ["ls" "-l"])
       [["ls"] {:in-enc default-enc :out-enc "ISO-8859-1" :out :capture :err :capture :dir nil :env nil}] (#'sh/parse-args ["ls" :out-enc "ISO-8859-1"])
       [[] {:in-enc platform-enc :out-enc platform-enc :out :capture :err :capture :dir nil :env nil}] (#'sh/parse-args [:in-enc platform-enc :out-enc platform-enc])
       [["ls" "-l"] {:in-enc default-enc :out-enc default-enc :out :pass :err :capture :dir nil :env nil}] (#'sh/parse-args [["ls" "-l"] :out :pass])))

(deftest test-with-sh-dir
  (are [x y] (= x y)
    nil *sh-dir*
    "foo" (with-sh-dir "foo" *sh-dir*)))

(deftest test-with-sh-env
  (are [x y] (= x y)
    nil *sh-env*
    {:KEY "VAL"} (with-sh-env {:KEY "VAL"} *sh-env*)))

(deftest test-as-env-strings
  (are [x y] (= x y)
    nil (#'sh/as-env-strings nil)
    ["FOO=BAR"] (seq (#'sh/as-env-strings {"FOO" "BAR"}))
    ["FOO_SYMBOL=BAR"] (seq (#'sh/as-env-strings {'FOO_SYMBOL "BAR"}))
    ["FOO_KEYWORD=BAR"] (seq (#'sh/as-env-strings {:FOO_KEYWORD "BAR"}))))

(deftest test-form-starts-with-sym?
  (are [x y] (= x y)
    false (#'sh/form-starts-with-sym? `(str "hello") sh)
    nil (#'sh/form-starts-with-sym? `(unknown-sym-sjhgd237 "hello") sh)
    true (#'sh/form-starts-with-sym? `(sh str "hello") sh)
    true (#'sh/form-starts-with-sym? `(sh/sh str "hello") sh)))

(let [outputf (File/createTempFile "shell-test-out" nil)
      inputf (doto (File/createTempFile "shell-test-in" nil) (spit "b\na\n"))]

  (deftest test-sh
    ; output to a file
    (sh "echo" "-n" "util test" "output" :out outputf)
    (is (= "b\na\n" (slurp inputf)))

    ; output to a Writer (OutputStream should work as well)
    (let [sw (StringWriter.)]
      (sh "echo" "-n" "hello" :out sw)
      (is (= "hello" (str sw))))

    (are [x y] (= x y)
      ; basic test, :out :capture
      {:exit 0 :out "util test output\n" :err ""}
      (sh "echo" "util test" "output")

      ; :out :pass
      {:exit 0 :out nil :err ""}
      (sh "echo" "util test" "output" :out :pass)

      ; non-zero exit status
      {:exit 1 :out "" :err ""}
      (sh "false")

      ; :err :capture
      {:exit 0 :out "" :err "hello\n"}
      (sh "bash" "-c" "echo hello 1>&2")

      ; :merge err into out
      {:exit 0 :out "hello\n" :err nil}
      (sh "bash" "-c" "echo hello 1>&2" :err :merge)

      ; :out fn
      {:exit 0, :out '("hello"), :err nil}
      (sh "bash" "-c" "echo hello 1>&2" :err :merge
          :out (fn [strm] (-> strm io/reader line-seq doall)))))

  (deftest pipe-test
    ; a pipe with 0 forms
    (is (= [nil] (pipe)))

    ; a pipe with 1 form
    (is (= [{:exit 0 :out "hello" :err ""}] (pipe (sh "echo" "-n" "hello"))))

    ; simple pipe with two forms
    (is (= [{:exit 0 :out nil :err ""} {:exit 0 :out "a\nb\n" :err ""}]
           (pipe (sh "cat" :in inputf) (sh "sort"))))

    ; A non-zero exit code does not terminate the process
    ; This is consistent with a bash pipe
    (is (= [{:exit 0 :out nil :err nil}
            {:exit 0 :out nil :err nil}
            {:exit 1 :out nil :err ""}
            {:exit 0 :out "       0\n" :err ""}]
           (pipe (sh "cat" :in inputf :err :pass)
                 (sh "sort" :err :pass)
                 (sh "false")  ; non-zero exit
                 (sh "wc" "-l"))))

    ; pipe stderr by merging it to out
    (is (= [{:exit 1 :out nil :err nil}
            {:exit 0 :out nil :err ""}
            {:exit 0 :out "       1\n" :err ""}]
           (pipe (sh "ls" "-l" "non-existant-file-qewutdf2378" :err :merge)
                 (sh "grep" "non")
                 (sh "wc" "-l"))))

    ; stderr text is :capture(d) by default
    (is (= [{:exit 1 :out nil
             :err "ls: non-existant-file-qewutdf2378: No such file or directory\n"}
            {:exit 0 :out "       0\n" :err ""}]
           (pipe (sh "ls" "-l" "non-existant-file-qewutdf2378")
                 (sh "wc" "-l"))))

    ; a pipe with a function
    (is (= [{:exit 0 :out nil :err ""}
            nil
            {:exit 0 :out "(\"a\" \"b\")\n" :err ""}]
           (pipe (sh "cat" :in inputf)
                 (fn [in out] (->> in io/reader line-seq sort (spit out)))
                 (sh "sort"))))

    ; function is first
    (is (= [nil {:exit 0 :out "line0\nline1\nline2\n" :err ""}]
           (pipe (fn [out] (with-open [w (java.io.PrintWriter. (io/writer out))]
                             (doseq [i (range 3)]
                               (.println w (str "line" i)))))
                 (sh "cat"))))

    ; function is last
    (is (= [{:exit 0 :out nil :err ""}
            ["a" "b"]]
           (pipe (sh "cat" :in inputf)
                 (fn [in] (->> in io/reader line-seq sort))))))

  (deftest test-wrap

    ; Deafult wrap behavior - an example of non-blocking streaming
    ; causing some side-effect
    (let [state (atom 0)
          result
            (pipe
              (sh "echo" "sweet home alabama\nno place like home\nmi casa es su casa")
              ;(wrap
                (fn [in out]
                  (with-open [wrt (PrintWriter. out)]
                    (doall
                      (for [line (->> in io/reader line-seq)
                            :when (re-find #"home" line)]
                        (do (swap! state inc)
                            (.println wrt line))))))
                ;)
              (sh "wc" "-l"))]
      (is (= 2 @state))
      (is (= "       2\n" (-> result last :out))))

    ; fn in tail
    (is (= [{:exit 0 :out nil :err ""} {:exit 0 :out 2 :err nil}]
           (pipe
             (sh "cat" :in inputf)
             (wrap count :in :line-seq :out :none))))

    ; line-seq input, fn at head and fn at tail of pipe, so set :in
    ; and :out to :none as appropriate.
    (is (= [{:exit 0 :out nil :err nil}
            {:exit 0 :out nil :err ""}
            {:exit 0 :out ["1"] :err nil}]
           (pipe
             (wrap (fn [out] (spit out "foo\n")) :in :none)
             (sh "wc" "-l")
             (wrap (comp doall (partial map s/trim))
                   :in :line-seq :out :none))))

    ; :out :forward,   AND   :rv :identity
    (is (= [{:exit 0 :out nil :err ""} nil {:exit 0 :out "2\n" :err ""}]
           (pipe
             (sh "cat" :in inputf)
             (wrap count :in :line-seq :out :forward :rv :identity)
             (sh "sort"))))

    ; output to a writer
    (is (= [{:exit 0 :out nil :err ""}
            {:exit 0 :out nil :err nil}
            {:exit 0 :out "2\n" :err ""}]
           (pipe
             (sh "cat" :in inputf)
             (wrap (fn [in-seq wrt] (->> in-seq count (.println wrt)))
                   :in :line-seq :out :writer)
             (sh "sort"))))

    ; :in :reader   AND  :out :forward-lines
    (is (= [{:exit 0 :out nil :err ""}
            {:exit 0 :out nil :err nil}
            {:exit 0 :out "#a\n#b\n" :err ""}]
           (pipe
             (sh "cat" :in inputf)
             (wrap (comp doall (partial map #(str "#" %)) line-seq)
                   :in :reader :out :forward-lines)
             (sh "sort")))))

  (deftest test-wrap-text-lines
    (is (= [{:exit 0 :out nil :err ""}
            {:exit 0 :out nil :err nil}
            {:exit 0 :out "#a\n#b\n" :err ""}]
           (pipe
             (sh "cat" :in inputf)
             (wrap-text-lines (comp doall (partial map #(str "#" %))))
             (sh "sort"))))))
