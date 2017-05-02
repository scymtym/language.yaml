;;;; language.yaml.parser.asd --- System definition for the language.yaml.parser system.
;;;;
;;;; Copyright (C) 2012-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "language.yaml.parser"
  :description "Parsing of YAML 1.2 documents."
  :license     "LLGPLv3" ; see COPYING file for details

  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  (:alexandria
                (:version "let-plus"                      "0.2")

                (:version "architecture.builder-protocol" "0.1")
                (:version "esrap"                         "0.14")
                (:version "parser.common-rules"           "0.1")

                (:version "language.yaml.base"            (:read-file-form "version-string.sexp")))

  :components  ((:module     "src"
                 :pathname   "src/parser"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "types")
                              (:file       "variables")
                              (:file       "grammar"))))
  :in-order-to ((test-op (test-op "language.yaml.parser/test"))))

(defsystem "language.yaml.parser/test"
  :description "Unit tests for the language.yaml.parser system."
  :license     "LLGPLv3" ; see COPYING file for details

  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"
                "let-plus"

                (:version "fiveam"               "1.4")
                (:version "esrap"                "0.14")

                (:version "language.yaml.parser" (:read-file-form "version-string.sexp")))

  :components  ((:module     "test"
                 :pathname   "test/parser"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "util")
                              (:file       "grammar"))))

  :perform    (test-op (operation component)
                (uiop:symbol-call '#:language.yaml.parser.test '#:run-tests)))
