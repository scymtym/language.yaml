;;;; language.yaml.construct.asd --- System definition for the language.yaml.construct system.
;;;;
;;;; Copyright (C) 2017, 2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "language.yaml.construct"
  :description "Constructing native representations of YAML 1.2 documents"
  :license     "LLGPLv3" ; see COPYING file for details

  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ((:version "let-plus"                      "0.2")
                (:version "more-conditions"               "0.1")
                (:version "utilities.print-items"         "0.1")

                (:version "architecture.builder-protocol" "0.10")
                (:version "parser.common-rules"           "0.4")

                (:version "language.yaml.base"            (:read-file-form "version-string.sexp"))
                (:version "language.yaml.tags"            (:read-file-form "version-string.sexp")))

  :components  ((:module     "src"
                 :pathname   "src/construct"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol")
                              (:file       "mixins")
                              (:file       "lisp-builder"))))

  :in-order-to ((test-op (test-op "language.yaml.construct/test"))))

(defsystem "language.yaml.construct/test"
  :description "Unit tests for the language.yaml.construct system."
  :license     "LLGPLv3" ; see COPYING file for details

  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ((:version "fiveam"                  "1.4")

                (:version "language.yaml.construct" (:read-file-form "version-string.sexp")))

  :components  ((:module     "test"
                 :pathname   "test/construct"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "lisp-builder"))))

  :perform     (test-op (operation component)
                 (uiop:symbol-call '#:language.yaml.construct.test '#:run-tests)))
