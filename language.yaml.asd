;;;; language.yaml.asd --- System definition for the language.yaml system.
;;;;
;;;; Copyright (C) 2012-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "language.yaml"
  :description    "Processing of YAML 1.2 documents."
  :license        "LLGPLv3" ; see COPYING file for details

  :author         "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer     "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :mailto         "jmoringe@techfak.uni-bielefeld.de"
  :homepage       "https://github.com/scymtym/language.yaml/"
  :bug-tracker    "https://github.com/scymtym/language.yaml/issues"
  :source-control (:git "https://github.com/scymtym/language.yaml.git")

  :version        (:read-file-form "version-string.sexp")
  :depends-on     ((:version "language.yaml.parser"    (:read-file-form "version-string.sexp"))
                   (:version "language.yaml.construct" (:read-file-form "version-string.sexp")))

  :components     ((:module     "src"
                    :serial     t
                    :components ((:file       "package")
                                 (:file       "protocol"))))

  :in-order-to    ((test-op (test-op "language.yaml/test"))))

(defsystem "language.yaml/test"
  :description "Unit tests for the language.yaml system."
  :license     "LLGPLv3" ; see COPYING file for details

  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ((:version "let-plus"      "0.2")

                (:version "fiveam"        "1.4")

                (:version "language.yaml" (:read-file-form "version-string.sexp")))
  :components  ((:module     "test"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol"))))

  :perform     (test-op (operation component)
                 (uiop:symbol-call '#:language.yaml.test '#:run-tests)))
