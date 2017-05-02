;;;; language.yaml.tags.asd --- System definition for the language.yaml.tags system.
;;;;
;;;; Copyright (C) 2012-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "language.yaml.tags"
  :description "Class and repository for YAML tags."
  :license     "LLGPLv3" ; see COPYING file for details

  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"

                (:version "more-conditions"       "0.1")
                (:version "utilities.print-items" "0.1")

                (:version "language.yaml.base"    (:read-file-form "version-string.sexp")))

  :components  ((:module     "src"
                 :pathname   "src/tags"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "conditions")
                              (:file       "protocol")
                              (:file       "tag")
                              (:file       "resolver")
                              (:file       "builtin"))))

  :in-order-to ((test-op (test-op "language.yaml.tags/test"))))

(defsystem "language.yaml.tags/test"
  :description "Unit tests for the language.yaml.tags system."
  :license     "LLGPLv3" ; see COPYING file for details

  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ((:version "fiveam"             "1.4")

                (:version "language.yaml.tags" (:read-file-form "version-string.sexp")))

  :components  ((:module     "test"
                 :pathname   "test/tags"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "tag"))))

  :perform     (test-op (operation component)
                 (uiop:symbol-call '#:language.yaml.tags.test '#:run-tests)))
