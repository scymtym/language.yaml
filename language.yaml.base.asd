;;;; language.yaml.base.asd --- System definition for the language.yaml.base system.
;;;;
;;;; Copyright (C) 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem :language.yaml.base
  :description "Basic types for representing YAML documents."
  :license     "LLGPLv3" ; see COPYING file for details

  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")

  :components  ((:module     "src"
                 :pathname   "src/base"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "types"))))

  :in-order-to ((test-op (test-op :language.yaml.base/test))))

(defsystem :language.yaml.base/test
  :description "Unit tests for the language.yaml.base system."
  :license     "LLGPLv3" ; see COPYING file for details

  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ((:version :fiveam             "1.3")

                (:version :language.yaml.base (:read-file-form "version-string.sexp")))

  :components  ((:module     "test"
                 :pathname   "test/base"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "types")))))

(defmethod perform ((operation test-op)
                    (component (eql (find-system :language.yaml.base/test))))
  (uiop:symbol-call '#:language.yaml.base.test '#:run-tests))
