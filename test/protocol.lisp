;;;; protocol.lisp --- Test for protocol functions.
;;;;
;;;; Copyright (C) 2013-2018 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.yaml.test)

(in-suite :language.yaml)

(test dump.smoke
  "Smoke test for the `dump' function."

  (with-output-to-string (stream)
    (language.yaml:dump '((:a . 1)) stream)))

(test load.list-builer.smoke
  "Smoke test for the `load' function using a `list' builder."

  (mapc (lambda+ ((input expected))
          (is (equal expected (language.yaml:load input :rule :root))))
        '(("- &foo 1
- *foo"
           (1 1)))))
