(cl:in-package #:language.yaml.tags.test)

(in-suite language.yaml.tags)

(test expand-shorthand.smoke
  "Smoke test for the `expand-shorthand' method specialized on
   `standard-expander'."

  (let ((expander (make-instance 'standard-expander))
        ;; (r (make-instance ))
        )
    (mapc (lambda+ ((prefix suffix expected))
            (is (equal expected (expand-shorthand expander prefix suffix))))
          '(("!"   "bar" "!bar")
            ("!!"  "bar" "tag:yaml.org,2002:bar")

            ("foo" "bar" "baz")))))
