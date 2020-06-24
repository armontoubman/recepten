(defsystem "recepten-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Armon Toubman"
  :license ""
  :depends-on ("recepten"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "recepten"))))
  :description "Test system for recepten"
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
