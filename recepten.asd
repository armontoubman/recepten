(defsystem "recepten"
  :version "0.1.0"
  :author "Armon Toubman"
  :license ""
  :depends-on ("clack"
               "lack"
               "caveman2"
               "envy"
               "cl-ppcre"
               "uiop"

               ;; for @route annotation
               "cl-syntax-annot"

               ;; HTML Template
               "djula"

               ;; for DB
               "datafly"
               "sxql"

               ;; string manipulation
               "str"

               ;; utilities
               "alexandria"
               "access"
               
               ;; for markdown formatting
               "3bmd"
               
               ;; web scraping
               "dexador"
               "lquery"
               
               ;; unit tests
               "rove")
               
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("config" "view" "db"))
                 (:file "web" :depends-on ("view" "logic"))
                 (:file "view" :depends-on ("config"))
                 (:file "db" :depends-on ("config"))
                 (:file "config")
                 (:file "recipe-repo" :depends-on ("config" "db"))
                 (:file "logic" :depends-on ("recipe-repo" "importers"))
                 (:file "importers"))))

  :description ""
  :in-order-to ((test-op (test-op "recepten-test"))))
