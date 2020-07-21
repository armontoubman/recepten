(in-package :cl-user)
(defpackage recepten.config
  (:use :cl)
  (:import-from :envy
                :config-env-var
                :defconfig)
  (:export :config
           :*application-root*
           :*static-directory*
           :*template-directory*
           :appenv
           :developmentp
           :productionp))
(in-package :recepten.config)

(setf (config-env-var) "APP_ENV")

(defparameter *application-root*   (asdf:system-source-directory :recepten))
(defparameter *static-directory*   (merge-pathnames #P"static/" *application-root*))
(defparameter *template-directory* (merge-pathnames #P"templates/" *application-root*))

(defconfig :common
  `(:application-root ,(asdf:component-pathname (asdf:find-system :recepten))))

;; start with: APP_ENV=development clackup --address 0.0.0.0 app.lisp
;; or with: echo "export APP_ENV=\"development\"" >> ~/.bashrc
;;          (ql:quickload :recepten) (recepten:start :port 5000)
(defconfig |development|
  `(:debug T
    :databases
    ((:maindb :sqlite3 :database-name ,(merge-pathnames #P"recepten.db"
                                                        *application-root*)))))

(defconfig |production|
  `(:databases
    ((:maindb :sqlite3 :database-name ,(merge-pathnames #P"recepten.db"
                                                        *application-root*)))))

(defconfig |test|
  '())

(defun config (&optional key)
  (envy:config #.(package-name *package*) key))

(defun appenv ()
  (uiop:getenv (config-env-var #.(package-name *package*))))

(defun developmentp ()
  (string= (appenv) "development"))

(defun productionp ()
  (string= (appenv) "production"))
