(in-package :cl-user)
(defpackage recepten.view
  (:use :cl :3bmd)
  (:import-from :recepten.config
                :*template-directory*)
  (:import-from :caveman2
                :*response*
                :response-headers)
  (:import-from :djula
                :add-template-directory
                :compile-template*
                :render-template*
                :*djula-execute-package*
                :def-filter)
  (:import-from :datafly
                :encode-json)
  (:export :render
           :render-json))
(in-package :recepten.view)

(djula:add-template-directory *template-directory*)

(defparameter *template-registry* (make-hash-table :test 'equal))

(def-filter :markdown (val)
  (let ((output (with-output-to-string (stream)
                  (3bmd:parse-string-and-print-to-stream val stream))))
      output))

(def-filter :total-minutes-to-only-hours (val)
  (if val
      (multiple-value-bind (quotient remainder) (truncate val 60)
        quotient)
      0))

(def-filter :total-minutes-to-only-minutes (val)
  (if val
      (multiple-value-bind (quotient remainder) (truncate val 60)
        remainder)
      0))


(defun render (template-path &optional env)
  (let ((template (gethash template-path *template-registry*)))
    (unless template
      (setf template (djula:compile-template* (princ-to-string template-path)))
      (setf (gethash template-path *template-registry*) template))
    (apply #'djula:render-template*
           template nil
           env)))

(defun render-json (object)
  (setf (getf (response-headers *response*) :content-type) "application/json")
  (encode-json object))


;;
;; Execute package definition

(defpackage recepten.djula
  (:use :cl)
  (:import-from :recepten.config
                :config
                :appenv
                :developmentp
                :productionp)
  (:import-from :caveman2
                :url-for))

(setf djula:*djula-execute-package* (find-package :recepten.djula))
