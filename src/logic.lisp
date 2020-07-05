(in-package :cl-user)
(defpackage recepten.logic
  (:use :cl
        :caveman2
        :recepten.config
        :recepten.db
        :datafly
        :sxql
        :alexandria
        :ppcre
        :access)
  (:export :create-recipe-from-form))
(in-package :recepten.logic)

(defun create-recipe-from-form (the-recipe) ; the-recipe = alist
  (let ((title (get-string-or-default (accesses the-recipe "title") (concatenate 'string "recept-" (write-to-string (get-universal-time))))))
    ; return plist
    (list :id (accesses the-recipe "id")
          :title title
          :ingredients (accesses the-recipe "ingredients")
          :servings (parse-integer-or-zero (accesses the-recipe "servings"))
          :cooking-time (combine-time (parse-integer-or-zero (accesses the-recipe "cooking-time-hours"))
                                      (parse-integer-or-zero (accesses the-recipe "cooking-time-minutes")))
          :waiting-time (combine-time (parse-integer-or-zero (accesses the-recipe "waiting-time-hours"))
                                      (parse-integer-or-zero (accesses the-recipe "waiting-time-minutes")))
          :directions (accesses the-recipe "directions")
          :slug (slugify title)
          :comments (accesses the-recipe "comments"))))

;;;; private

(defun get-string-or-default (the-string the-default)
  (if (string= "" the-string) the-default the-string))

(defun parse-integer-or-zero (s)
  (or (parse-integer s :start 0 :end nil :radix 10 :junk-allowed t) 0))

(defun combine-time (hours minutes)
  (+ (* hours 60) minutes))

(defun split-time (time)
  (truncate time 60))

(defun slugify (the-string)
  (let* ((the-string (string-trim '(#\Space #\Tab #\Newline) the-string))
         (the-string (ppcre:regex-replace-all "&" the-string "-and-"))
         (the-string (ppcre:regex-replace-all "[\\s\\W-]+" the-string "-")))
    the-string))
   
