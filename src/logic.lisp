(in-package :cl-user)
(defpackage recepten.logic
  (:use :cl
        :caveman2
        :recepten.config
        :recepten.db
        :recepten.importers
        :recepten.recipe-repo
        :datafly
        :sxql
        :alexandria
        :ppcre
        :access)
  (:export :create-recipe-from-form
           :create-recipe-from-import
           :rename-tag))
(in-package :recepten.logic)

(defun create-recipe-from-form (the-recipe &optional id) ; the-recipe = alist
  (let ((title (get-string-or-default (accesses the-recipe "title") (str:concat "recept-" (write-to-string (get-universal-time))))))
    ; return plist
    (list :id (or (accesses the-recipe "id") id)
          :title title
          :ingredients (accesses the-recipe "ingredients")
          :servings (parse-integer-or-zero (accesses the-recipe "servings"))
          :cooking-time (combine-time (parse-integer-or-zero (accesses the-recipe "cooking-time-hours"))
                                      (parse-integer-or-zero (accesses the-recipe "cooking-time-minutes")))
          :waiting-time (combine-time (parse-integer-or-zero (accesses the-recipe "waiting-time-hours"))
                                      (parse-integer-or-zero (accesses the-recipe "waiting-time-minutes")))
          :directions (accesses the-recipe "directions")
          :slug (slugify title)
          :comments (accesses the-recipe "comments")
          :tags (loop for tag in (str:words (str:trim (accesses the-recipe "tags"))) collect `(:tag ,tag)))))

(defun create-recipe-from-import (source url)
  (cond ((string= source "allerhande") (import-recipe-from-allerhande url))
        ((string= source "jumbo") (import-recipe-from-jumbo url))))

(defun slugify (the-string)
  (let* ((the-string (str:trim the-string))
         (the-string (ppcre:regex-replace-all "&" the-string "-and-"))
         (the-string (ppcre:regex-replace-all "[\\s\\W-]+" the-string "-")))
    the-string))

(defun rename-tag (old-name new-name)
  (let ((old-tag (get-tag-by-tag old-name))
        (new-tag (get-tag-by-tag new-name)))
    (if old-tag
      ; tag to change actually exists
        (progn
          (if new-tag
              ; requested tag already exists
              (subsume-tag old-name new-name)
              ; request tag does not exist, rename suffices
              (update-tag old-name new-name))
          new-tag)
        nil)))

;;;; private

(defun get-string-or-default (the-string the-default)
  (if (string= "" the-string) the-default the-string))

(defun parse-integer-or-zero (s)
  (or (parse-integer s :start 0 :end nil :radix 10 :junk-allowed t) 0))

(defun combine-time (hours minutes)
  (+ (* hours 60) minutes))

(defun split-time (time)
  (truncate time 60))
