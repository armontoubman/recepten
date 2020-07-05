(in-package :cl-user)
(defpackage recepten.recipe-repo
  (:use :cl
        :caveman2
        :recepten.config
        :recepten.db
        :datafly
        :sxql
        :alexandria
        :ppcre)
  (:export :get-random-recipe
           :get-recipe-count
           :get-recipe-by-id
           :get-recipe-by-slug
           :get-all-recipes))
(in-package :recepten.recipe-repo)


(defun get-random-recipe ()
  (with-connection (db) (retrieve-one (select :* (from :recipes) (order-by '(:raw "random()")) (limit 1)))))

(defun get-recipe-count ()
  (with-connection (db) (retrieve-one (select (fields (:as (:count :*) :num)) (from :recipes)))))

(defun get-all-recipes ()
  (with-connection (db) (retrieve-all (select :* (from :recipes)))))

(defun get-recipe-by-id (id)
  (with-connection (db) (retrieve-one (select :* (from :recipes) (where (:= :id id))))))

(defun get-recipe-by-slug (slug)
  (with-connection (db) (retrieve-one (select :* (from :recipes) (where (:= :slug slug))))))
