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
           :get-all-recipes
           :create-recipe))
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

(defun create-recipe (the-recipe)
  (when (get-recipe-by-slug (getf the-recipe :slug)) (setf (getf the-recipe :slug) (incr-slug (getf the-recipe :slug) 1)))
  (with-connection (db) (execute (insert-into :recipes (set= :title (getf the-recipe :title)
                                                             :ingredients (getf the-recipe :ingredients)
                                                             :servings (getf the-recipe :servings)
                                                             :cooking_time (getf the-recipe :cooking-time)
                                                             :waiting_time (getf the-recipe :waiting-time)
                                                             :directions (getf the-recipe :directions)
                                                             :slug (getf the-recipe :slug)
                                                             :comments (getf the-recipe :comments)))))
  (getf the-recipe :slug))

;;;; private
  
(defun incr-slug (orig-slug n)
  (let ((new-slug (concatenate 'string orig-slug "-" (write-to-string n))))
       (if (get-recipe-by-slug new-slug)
           (incr-slug orig-slug (+ n 1))
           new-slug)))