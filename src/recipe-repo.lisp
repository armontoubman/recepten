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
           :get-recipes-starting-with
           :create-recipe
           :update-recipe
           :get-all-tags-like))
(in-package :recepten.recipe-repo)

;;;; recipes

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

(defun get-recipes-starting-with (letter)
  (let ((q (concatenate 'string letter "%")))
    (with-connection (db) (retrieve-all (select :* (from :recipes) (where (:like :title q)) (order-by :title))))))

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

(defun update-recipe (the-recipe)
  ;; Erase own slug to make it available for itself;
  (with-connection (db) (execute (update :recipes (set= :slug "") (where (:= :id (getf the-recipe :id))))))
  (let* ((recipe-with-same-slug (get-recipe-by-slug (getf the-recipe :slug)))
         (recipe-id (getf the-recipe :id))
         (other-id (getf recipe-with-same-slug :id)))
        (when recipe-with-same-slug (setf (getf the-recipe :slug) (incr-slug (getf the-recipe :slug) 1)))
        (with-connection (db) (execute (update :recipes (set= :title (getf the-recipe :title)
                                                          :ingredients (getf the-recipe :ingredients)
                                                          :servings (getf the-recipe :servings)
                                                          :cooking_time (getf the-recipe :cooking-time)
                                                          :waiting_time (getf the-recipe :waiting-time)
                                                          :directions (getf the-recipe :directions)
                                                          :slug (getf the-recipe :slug)
                                                          :comments (getf the-recipe :comments))
                                                    (where (:= :id (getf the-recipe :id)))))))
  (getf the-recipe :slug))

;;;; tags

(defun get-all-tags-like (query)
  (let ((q (concatenate 'string "%" query "%")))
    (with-connection (db) (retrieve-all (select :* (from :tags) (where (:like :tag q)) (order-by :tag))))))

;;;; private
  
(defun incr-slug (orig-slug n)
  (let ((new-slug (concatenate 'string orig-slug "-" (write-to-string n))))
       (if (get-recipe-by-slug new-slug)
           (incr-slug orig-slug (+ n 1))
           new-slug)))
