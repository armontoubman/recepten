(in-package :cl-user)
(defpackage recepten.recipe-repo
  (:use :cl
        :caveman2
        :recepten.config
        :recepten.db
        :datafly
        :sxql
        :alexandria)
  (:export :get-random-recipe
           :get-recipe-count
           :get-recipe-by-slug
           :get-all-recipes
           :get-recipes-starting-with
           :get-recipes-by-tag
           :get-recipes-by-search-query
           :create-recipe
           :update-recipe
           :delete-recipe
           :get-all-tags-like
           :get-tag-by-tag
           :get-tags-starting-with
           :subsume-tag
           :update-tag))
(in-package :recepten.recipe-repo)

;;;; recipes

(defun get-random-recipe ()
  (with-connection (db) (retrieve-one (select :* (from :recipes) (order-by '(:raw "random()")) (limit 1)))))

(defun get-recipe-count ()
  (with-connection (db) (retrieve-one (select (fields (:as (:count :*) :num)) (from :recipes)))))

(defun get-all-recipes ()
  (with-connection (db) (retrieve-all (select :* (from :recipes)))))

(defun get-recipe-by-slug (slug)
  (with-connection (db) 
    (let ((the-recipe (retrieve-one (select :* (from :recipes) (where (:= :slug slug))))))
         (when the-recipe
          (setf (getf the-recipe 'tags) (get-tags-by-recipe the-recipe))
          the-recipe))))

(defun get-recipes-starting-with (letter)
  (let ((q (str:concat letter "%")))
    (with-connection (db) (retrieve-all (select :* (from :recipes) (where (:like :title q)) (order-by :title))))))

(defun get-recipes-by-tag (tag)
  (let ((the-tag (get-tag-by-tag tag)))
    (if the-tag
        (with-connection (db) (retrieve-all (select :* (from :recipes) (left-join :recipes_tags :on (:= :recipes.id :recipes_tags.recipe_id)) 
                                                                      (where (:= :recipes_tags.tag_id (getf the-tag :id))) 
                                                                      (order-by :recipes.title))))
        (list))))

(defun get-recipes-by-search-query (query)
  (let ((q (str:concat "%" query "%")))
    (with-connection (db) (retrieve-all (select :* (from :recipes) 
                                                   (where (:or (:like :title q)
                                                               (:like :ingredients q)
                                                               (:like :directions q)))
                                                   (order-by :title))))))

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
  (associate-tags-with-recipe (get-recipe-by-slug (getf the-recipe :slug)) (loop for the-tag in (getf the-recipe :tags) collect (create-tag the-tag)))
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
                                                    (where (:= :id (getf the-recipe :id)))))
                              (associate-tags-with-recipe the-recipe (loop for the-tag in (getf the-recipe :tags) collect (create-tag the-tag)))))
  (getf the-recipe :slug))

(defun delete-recipe (the-recipe)
  (with-connection (db) (execute (delete-from :recipes (where (:= :id (getf the-recipe :id)))))
                        (execute (delete-from :recipes_tags (where (:= :recipe_id (getf the-recipe :id)))))))
  

;;;; tags

(defun get-all-tags ()
  (with-connection (db) (retrieve-all (select :* (from :tags)))))

(defun get-all-tags-like (query)
  (let ((q (str:concat "%" query "%")))
    (with-connection (db) (retrieve-all (select :* (from :tags) (where (:like :tag q)) (order-by :tag))))))

(defun get-tags-by-recipe (the-recipe)
  (with-connection (db) (retrieve-all (select (:id :tag) (from :tags) (inner-join :recipes_tags :on (:= :tags.id :recipes_tags.tag_id)) 
                                                                      (where (:= :recipes_tags.recipe_id (getf the-recipe :id)))))))

(defun get-tag-by-tag (tag)
  (with-connection (db) (retrieve-one (select :* (from :tags) (where (:= :tag tag))))))

(defun get-tags-starting-with (letter)
  (let ((q (str:concat letter "%")))
    (with-connection (db) (retrieve-all (select :* (from :tags) (where (:like :tag q)) (order-by :tag))))))

(defun create-tag (the-tag)
   (with-connection (db) 
                    (ignore-errors (execute (insert-into :tags (set= :tag (getf the-tag :tag)))))
                      ;; query was INSERT OR IGNORE, maar is niet te maken met datafly/sxql
                    (get-tag-by-tag (getf the-tag :tag))))

(defun associate-tags-with-recipe (the-recipe the-tags)
  (with-connection (db) (execute (delete-from :recipes_tags (where (:= :recipe_id (getf the-recipe :id)))))
                        (loop for the-tag in the-tags do (execute (insert-into :recipes_tags (set= :recipe_id (getf the-recipe :id)
                                                                                                   :tag_id (getf the-tag :id)))))))
                        
;; TODO: fix inconsistent use of "tag" meaning, string or plist
(defun subsume-tag (old-tag-tag new-tag-tag)
  (with-connection (db)
    (let* ((old-tag (get-tag-by-tag old-tag-tag))
           (new-tag (get-tag-by-tag new-tag-tag)))
      (loop for recipe in (get-recipes-by-tag old-tag-tag)
        do (let* ((recipe-tags (get-tags-by-recipe recipe))
                  (has-old-tag (loop for tag in recipe-tags thereis (same-id-p tag old-tag)))
                  (has-new-tag (loop for tag in recipe-tags thereis (same-id-p tag new-tag))))
             (if (and has-old-tag has-new-tag)
                 ; recipe already has new-tag, so only remove old-tag association
                 (execute (delete-from :recipes_tags (where (:and (:= :tag_id (getf old-tag :id))
                                                                 (:= :recipe_id (getf recipe :id))))))
                 ; recipe does not have new-tag, so update the association
                 (execute (update :recipes_tags (set= :tag_id (getf new-tag :id))
                                     (where (:and (:= :tag_id (getf old-tag :id))
                                                  (:= :recipe_id (getf recipe :id)))))))))
        ; anyway drop the old tag
        (execute (delete-from :tags (where (:= :id (getf old-tag :id))))))))

(defun update-tag (old-tag-tag new-tag-tag)
  (with-connection (db)
    (execute (update :tags (set= :tag new-tag-tag) (where (:= :tag old-tag-tag))))))



;;;; private
  
(defun incr-slug (orig-slug n)
  (let ((new-slug (str:concat orig-slug "-" (write-to-string n))))
       (if (get-recipe-by-slug new-slug)
           (incr-slug orig-slug (+ n 1))
           new-slug)))

(defun same-id-p (a b)
  (= (getf a :id) (getf b :id)))
