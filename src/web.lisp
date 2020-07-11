(in-package :cl-user)
(defpackage recepten.web
  (:use :cl
        :caveman2
        :recepten.config
        :recepten.view
        :recepten.db
        :recepten.logic
        :recepten.recipe-repo
        :datafly
        :sxql
        :alexandria
        :access)
  (:export :*web*))
(in-package :recepten.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; Routing rules

(defroute "/" ()
  (let ((random-recipe (get-random-recipe))
        (recipe-count (get-recipe-count)))
    (render #P"index.html" `(:recipe ,random-recipe 
                             :recipe-count ,recipe-count))))

(defroute "/recipe/show/:slug" (&key slug)
  (let* ((the-recipe (get-recipe-by-slug slug)) ; the-recipe = plist
         (tags (with-connection (db) (retrieve-all (select :tag (from :tags) 
                                                               (inner-join :recipes_tags :on (:= :tags.id :recipes_tags.tag_id))
                                                               (where (:= :recipes_tags.recipe_id (getf the-recipe :id))))))))
        (render #P"recipe-show.html" `(:recipe ,the-recipe
                                       :tags ,tags))))

(defroute ("/recipe/new" :method :GET) ()
  (render #P"recipe-new.html" '()))

(defroute ("/recipe/new" :method :POST) (&key _parsed) ; _parsed = alist
  (let* ((the-recipe (create-recipe-from-form (accesses _parsed "recipe")))
         (the-slug (create-recipe the-recipe))
         (redirect-url (concatenate 'string "/recipe/show/" the-slug)))
    (redirect redirect-url)))

(defroute "/recipe/edit/:slug" (&key slug)
  (let* ((the-recipe (get-recipe-by-slug slug))
         (tags (with-connection (db) (retrieve-all (select :tag (from :tags) 
                                                               (inner-join :recipes_tags :on (:= :tags.id :recipes_tags.tag_id))
                                                               (where (:= :recipes_tags.recipe_id (getf the-recipe :id))))))))
        (render #P"recipe-edit.html" `(:recipe ,the-recipe
                                       :tags ,tags))))

(defroute ("/recipe/edit/:slug" :method :POST) (&key slug _parsed) ; _parsed = alist
  (let* ((current-recipe (get-recipe-by-slug slug))
         (the-recipe (create-recipe-from-form (accesses _parsed "recipe") (getf current-recipe :id)))
         (the-slug (update-recipe the-recipe))
         (redirect-url (concatenate 'string "/recipe/show/" the-slug)))
    (redirect redirect-url)))
    

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
