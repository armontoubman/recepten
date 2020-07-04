(in-package :cl-user)
(defpackage recepten.web
  (:use :cl
        :caveman2
        :recepten.config
        :recepten.view
        :recepten.db
        :datafly
        :sxql)
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
  (let ((random-recipe (with-connection (db) (retrieve-one (select :* (from :recipes) (order-by '(:raw "random()")) (limit 1)))))
        (recipe-count (with-connection (db) (retrieve-one (select (fields (:as (:count :*) :num)) (from :recipes))))))
    (render #P"index.html" `(:recipe ,random-recipe 
                             :recipe-count ,recipe-count))))

(defroute "/recipe/show/:slug" (&key slug)
  (let* ((the-recipe (with-connection (db) (retrieve-one (select :* (from :recipes) (where (:= :slug slug))))))
         (tags (with-connection (db) (retrieve-all (select :tag (from :tags) 
                                                               (inner-join :recipes_tags :on (:= :tags.id :recipes_tags.tag_id))
                                                               (where (:= :recipes_tags.recipe_id (getf the-recipe :id))))))))
        (render #P"recipe-show.html" `(:recipe ,the-recipe
                                       :tags ,tags))))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
