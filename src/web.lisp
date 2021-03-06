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

(defvar *the-alphabet* (loop for char across "abcdefghijklmnopqrstuvwxyz" collect (string char)))

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
  (let* ((the-recipe (get-recipe-by-slug slug)))
        (render #P"recipe-edit.html" `(:recipe ,the-recipe))))

(defroute ("/recipe/edit/:slug" :method :POST) (&key slug _parsed) ; _parsed = alist
  (let* ((current-recipe (get-recipe-by-slug slug))
         (the-recipe (create-recipe-from-form (accesses _parsed "recipe") (getf current-recipe :id)))
         (the-slug (update-recipe the-recipe))
         (redirect-url (concatenate 'string "/recipe/show/" the-slug)))
    (redirect redirect-url)))

(defroute ("/recipe/delete/:slug") (&key slug)
  (let ((the-recipe (get-recipe-by-slug slug)))
    (render #P"recipe-confirm-delete.html" `(:recipe ,the-recipe))))

(defroute ("/recipe/delete/:slug" :method :POST) (&key slug)
  (let ((the-recipe (get-recipe-by-slug slug)))
    (delete-recipe the-recipe)
    (redirect "/")))

(defroute ("/recipe/print/:slug") (&key slug)
  (let ((the-recipe (get-recipe-by-slug slug)))
    (render #P"recipe-print.html" `(:recipe ,the-recipe :no-nav ,t))))

(defroute ("/recipe/search") (&key (|q| ""))
  (let ((recipes (get-recipes-by-search-query |q|)))
    (render #P"recipe-search.html" `(:query ,|q| :recipes ,recipes))))

(defroute ("/recipe/import") ()
  (render #P"recipe-import.html"))

(defroute ("/recipes") ()
  (render #P"recipes-index.html" `(:letters ,*the-alphabet*)))

(defroute ("/recipes/:letter") (&key letter)
  (let* ((recipes (get-recipes-starting-with letter)))
    (render #P"recipes-per-letter.html" `(:recipes ,recipes
                                          :current-letter ,letter
                                          :letters ,*the-alphabet*))))

(defroute ("/tag/:tag") (&key tag)
  (let* ((recipes (get-recipes-by-tag tag)))
    (render #P"recipes-per-tag.html" `(:recipes ,recipes
                                       :tag ,tag))))

(defroute ("/tag/edit/:tag") (&key tag)
  (let* ((the-tag (get-tag-by-tag tag)))
    (render #P"tag-edit.html" `(:tag ,the-tag))))

(defroute ("/tag/rename/:tag" :method :POST) (&key tag _parsed)
  (let ((new-tag (accesses _parsed "new-tag")))
    (rename-tag tag new-tag)
    (redirect (str:concat "/tag/" new-tag))))

(defroute ("/tags") ()
  (render #P"tags-index.html" `(:letters ,*the-alphabet*)))

(defroute ("/tags/:letter") (&key letter)
  (let* ((tags (get-tags-starting-with letter)))
    (render #P"tags-per-letter.html" `(:tags ,tags
                                       :current-letter ,letter
                                       :letters ,*the-alphabet*))))

(defroute ("/ajax/recipe-tags-autocomplete") (&key _parsed)
  (let ((q (accesses _parsed "term")))
    (if (or (not q) (string= q ""))
      (render-json (list))
      (let ((tags (get-all-tags-like q)))
        (render-json (loop for tag in tags collect (accesses tag :tag)))))))

(defroute ("/ajax/recipe-import") (&key _parsed)
  (render-json (create-recipe-from-import (accesses _parsed "importsource") (accesses _parsed "importurl"))))


;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
