(in-package :cl-user)
(defpackage :recepten.importers
  (:use :cl
        ; :dexador
        ; :lquery
        :cl-ppcre
        :rove)
  (:export :import-recipe-from-allerhande))
(in-package :recepten.importers)

;;; section: allerhande

;; TODO: better error handling
(defun import-recipe-from-allerhande (url)
  (let* ((html (ignore-errors (dex:get url)))
         (doc (lquery:$ (initialize (or html "<html/>"))))
         (title (allerhande-title doc))
         (tags (allerhande-tags doc))
         (servings (allerhande-servings doc))
         (cooking-time (allerhande-cooking-time doc))
         (waiting-time (allerhande-waiting-time doc))
         (ingredients (allerhande-ingredients doc))
         (directions (allerhande-directions doc)))
         (list :title title 
                :tags tags 
                :servings servings 
                :cooking-time cooking-time 
                :waiting-time waiting-time
                :ingredients ingredients
                :directions directions)))

(defun allerhande-title (doc)
  (let ((title-vec (lquery:$ doc "meta[property='twitter:title']" (attr "content"))))
    (if (> (length title-vec) 0)
        (elt title-vec 0)
        "geen-titel-gevonden")))

(defun allerhande-tags (doc)
  (or (remove-duplicates (trim-string-vector (lquery:$ doc "ul.tags li" (text))) :test #'string=)
      '("geen-tags-gevonden")))

(defun allerhande-servings (doc)
  (let* ((servings-vec (lquery:$ doc "article > section.content > section > ul.short > li:nth-child(2) > span" (text))))
        (if (> (length servings-vec) 0)
            (parse-integer (ppcre:scan-to-strings "(\\d)" (elt servings-vec 0)))
            0)))

;; TODO: see "article > section.content > div.content-wrapper > div.microdata", easier to parse?
(defun allerhande-cooking-time (doc)
  (let* ((raw (lquery:$ doc "article > section.teaser > ul > li > div > section > ul.short > li.cooking-time" (text)))
         (trimmed (trim-string-vector raw)))
    (parse-allerhande-cooking-times trimmed)))

(defun allerhande-waiting-time (doc)
  (let* ((raw (lquery:$ doc "article > section.teaser > ul > li > div > section > ul.short > li.cooking-time" (text)))
         (trimmed (trim-string-vector raw)))
    (parse-allerhande-waiting-times trimmed)))

(defun allerhande-ingredients (doc)
  (let* ((raw (lquery:$ doc "li[itemprop='ingredients']" (text)))
         (trimmed (trim-string-vector raw))
         (md (apply #'str:concat (loop for term in trimmed collect (format nil "* ~a~%" term)))))
    md))

(defun allerhande-directions (doc)
  (let* ((raw (lquery:$ doc "section.preparation ol > li" (text)))
         (trimmed (trim-string-vector raw))
         (md (apply #'str:concat (loop for term in trimmed collect (format nil "* ~a~%" term)))))
    md))

(defun parse-allerhande-cooking-times (strings)
  (loop for s in strings 
        when (or (str:ends-with-p "bereiden" s)) 
        sum (extract-allerhande-time s)))

(defun parse-allerhande-waiting-times (strings)
  (loop for s in strings 
        when (or (str:ends-with-p "oventijd" s) 
                 (str:ends-with-p "wachtten" s))
        sum (extract-allerhande-time s)))

(defun extract-allerhande-time (s)
  (let ((uren (loop for pattern in '("(\\d+) u") 
                    sum (or (ppcre:register-groups-bind ((#'parse-integer m)) (pattern s) m) 0)))
        (minuten (loop for pattern in '("(\\d+) m")
                       sum (or (ppcre:register-groups-bind ((#'parse-integer m)) (pattern s) m) 0))))
       (+ (* 60 uren) minuten)))

;;; section: general helpers

(defun trim-string-vector (string-vec)
  (loop for s across string-vec collect (str:trim s)))

;;; section: unit tests

;; TODO: test more recipes
(deftest import-recipe-from-allerhande-test
  (let ((burger-op-chic (import-recipe-from-allerhande 
                         "https://www.ah.nl/allerhande/recept/R-R640370/burger-op-chic")))
    (testing "title" (ok (string= "Burger-op-chic" (getf burger-op-chic :title))))
    (testing "servings" (ok (= 4 (getf burger-op-chic :servings))))
    (testing "tags" (ok (and (member "borrel" (getf burger-op-chic :tags) :test #'string=)
                             (member "grillen" (getf burger-op-chic :tags) :test #'string=))))
    (testing "cooking-time" (ok (= 40 (getf burger-op-chic :cooking-time))))
    (testing "waiting-time" (ok (= 0 (getf burger-op-chic :waiting-time))))
    (testing "ingredients" (ok (str:containsp "* 3 el olijfolie" (getf burger-op-chic :ingredients))))
    (testing "directions" (ok (str:containsp "* Bestrijk de hamburgers" (getf burger-op-chic :directions))))))

(run-suite *package*) ; run rove tests upon package load