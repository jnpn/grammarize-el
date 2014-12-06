;;; grammarize.el --- Summary --- -*- lexical-binding: t; coding: utf-8 -*-
;;; 
;;; Commentary:
;;; 
;;;  Tree -> Grammar
;;;  Infers grammar through parent -> children relationships.
;;;
;;; Code:

(require 'dash)
(require 'xml)

(defvar *xml* (car (xml-parse-file "~/tree.xml")) "A test tree.")
(defvar *xml* (car (xml-parse-file "~/tree2.xml")) "A test tree.")

(defun -mapfilter (m f l)
  "Simple wrapper.
M : mapping; F: filter; L: input list"
  (-map m (-filter f l)))

(defun -flatmap (f l)
  "Common flatmap.
F : mapping; L: input list"
  (-flatten (-map f l)))

;; (-map #'list (-range 10))
;; (-flatmap #'list (-range 10))

(defun xml-nodefn (xml)
  "Unnecessary ~ADT wrapper.
XML: input xml tree"
  (xml-node-name xml))

(defun xml-childrenfn (xml)
  "Unnecessary ~ADT wrapper.
XML: input xml tree"
  (-filter #'consp (xml-node-children xml)))

(defalias '@ 'funcall)

(defun -walk (nf cf tree)
  "Tree walk, generic.
NF: node mapping; CF: children mapping; TREE: input tree"
  (append (list (@ nf tree))
	  (-flatten-n 1 (-map (-partial #'-walk nf cf)
		 (@ cf tree)))))

(-each (-walk #'xml-nodefn #'xml-childrenfn *xml*) (-partial #'message "%s"))
(-walk #'xml-nodefn #'xml-childrenfn *xml*)

(defun xml-descendancy (tree)
  "Parent - Children relationship.
TREE: input tree"
  (list :parent (xml-nodefn tree)
	:children (-uniq (-map #'xml-node-name (xml-childrenfn tree)))))

(-map #'xml-descendancy (-walk #'identity #'xml-childrenfn *xml*))

;; ((:parent catalog :children (book mook)) (:parent book :children (author title genre price publish_date description)) (:parent author :children nil) (:parent title :children nil) (:parent genre :children nil) (:parent price :children nil) (:parent publish_date :children nil) (:parent description :children nil) (:parent mook :children (author title genre price publish_date description)) (:parent author :children nil) (:parent title :children nil) (:parent genre :children nil) ...)

(-group-by
 ;; (lambda (d0 d1) (eq (nth 1 d0) (nth 1 d1)))
 (lambda (d) (nth 1 d))
 (-map #'xml-descendancy (-walk #'identity #'xml-childrenfn *xml*)))

;; ((catalog (:parent catalog :children (book mook)))
;;  (book (:parent book :children (author title genre price publish_date description)) (:parent book :children (author title genre price publish_date description)))
;;  (author (:parent author :children nil) (:parent author :children nil) (:parent author :children nil) (:parent author :children nil))
;;  (title (:parent title :children nil) (:parent title :children nil) (:parent title :children nil) (:parent title :children nil))
;;  (genre (:parent genre :children nil) (:parent genre :children nil) (:parent genre :children nil) (:parent genre :children nil))
;;  (price (:parent price :children nil) (:parent price :children nil) (:parent price :children nil) (:parent price :children nil))
;;  (publish_date (:parent publish_date :children nil) (:parent publish_date :children nil) (:parent publish_date :children nil) (:parent publish_date :children nil))
;;  (description (:parent description :children nil) (:parent description :children nil) (:parent description :children nil) (:parent description :children nil))
;;  (mook (:parent mook :children (author title genre price publish_date description)) (:parent mook :children (author title genre price publish_date description))) )

(defun -tree-grammar (tree)
  "Generate an abstract grammar from TREE."
 (-map
  (lambda (~d) (list (car ~d) (nth 3 (cadr ~d))))
  (-group-by
   (lambda (d) (nth 1 d))
   (-map #'xml-descendancy (-walk #'identity #'xml-childrenfn tree)))))

(-tree-grammar *xml*)

;; ((catalog (book mook))
;;  (book (author title genre price publish_date description))
;;  (author nil)
;;  (title nil)
;;  (genre nil)
;;  (price nil)
;;  (publish_date nil)
;;  (description nil)
;;  (mook (author title genre price publish_date description)))

;;; BNF:

(mapconcat (lambda (r) (format "<%s> ::= %S" (car r) (cadr r))) (-tree-grammar *xml*) "\n")

(defun -tree-bnf (tree)
  "Generate a BNF grammar from TREE."
  (let* ((grammar (-tree-grammar tree))
	 (terminal (-partial #'format "<%S>"))
	 (right (lambda (terms)
		  (cond ((null terms) (list "<bottom>"))
			(t (-map (-partial #'format "<%s>") terms))))))
    (mapconcat
     (lambda (r) (format "%s ::= %s"
			 (funcall terminal (car r))
			 (-reduce (lambda (a b) (concat a " | " b)) (funcall right (cadr r)))))
     grammar "\n")))

;;; better tree bnf, recursivish and lispier
;;;
;;; case rule of
;;; (parent (c:cs)) -> <parent> ::= <c> | <cs...>
;;; (parent nil)    -> <parent>
;;; 

(message (-tree-bnf *xml*))

(defun -treecount (tree)
  "Count the element of TREE."
  (apply #'+ (-walk (lambda (t) 1) #'xml-childrenfn tree)))

(-treecount *xml*)
;;; -> 29

;;; Various Tests:

(-map (-partial #'xml-nodefn)
      (-filter #'consp
	       (xml-childrenfn *xml*)))

;; (nil book nil book nil book nil book nil book nil book ...)

(xml-node-name *xml*)

(-mapfilter
 (-partial #'xml-node-name)
 (lambda (c) (not (stringp c)))
 (xml-node-children *xml*))

;;; Main:

(provide 'grammarize)
;;; grammarize.el ends here
