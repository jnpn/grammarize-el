;;; grammarize.el --- Summary --- -*- lexical-binding: t; coding: utf-8 -*-
;;; 
;;; Commentary:
;;; 
;;;  Tree -> Grammar
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
	  (-map (-partial #'-walk nf cf)
		;;(-filter #'consp (@ cf tree))
		(@ cf tree))))

(-walk #'xml-nodefn #'xml-childrenfn *xml*)

(defun xml-descendancy (tree)
  "Parent - Children relationship.
TREE: input tree"
  (list :parent (xml-nodefn tree)
	:children (-uniq (-map #'xml-node-name (xml-childrenfn tree)))))

(-map #'xml-descendancy (-walk #'xml-nodefn #'xml-childrenfn *xml*))

(-walk #'xml-nodefn #'xml-childrenfn *xml*)


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
