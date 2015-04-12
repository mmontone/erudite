;; @title Erudite output test
;; @author Mariano Montone
;; 
;; @section Introduction
;;
;; This is a test of Erudite output rendering, commands and syntax elements
;;
;; @section Chunks
;; 
;; @include chunk1.lisp
;;
;; @include chunk2.lisp
;;
;; @include chunk4.lisp
;;
;; @include factorial.lisp
;;
;; @section Extracts
;; 
;; @include extract1.lisp
;;
;; @include extract2.lisp
;;
;; @include extract3.lisp
;; 
;; @section Includes
;;
;; @include include1.lisp
;;
;; @section Ignore
;;
;; @ignore
;; This is ignored
(print "This is ignored")
;; @end ignore
;;
;; @section Conditional output
;; @include when.lisp
;;
;; @include if.lisp
;; 
;; @section Erudite syntax
;; @subsection Subsection
;; @subsection Subsubsection
;;
;; @subsection Verbatim
;; @verbatim
;; This is in verbatim
;; @end verbatim
;;
;; @subsection Code
;; @code
;; (defun hello-world ()
;;    (print "Hello world"))
;; @end code
;;
;; @subsection List
;; @list
;; @item First item
;; @item Second item
;; @end list
;;
;; @subsection Emphasis
;; @emph{This is emphasized}
;;
;; @bold{This is in bold}
;;
;; @it{This is in italics}
;;
;; @subsection Inline verbatim
;; This is in @verb{inline verbatim}
;;
;; @subsection Link
;; @link{https://github.com/mmontone/erudite}{Erudite}
;;
;; @subsection Reference
;; @ref{hello-world}
;;
;; @subsection Label and index
;; @label{label-test}
;; @index{label-test}
;; This section is labelled

