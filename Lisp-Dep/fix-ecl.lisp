;;;; Support for the Embeddable Common Lisp.


;; originally derived from fix-scl.lisp

;; ECL defines a function GRAY::REDEFINE-CL-FUNCTIONS, which is used
;; by trivial-gray-streams. The function serves to ensure that some
;; functions for stream operation, such as are defined in ANSI Common
;; Lisp as functions and defined as generic functions in Gray Streams,
;; will be defined in the ECL CL package with, in each, the
;; fdefinition of the corresponding generic function defined in the
;; ECL GRAY package.
;;
;; CL:INTERACTIVE-STREAM-P is not one of those redefined functions.
;;
;; Rather than chaging the symbol's fdefinition, the following form
;; will define a generic function interface onto
;; CL:INTERACTIVE-STREAM-P, such as to allow for definition of
;; methods onto a generic function INTERACTIVE-STREAM-P, in McCLIM

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage #:clim-lisp-compat
    (:use #:cl)
    (:shadow cl:interactive-stream-p)
    (:export #:interactive-stream-p)))

(in-package #:clim-lisp-compat)

(defgeneric interactive-stream-p (stream)
  (:method ((stream stream))
    (cl:interactive-stream-p stream)))




(defpackage :clim-mop
  (:use #:clos #:cl))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (loop for sym being the symbols of :clim-mop
       ;; FIXME: use c2mop (also for CCL)
        do (export sym :clim-mop)))

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (export '(clim-lisp-patch::defconstant
;;             clim-lisp-patch::defclass)
;;           :clim-lisp-patch))

;; (defmacro clim-lisp-patch:defconstant (symbol value &optional docu)
;;   `(defvar ,symbol ,value ,@(and docu (list docu))))

;; (defvar clim-lisp-patch::*compile-time-clos-names* (make-hash-table))

;; (defun clim-lisp-patch::compile-time-clos-class-p (name)
;;   (gethash name clim-lisp-patch::*compile-time-clos-names* nil))

;; (defmacro clim-lisp-patch:defclass (name &rest args)
;;   `(progn
;;      (eval-when (:compile-toplevel)
;;        (setf (gethash ',name clim-lisp-patch::*compile-time-clos-names*) t))
;;      (eval-when (:compile-toplevel :load-toplevel :execute)
;;        (cl:defclass ,name ,@args))))


