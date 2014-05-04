;;;; Support for the Embeddable Common Lisp.

;; originally derived from fix-scl.lisp



(in-package #:clim-lisp)

;; #+ECL
;; (let ((p (find-package '#:gray))
;;       (clp (find-package '#:cl)))

;;   ;; shadow potential name conflicts
;;   ;; for symbols exported from #:GRAY
;;   (do-external-symbols (s p)
;;     (multiple-value-bind (s% s%p)
;;         (find-symbol (symbol-name s) p)
;;       (declare (ignore %s))
;;       (when s%p
;;         (shadowing-import s))))

;;   (use-package p '#:clim-lisp)
;;   ) ;; LET





(defpackage :clim-mop
  (:use :clos))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (loop for sym being the symbols of :clim-mop
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


