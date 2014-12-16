;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-

;;;  (c) copyright 2001 by
;;;           Tim Moore (moore@bricoworks.com)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

(in-package :clim-internals)

(defun setf-name-p (name)
  (and (listp name) (eq (car name) 'setf)))


(deftype setf-function-name ()
  '(cons (eql setf) (cons symbol nil)))

(defun make-setf*-gfn-name (function-name)
  (let* ((name-sym (cadr function-name)))
    `(setf ,(intern (format nil ".~A-~A."
                            (symbol-name name-sym)
                            #.(symbol-name '#:star))
                    (symbol-package name-sym)))))

(defmacro function* (fun-name)
  ;; FIXME: Export FUNCTION*
  (unless (setf-name-p fun-name)
    (error "~S is not a valid name for a SETF* generic function."
           fun-name))
  `(function ,(make-setf*-gfn-name fun-name)))

;; (function* (setf clim:stream-cursor-position))

(defmacro fdefinition* (fun-name)
  ;; FIXME: Export FDEFINITION
  "Return the fdefinition for the multiple-value setf form denoted by FUN-NAME

See also: `function*', `defgeneric*', `defmethod*'"
  (let ((%fun-name (gensym "%fun-name-")))
  `(let ((,%fun-name ,fun-name))
     (cond
       ((setf-name-p ,%fun-name)
        (fdefinition (make-setf*-gfn-name ,%fun-name)))
       (t (error "~S is not a valid name for a SETF* generic function."
                 ,%fun-name))))))

;; (fdefinition* '(setf clim:stream-cursor-position))


(defmacro defgeneric* (fun-name lambda-list &body options)
  "Defines a SETF* generic function.  FUN-NAME is a SETF function
name.  The last argument is the single argument to the function in a
SETF place form; the other arguments are values collected from the
SETF new value form.

See also: `defmethod*', `function*', `fdefinition*'
"
  (unless (setf-name-p fun-name)
    (error "~S is not a valid name for a SETF* generic function."
           fun-name))
  (flet ((gensym% (s) (gensym (format nil "~a-" (symbol-name s)))))
    (let* ((setf-name (cadr fun-name))
           (args (butlast lambda-list))
           (place (car (last lambda-list)))
           (gf (make-setf*-gfn-name fun-name))
           (%place-tmp (gensym% place))
           (%values-store (mapcar #'gensym% args)))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (prog1
             (defgeneric ,gf ,lambda-list ,@options)
           (define-setf-expander ,setf-name (,place)
             (values
              (quote (,%place-tmp))
              (list ,place)
              ;; CLHS (CLtL2) does not explicitly specify
              ;; that multiple values may stored by the third return
              ;; value from DEFINE-SETF-EXPANDER. However, CCL, ECL,
              ;; and SBCL are known to implement such multiple value
              ;; storage, with regards to DEFINE-SETF-EXPANDER.
              ;;
              ;; This revised DEFGENERIC* should be tested on other
              ;; Lisp implementations, and accompanied with a
              ;; corresponding regression test procedure.
              (quote (,@%values-store))
              (quote (funcall (function ,gf) ,@%values-store ,%place-tmp))
              (quote (funcall (function ,setf-name) ,%place-tmp))))
           )))))


#|
 (macroexpand-1 '(defgeneric* (setf foo-rectangle-edges*)
                     (x1 y1 x2 y2 rectangle)))

  (defgeneric* (setf foo-rectangle-edges-1*) (x1 y1 x2 y2 rectangle))
  (get-setf-expansion '(setf (foo-rectangle-edges* foo) (values a b c d)))
  (fboundp '(setf foo-rectangle-edges*)) ;; => NIL ;; first run

  ;; Check: #'(SETF FOO) and setf expansion for (SETF FOO) can coexist? (?)
  (defgeneric (setf foo-rectangle-edges*) (x1 y1 x2 y2 rectangle))
  (get-setf-expansion '(setf (foo-rectangle-edges* foo) (values a b c d)))

  (typep (fdefinition '(setf foo-rectangle-edges*)) 'generic-function)

 ;; test funcxtion*, fdefinition*

  (function* (setf foo-rectangle-edges*))
  (fdefinition* '(setf foo-rectangle-edges*))

  (setf (foo-rectangle-edges* 'foo) (values 1 2 3 4))
  ;; ...

|#

(defmacro defmethod* (name &body body)
  "Defines a SETF* method.  NAME is a SETF function name.  Otherwise,
like DEFMETHOD except there must exist a corresponding DEFGENERIC*
form.

See also: `defmethod*', `function*', `fdefinition*'
"
  (unless (setf-name-p name)
    (error "~S is not a valid name for a SETF* generic function."
    name))
  (flet ((extract-gf-lambda (exprs)
           (let ((ll (car exprs)))
             (when (symbolp ll) ;; disregard method qualifiers
               (setq ll (cadr exprs)))
             (mapcar #'(lambda (spec)
                         (typecase spec
                           (cons  (car spec))
                           (symbol spec)
                           (t (error "Lambda list element is not a CONS or a SYMBOL: ~s" spec))))
                     ll))))
    (let ((gf (make-setf*-gfn-name name)))
      `(progn
         (unless (when (fboundp (quote ,gf))
                   (typep (fdefinition (quote ,gf)) 'generic-function))
           (warn 'simple-style-warning
                 :format-control "Implicitly defining multiple-value ~s"
                 :format-arguments (quote (,name)))
           (defgeneric* (setf ,(cadr name)) ,(extract-gf-lambda body)))
         (defmethod ,gf ,@body)))))


#| FIXME: Begin definining a regression test framework for McCLIM,
;; and include this in the same.

;; TEST SETUP FORM:
(defclass triangle ()
  ((a :accessor triangle-a :initform 2)
   (b :accessor triangle-b :initform 2)
   (c :accessor triangle-c :initform (sqrt 8)))
  )


(macroexpand-1 (quote ;; DO NOT INCLUDE IN REGRESSION TEST
(defmethod* (setf tribbq)
    ((a real) (b real) (c real) (triangle triangle))
  (setf (triangle-a triangle) a
        (triangle-b triangle) b
        (triangle-c triangle) c)
  (values a b c))
))


;; TEST SETUP FORM:
(defun get-values (tri)
  (with-slots (a b c) tri
    (values a b c)))

;; TEST:
(get-values (make-instance 'triangle))
;; =EXPECT> 2, 2, #.(sqrt 8)


;; MORE TEST SETUP:
(defmacro do-test ()
  ;; This should serve to test the implementation of DEFGENERIC*,
  ;; implicitly, as well as DEFMETHOD*, consideirng that DEFGENERIC*
  ;; would be called in the macroexapnsion of DEFMETHOD*
  ;; for an undefined "multiple-value setf" form
  (let ((fname (gentemp "MVS-TEST-TRI-")))
    `(progn
       (defmethod* (setf ,fname)
         ((a number) (b number) (c number) (triangle triangle))
         (setf (triangle-a triangle) a
               (triangle-b triangle) b
               (triangle-c triangle) c)
         (values a b c))

       (let ((instance (make-instance 'triangle)))
         #+NIL
         (get-setf-expansion
          '(setf (,fname instance)
            ;; isocolese triangle
            (values 4 4 4)))

         (setf (,fname instance)
               (values 4 4 4))

         (fmakunbound '(setf ,fname))

         (values (triangle-a instance)
                 (triangle-b instance)
                 (triangle-c instance))))))

;; TEST:
(do-test)
;; =expect=> 4, 4, 4

;; TEST REPORT:
;; * "Clozure Common Lisp" "Version 1.10-dev-r15936M-trunk  (LinuxARM32)" armv7l
;;    TEST SUCCESS
;; * "ECL" "13.5.1" armv7l
;;    TEST SUCCESS
;; * "SBC" "1.2.5.76-65a44db"
;;    TEST SUCCESS


;; ALTERNATE TEST:

(defmethod* (setf mv-test-tri-1)
  ((a number) (b number) (c number) (triangle triangle))
  (setf (triangle-a triangle) a
        (triangle-b triangle) b
        (triangle-c triangle) c)
  (values a b c))

(let ((instance (make-instance 'triangle)))
  #+NIL
  (get-setf-expansion
   '(setf (,fname instance)
     ;; isocolese triangle
     (values 4 4 4)))
  
  (setf (mv-test-tri-1 instance)
        (values 4 4 4))
  
  (values (triangle-a instance)
          (triangle-b instance)
          (triangle-c instance)))
;; =expect=> 4, 4, 4

|#
