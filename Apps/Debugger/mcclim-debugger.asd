;;; -*- lisp -*-

(defpackage :mcclim-debugger.system
  (:use :cl :asdf))

(in-package :mcclim-debugger.system)

(defsystem :mcclim-debugger
    :depends-on (#:mcclim #:clouseau)
    :serial t
    :components
    ((:file "clim-debugger")))
