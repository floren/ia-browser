;;; system definition for ia-browser. -*- lisp -*-
(in-package #:cl-user)

(defpackage #:ia-browser-system
    (:use #:cl #:asdf))

(in-package #:ia-browser-system)

(defsystem "ia-browser"
    :depends-on ("cl-json" "drakma" "quri" "bobbin" "str" "mcclim")
    :serial t
    :components ((:file "package")
		 (:file "util")
		 (:file "ia-api")
		 (:file "cache")
		 (:file "ia-types")
		 (:file "gui")))
