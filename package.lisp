;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
(in-package :cl-user)

(defpackage :ia-browser
  (:use :clim :clim-lisp :clim-extensions)
  (:export :do-search
	   :do-texts-search
	   :fetch-thumbnail
	   :fetch-metadata
	   :ia))

(in-package :ia-browser)
