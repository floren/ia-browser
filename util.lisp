(in-package :ia-browser)

;;;
;;; Utility functions for dealing with streams and strings and octets
;;;
(defun decode-json-response (octets)
  "Turn raw octets of json data into lisp objects"
  (cl-json:decode-json-from-string (flexi-streams:octets-to-string octets)))

(defun decode-response (octets)
  "Turn octets into a string"
  (flexi-streams:octets-to-string octets))

(defun string-to-stream (s)
  "Turn a string into an octet stream"
  (flexi-streams:make-in-memory-input-stream (flexi-streams:string-to-octets s)))
