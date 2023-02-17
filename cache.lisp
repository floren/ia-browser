(in-package :ia-browser)

;;;
;;; Functions that manage a cache of thumbnail images
;;;
(defun get-thumbnail-cache-path (identifier)
  "Given an identifier, return the path for its thumbnail in the cache"
  (merge-pathnames identifier (merge-pathnames "ia/thumbnails/" (uiop:xdg-cache-home))))

(defun read-cached-thumbnail (identifier)
  "Given an identifier, return the cached thumbnail as a CLIM pattern"
  (let ((path (get-thumbnail-cache-path identifier)))
    (ensure-directories-exist path)
    (or (ignore-errors (make-pattern-from-bitmap-file path :format :jpeg))
	(ignore-errors (make-pattern-from-bitmap-file path :format :png)))))

(defun write-cached-thumbnail (identifier value)
  "Given an identifier and a binary value, write a thumbnail into the cache"
  (let ((path (get-thumbnail-cache-path identifier)))
    (ensure-directories-exist path)
    (with-open-file (file path :direction :output
			       :element-type '(unsigned-byte 8)
			       :if-exists :supersede
			       :if-does-not-exist :create)
      (loop
	:for b :across value
	:do (write-byte b file)))))

(defun thumbnail (identifier)
  "Retrieve the thumbnail for the given identifier, reading from cache if possible."
  (let ((cache (get-thumbnail-cache-path identifier)))
    (if (probe-file cache)
	(read-cached-thumbnail identifier)
	(let ((thumbnail (fetch-thumbnail identifier)))
	  (write-cached-thumbnail identifier thumbnail)
	  (read-cached-thumbnail identifier)))))

