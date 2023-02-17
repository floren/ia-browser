(ql:quickload "cl-json")
(ql:quickload "drakma")
(ql:quickload "quri")
(ql:quickload "bobbin")
(ql:quickload "str")
(ql:quickload "mcclim")

(defpackage :ia
  (:use :clim :clim-lisp)
  (:export #:do-search))

(in-package :ia)

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

;;;
;;; Functions to search the Internet Archive
;;;
(defun do-texts-search (query rows page)
  "Run a search on texts only"
  (do-search query rows page '("texts")))

(defun do-search (query rows page mediatypes)
  "Run a search and return the results. Mediatypes is an array."
  (let* ((params (list
;		  (cons "q" query)
		  (cons "output" "json")
		  (cons "rows" rows)
		  (cons "page" page)))
	 (q query))
    (loop
      :for type :in mediatypes
      :do (setf q (format nil "~a mediatype:~a" q type)))
    (push (cons "q" q) params)
    (let ((uri (quri:render-uri (quri:make-uri
				 :scheme "https"
				 :host "archive.org"
				 :path "/advancedsearch.php"
				 :query params))))
      (decode-json-response (drakma:http-request uri :preserve-uri t)))))

;;;
;;; Functions to fetch information about individual items on the Internet Archive
;;;
(defun fetch-thumbnail (identifier)
  "Fetch an appropriate thumbnail for the given identifier from archive.org"
  (let ((uri (quri:render-uri
	      (quri:make-uri :scheme "https"
			     :host "archive.org"
			     :path (format nil "/services/img/~a" identifier)))))
    (drakma:http-request uri :preserve-uri t)))

(defun fetch-metadata (identifier)
  "Fetch metadata about a particular identifier from archive.org"
  (let ((uri (quri:render-uri
	      (quri:make-uri :scheme "https"
			     :host "archive.org"
			     :path (format nil "/metadata/~a" identifier)))))
    (decode-json-response (drakma:http-request uri :preserve-uri t))))

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

;;;
;;; Functions for dealing with the results of searches
;;;
(defun get-num-found-from-results (results)
  "Get the number of results found from do-search"
  (cdr (assoc :num-found (cdr (assoc :response results)))))

(defun get-docs-from-results (results)
  "Given results (as returned by do-search), hand back the :DOCS section"
  (cdr (assoc :docs (cdr (assoc :response results)))))

(defun get-title (doc)
  "Given a single document (one element of get-docs-from-results), return the title"
  (cdr (assoc :title doc)))

(defun get-year (doc)
  "Given a single document, return the year"
  (cdr (assoc :year doc)))

(defun get-description (doc)
  "Given a single document, return the description"
  (cdr (assoc :description doc)))

(defun get-ia-identifier (doc)
  "Given a single document, return the identifier"
  (cdr (assoc :identifier doc)))

;;;
;;; Functions for dealing with metadata
;;;
(defun get-metadata-field (field obj)
  "Fetch the named field from the metadata object"
  (let ((meta (cdr (assoc :metadata obj))))
    (cdr (assoc field meta))))

;;;
;;; GUI stuff
;;;

(define-command-table ia-command-table)

(define-application-frame ia-app ()
  ((last-query :accessor last-query :initform nil
	       :documentation "Most recently-run query")
   (last-rows :accessor last-rows :initform 0
	      :documentation "Most recent rows-per-page value")
   (last-page :accessor last-page :initform 0
	      :documentation "Most recent page selection"))
  (:command-table (ia-command-table))
  (:panes
   (interactor :interactor
	       :background +beige+
               :end-of-line-action :wrap*))
  (:layouts
   (default
    (vertically ()
      interactor)))
  (:menu-bar t))

(define-command (com-search :command-table ia-command-table :name "Search")
    ;; Run a search on texts
    ((search-string 'string :prompt "Search string")
     (rows 'integer :prompt "Rows per page")
     (page 'integer :prompt "Page" :default 1))
  (progn
    (if (<= page 0)
	(setf page 1))
    ;; update state
    (setf (last-query *application-frame*) search-string)
    (setf (last-rows *application-frame*) rows)
    (setf (last-page *application-frame*) page)
    (let ((results (do-texts-search search-string rows page)))
      (dump-results *standard-output* results))))

(define-command (com-next :command-table ia-command-table :name "Next")
    ;; Fetch the next page of results for the last search
    ()
  (let* ((query (last-query *application-frame*))
	 (rows (last-rows *application-frame*))
	 (page (+ 1 (last-page *application-frame*))))
    (if query
	(com-search query rows page)
	(with-drawing-options (t :ink +red+)
	  (format t "No previous query! Run Search first")))))

(define-command (com-prev :command-table ia-command-table :name "Prev")
    ;; Fetch the previous page of results for the last search
    ()
  (let* ((query (last-query *application-frame*))
	 (rows (last-rows *application-frame*))
	 (page (- (last-page *application-frame*) 1)))
    (if query
	(if (> page 0)
	    (com-search query rows page)
	    (with-drawing-options (t :ink +red+)
	      (format t "Already at beginning of search!")))
	(with-drawing-options (t :ink +red+)
	  (format t "No previous query! Run Search first")))))

(define-command (com-select-item :command-table ia-command-table :name "Select")
    ((which-item 'ia-item :prompt "Item"))
  (describe-item *standard-output* (fetch-metadata which-item)))

(define-command (com-browser-open :command-table ia-command-table :name "BrowserOpen")
    ((which-item 'ia-item :prompt "Item"))
  (let ((uri (quri:render-uri
	      (quri:make-uri :scheme "https"
			     :host "archive.org"
			     :path (format nil "/details/~a" which-item)))))
    (uiop:launch-program (list "open" uri))))

(define-command (com-clear :command-table ia-command-table :name "Clear" :menu t)
    ()
  (window-clear *standard-output*))

(defun describe-item (pane metadata)
  (format pane "Title: ~a" (get-metadata-field :title metadata)))

(defun dump-results (pane results)
  "Display results of do-search"
  (let* ((docs (get-docs-from-results results))
	 (num-found (get-num-found-from-results results))
	 (rows (last-rows *application-frame*))
	 (page (last-page *application-frame*)))
    (with-drawing-options (t :ink +slate-grey+)
      (format pane "Displaying results ~a through ~a of ~a~%" (* rows (- page 1)) (- (* rows page) 1) num-found))
    (formatting-table (pane)
    (dotimes (item-num (list-length docs))
      (let* ((x (nth item-num docs))
	    (thumb (thumbnail (get-ia-identifier x))))
	(with-output-as-presentation (t (get-ia-identifier x) 'ia-item)
	  (formatting-row (pane)
	    (formatting-cell (pane :align-y :center)
	      (draw-pattern* pane thumb 0 0))
	    (formatting-cell (pane :align-y :center)
	      (with-drawing-options (t :ink +steel-blue+)
		(format pane "~a (~a)~%" (format-string (get-title x) 140 70) (get-year x)))
	      (format pane "~a~%" (format-desc (get-description x) 10 75))))))))))

(defun first-n (n list)
  "Returns the first N elements of the LIST."
  (if (> (list-length list) n)
      (butlast list (- (list-length list) n))
      list))

(defun format-desc (desc max-lines line-len)
  (let* ((joined ""))
    (if (listp desc)
	(mapcar (lambda (x) (setf joined (format nil "~a~a~%" joined (bobbin:wrap x line-len)))) desc)
	(setf joined (bobbin:wrap desc line-len)))
    (str:unlines (first-n max-lines (str:lines joined)))))

(defun format-string (s max-chars line-len)
  (let* ((shorter (str:shorten max-chars s))
	 (wrapped (bobbin:wrap shorter line-len)))
    wrapped))

;; ia-item is a "thing" in the internet archive
(define-presentation-type ia-item ())

(define-presentation-to-command-translator browser-open-ia-item
    (ia-item com-browser-open ia-command-table
             :menu t
	     :gesture nil
             :documentation "Open item in browser"
             :pointer-documentation "Open in browser")
    (object)
  (list object))

(define-presentation-to-command-translator select-ia-item
    (ia-item com-select-item ia-command-table
              :menu t
              :gesture :select
              :documentation "Select item"
              :pointer-documentation "Select item")
    (object)
  (list object))


(setf frame (make-application-frame 'ia-app))
(run-frame-top-level frame)
