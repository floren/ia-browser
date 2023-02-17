(in-package :ia-browser)

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

(defun get-file-info (format metadata)
  "Get info block for a file of the given format in the metadata object"
  (let ((files (cdr (assoc :files metadata))))
    (find-format-in-file-list format files)))

(defun find-format-in-file-list (format list)
  (if list
      (if (equal format (cdr (assoc :format (car list))))
	  (car list)
	  (find-format-in-file-list format (cdr list)))
      nil))
