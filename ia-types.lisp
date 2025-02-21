(in-package :ia-browser)

;;; Manage results from a search
(defclass ia-search-results ()
  ((pane-id :accessor pane-id :initarg :pane-id)
   (last-query :accessor last-query :initform nil :initarg :last-query
	       :documentation "Most recently-run query")
   (last-rows :accessor last-rows :initform 0 :initarg :last-rows
	      :documentation "Most recent rows-per-page value")
   (last-page :accessor last-page :initform 0 :initarg :last-page
	      :documentation "Most recent page selection")
   (last-mediatypes :accessor last-mediatypes :initform '() :initarg :last-mediatypes
		    :documentation "Most recent mediatypes")
   (current-results :accessor current-results :initform '() :initarg :current-results
		    :documentation "The most recent set of results")))

;; this is what gets displayed in the item picker list
(defmethod list-item-name ((item ia-search-results))
  (format nil "[~d] ~s" (slot-value item 'pane-id) (slot-value item 'last-query)))

(defmethod next-results ((search ia-search-results))
  (incf (last-page search))
  (fetch-results search))

(defmethod prev-results ((search ia-search-results))
  (if (> (last-page search) 1)
      (progn
	(decf (last-page search))
	(fetch-results search))))

(defmethod fetch-results ((search ia-search-results))
  (setf (current-results search)
	(do-search (last-query search) (last-rows search) (last-page search) (last-mediatypes search))))


;;;
;;; Displays information about a single item
;;;
(defclass ia-item ()
  ((identifier :accessor identifier :initform nil :initarg :identifier
	       :documentation "The unique ID of the item")
   (metadata :accessor metadata :initform nil :initarg :metadata
	     :documentation "Metadata for the item")))


;; This is what gets displayed in the item picker list
(defmethod list-item-name ((item ia-item))
  (identifier item))


(defun desc-field (pane field metadata)
  (with-drawing-options (pane :ink +red2+)
    (format pane "~a: " field))
  (format pane "~a~%" (format-block (get-metadata-field field metadata) 1024 75)))

(defgeneric dump (object pane)
  (:documentation "Render whatever the object is to a particular pane"))

;;;
;;; Dump details about a single item (identifier) given its metadata
;;; to the specified pane.
;;;
(defmethod dump ((item ia-item) pane)
  (let* ((identifier (identifier item))
	 (metadata (metadata item))
	 (text-pdf (get-file-info "Text PDF" metadata))
	 (thumb (thumbnail identifier))
	 (filename (cdr (assoc :name text-pdf)))
	 (files (cdr (assoc :files metadata))))
    (formatting-table (pane)
      (formatting-row (pane)
	(formatting-cell (pane :align-y :center)
	  (draw-pattern* pane thumb 0 0))
	(formatting-cell (pane :align-y :center)
	  (desc-field pane :title metadata)
	  (desc-field pane :date metadata)
	  (desc-field pane :creator metadata)
	  (desc-field pane :collection metadata)
	  (desc-field pane :isbn metadata))))
    (terpri pane)
    (desc-field pane :description metadata)
    (terpri pane)
    (mapc (lambda (file)
	    (let* ((filetype (cdr (assoc :format file)))
		   (filename (cdr (assoc :name file)))
		   (path (format nil "~a/~a" identifier filename)))
	      (with-drawing-options (pane :ink +red2+)
		(format pane "~a: " filetype))
	      (surrounding-output-with-border (pane :shape :rectangle)
		(with-output-as-presentation (pane path 'ia-file)
		  (format pane "~a" path)))
	      (terpri pane)))
	  files)))


;;;
;;; Dump results of a search
;;;
(defmethod dump ((search ia-search-results) pane)
  "Display results of do-search"
  (let* ((results (current-results search))
	 (docs (get-docs-from-results results))
	 (num-found (get-num-found-from-results results))
	 (rows (last-rows search))
	 (page (last-page search)))
    (with-drawing-options (pane :ink +slate-grey+)
      (format pane "Displaying results ~a through ~a of ~a~%" (* rows (- page 1)) (- (* rows page) 1) num-found))
    (formatting-table (pane)
      (dotimes (item-num (list-length docs))
	(let* ((x (nth item-num docs))
	       (thumb (thumbnail (get-ia-identifier x))))
	  (with-output-as-presentation (pane (get-ia-identifier x) 'ia-item)
	    (formatting-row (pane)
	      (formatting-cell (pane :align-y :center)
		(draw-pattern* pane thumb 0 0))
	      (formatting-cell (pane :align-y :center)
		(with-drawing-options (pane :ink +steel-blue+)
		  (format pane "~a (~a)~%" (format-string (get-title x) 140 70) (get-year x)))
		(format pane "~a~%" (format-block (get-description x) 10 75))))))))))

(defun first-n (n list)
  "Returns the first N elements of the LIST."
  (if (> (list-length list) n)
      (butlast list (- (list-length list) n))
      list))

(defun format-block (desc max-lines line-len)
  (let* ((joined ""))
    (if (listp desc)
	(mapcar (lambda (x) (setf joined (format nil "~a~a~%" joined (bobbin:wrap x line-len)))) desc)
	(setf joined (bobbin:wrap desc line-len)))
    (str:unlines (first-n max-lines (str:lines joined)))))

(defun format-string (s max-chars line-len)
  (let* ((shorter (str:shorten max-chars s))
	 (wrapped (bobbin:wrap shorter line-len)))
    wrapped))
