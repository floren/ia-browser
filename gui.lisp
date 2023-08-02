(in-package :ia-browser)

;;;
;;; GUI stuff
;;;

(define-command-table ia-command-table)

;;;
;;; The main window itself
;;;
(define-application-frame ia-app ()
  ((counter :accessor counter :initform 0)
   (searches :accessor searches :initform '())
   (items :accessor items :initform '()))
  (:command-table (ia-command-table))
  (:panes
   (search-picker :list-pane
		  :name-key #'list-item-name
		  :value-changed-callback 'search-picker-changed
		  :mode :exclusive)
   (item-picker :list-pane
		:name-key #'list-item-name
		:value-changed-callback 'item-picker-changed
		:mode :exclusive)
   (results (make-clim-stream-pane :type 'application-pane :display-time nil))
   (interactor :interactor
	       :name 'interactor
	       :background +beige+
               :end-of-line-action :wrap*))
  (:layouts
   (default
    (horizontally (:width 1600 :height 1200)
      (1/5 (vertically ()
	     (1/2 (labelling (:label "Searches") (scrolling ()  search-picker)))
	     (1/2 (labelling (:label "Items") (scrolling () item-picker)))))
      (4/5 (vertically ()
	      (7/8 (labelling (:label "Display") results))
	      (1/8 interactor)))))))

;; return the currently-selected query
(defun selected-query ()
  (clim:gadget-value (find-pane-named *application-frame* 'search-picker)))

;; return the currently-selected item
(defun selected-item ()
  (clim:gadget-value (find-pane-named *application-frame* 'item-picker)))

;; for displaying the lists
(defgeneric list-item-name (object)
  (:documentation "return a string to represent the object in a list"))


;; Invoked when someone clicks an item in the search list.
(defun search-picker-changed (pane value)
  (declare (ignore pane))
  (update-output value))

;; Invoked when someone clicks in the item list
(defun item-picker-changed (pane value)
  (declare (ignore pane))
  (update-output value))

;; Clears the results window and shows the current search results.
(defun update-output (value)
  (let* ((output (get-frame-pane *application-frame* 'results)))
    (when value
      (window-clear output)
      (dump value output))))

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

;; dump some info about an item
(defmethod display-item-info ((frame ia-app) pane)
  (let ((identifier (slot-value pane 'identifier))
	(metadata (slot-value pane 'metadata)))
    (when metadata
      (updating-output (pane :unique-id metadata)
	(dump-metadata pane identifier metadata)))))

;; This is what gets displayed in the item picker list
(defmethod list-item-name ((item ia-item))
  (identifier item))

(defmethod find-ia-item ((frame ia-app) identifier)
  nil)

;; write an error message to the console (interactor frame)
(defun console-err (control-string &rest args)
  (let ((console (find-pane-named *application-frame* 'interactor)))
    (with-drawing-options (console :ink +red+)
      (format console control-string args))))

(defmethod new-query ((frame ia-app) query rows page mediatypes)
  (if (<= page 0)
      (setf page 1))
  (let* ((picker (find-pane-named frame 'search-picker))
	 (item (make-instance
		'ia-search-results
		:pane-id (incf (counter frame))
		:last-query query
		:last-rows rows
		:last-page page
		:last-mediatypes mediatypes
		:current-results (do-search query rows page mediatypes))))
    (push item (searches frame))
    (setf (clime:list-pane-items picker) (searches frame))
    (setf (gadget-value picker :invoke-callback t) item)))

;;; Run a search on all media types
(define-command (com-search :command-table ia-command-table :name "Search")
    ((search-string 'string :prompt "Search string")
     (rows 'integer :prompt "Rows per page")
     (page 'integer :prompt "Page" :default 1))
  (new-query *application-frame* search-string rows page '()))

;;; Run a search over texts
(define-command (com-text-search :command-table ia-command-table :name "TextSearch")
    ((search-string 'string :prompt "Search string")
     (rows 'integer :prompt "Rows per page" :default 4)
     (page 'integer :prompt "Page" :default 1))
  (new-query *application-frame* search-string rows page '("texts")))

;;; Run a search over videos
(define-command (com-vid-search :command-table ia-command-table :name "VidSearch")
    ((search-string 'string :prompt "Search string")
     (rows 'integer :prompt "Rows per page" :default 4)
     (page 'integer :prompt "Page" :default 1))
  (new-query *application-frame* search-string rows page '("movies")))

;;; Fetch the next page of results for the last search
(define-command (com-next :command-table ia-command-table :name "Next")
    ()
  (let* ((current (selected-query)))
    (next-results current)
    (update-output current)))

;;; Fetch the previous page of results for the last search
(define-command (com-prev :command-table ia-command-table :name "Prev")
    ()
  (let* ((current (selected-query)))
    (prev-results current)
    (update-output current)))

;;; Select an item, displaying its details.
(define-command (com-select-item :command-table ia-command-table :name "Select")
    ((which-item 'ia-item :prompt "Item"))
  (let* ((picker (find-pane-named *application-frame* 'item-picker))
	 (existing (find-ia-item *application-frame* which-item)))
    (if (not existing)
	(let* ((item (make-instance
		      'ia-item
		      :identifier which-item
		      :metadata (fetch-metadata which-item))))
	  
	  (push item (items *application-frame*))
	  (setf (clime:list-pane-items picker) (items *application-frame*))
	  (setf (gadget-value picker :invoke-callback t) item))
	(setf (gadget-value picker :invoke-callback t) existing))))

;;; Open an item's details page in the browser
(define-command (com-browser-open :command-table ia-command-table :name "OpenDetails")
    ((which-item 'ia-item :prompt "Item"))
  (let ((uri (quri:render-uri
	      (quri:make-uri :scheme "https"
			     :host "archive.org"
			     :path (format nil "/details/~a" which-item)))))
    (uiop:launch-program (list "open" uri))))

;;; Open a particular file in the appropriate tool (possibly the browser)
;;; Doesn't download (yet)
(define-command (com-open-file :command-table ia-command-table :name "OpenFile")
    ((which-item 'ia-file :prompt "File"))
  (let ((uri (quri:render-uri
	      (quri:make-uri :scheme "https"
			     :host "archive.org"
			     :path (format nil "/download/~a" which-item)))))
    (uiop:launch-program (list "open" uri))))

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

;;; ia-item is a "thing" in the internet archive
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

;;; ia-file is a specific file in the internet archive
(define-presentation-type ia-file ())

(define-presentation-to-command-translator open-ia-file
    (ia-file com-open-file ia-command-table
	     :menu t
	     :gesture :select
	     :documentation "Open file"
	     :pointer-documentation "Open file")
    (object)
  (list object))

(defun ia ()
  (setf frame (make-application-frame 'ia-app))
  (run-frame-top-level frame))

(setf frame (make-application-frame 'ia-app))
(run-frame-top-level frame)
