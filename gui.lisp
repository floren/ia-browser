(in-package :ia-browser)

;;;
;;; GUI stuff
;;;

(define-command-table ia-command-table)

;;;
;;; The main window itself
;;;
(define-application-frame ia-app ()
  ((last-query :accessor last-query :initform nil
	       :documentation "Most recently-run query")
   (last-rows :accessor last-rows :initform 0
	      :documentation "Most recent rows-per-page value")
   (last-page :accessor last-page :initform 0
	      :documentation "Most recent page selection")
   (last-mediatypes :accessor last-mediatypes :initform '()
		    :documentation "Most recent mediatypes")
   (current-results :accessor current-results :initform '()
		    :documentation "The most recent set of results"))
  (:command-table (ia-command-table))
  (:panes
   (results (make-clim-stream-pane :type 'results-pane :display-function 'display-results))
   (interactor :interactor
	       :name 'interactor
	       :background +beige+
               :end-of-line-action :wrap*))
  (:layouts
   (default
    (vertically ()
      (7/8 (with-tab-layout ('tab-page :name 'ia-tabs)
	     ("Search Results" results)))
      (1/8 interactor)))))

(defmethod display-results ((frame ia-app) pane)
  (let ((results (slot-value frame 'current-results)))
    (when results
      (updating-output (pane :unique-id results)
	(dump-results pane results)))))

;;;
;;; Displays search results
;;;
(defclass results-pane (application-pane)
  ((last-query :accessor last-query :initform nil
	       :documentation "Most recently-run query")
   (last-rows :accessor last-rows :initform 0
	      :documentation "Most recent rows-per-page value")
   (last-page :accessor last-page :initform 0
	      :documentation "Most recent page selection")
   (last-mediatypes :accessor last-mediatypes :initform '()
		    :documentation "Most recent mediatypes")
   (current-results :accessor current-results :initform '()
		    :documentation "The most recent set of results")))

(defmethod display-item-info ((frame ia-app) pane)
  (let ((identifier (slot-value pane 'identifier))
	(metadata (slot-value pane 'metadata)))
    (when metadata
      (updating-output (pane :unique-id metadata)
	(dump-metadata pane identifier metadata)))))

;;;
;;; Displays information about a single item
;;;
(defclass item-pane (application-pane)
  ((identifier :accessor identifier :initform nil :initarg :identifier
	       :documentation "The unique ID of the item")
   (metadata :accessor metadata :initform nil :initarg :metadata
	     :documentation "Metadata for the item")))


(defun find-or-create-search-tab (query mediatypes)
  (let* ((pages (tab-layout-pages (ia-tab-layout))))))
    
(defun ia-tab-layout ()
  (find-pane-named *application-frame* 'ia-tabs))

(defun console-err (control-string &rest args)
  (let ((console (find-pane-named *application-frame* 'interactor)))
    (with-drawing-options (console :ink +red+)
      (format console control-string args))))

(defun run-query-and-set-state (frame query rows page mediatypes)
  "Runs the query and sets the state on the frame to reflect the query parameters"
  (setf (last-query frame) query)
  (setf (last-rows frame) rows)
  (setf (last-page frame) page)
  (setf (last-mediatypes frame) mediatypes)
  (do-search query rows page mediatypes))

;;; Run a search on all media types
(define-command (com-search :command-table ia-command-table :name "Search")
    ((search-string 'string :prompt "Search string")
     (rows 'integer :prompt "Rows per page")
     (page 'integer :prompt "Page" :default 1))
  (progn
    (if (<= page 0)
	(setf page 1))
    (setf (current-results *application-frame*) (run-query-and-set-state *application-frame* search-string rows page '()))))

;;; Run a search over texts
(define-command (com-text-search :command-table ia-command-table :name "TextSearch")
    ((search-string 'string :prompt "Search string")
     (rows 'integer :prompt "Rows per page" :default 4)
     (page 'integer :prompt "Page" :default 1))
  (setf (current-results *application-frame*)
	(run-query-and-set-state *application-frame* search-string rows page '("texts"))))

;;; Run a search over videos
(define-command (com-vid-search :command-table ia-command-table :name "VidSearch")
    ((search-string 'string :prompt "Search string")
     (rows 'integer :prompt "Rows per page" :default 4)
     (page 'integer :prompt "Page" :default 1))
  (setf (current-results *application-frame*)
	(run-query-and-set-state *application-frame* search-string rows page '("movies"))))

;;; Fetch the next page of results for the last search
(define-command (com-next :command-table ia-command-table :name "Next")
    ()
  (let* ((query (last-query *application-frame*))
	 (rows (last-rows *application-frame*))
	 (page (+ 1 (last-page *application-frame*)))
	 (mediatypes (last-mediatypes *application-frame*)))
    (if query
	(setf (current-results *application-frame*) (run-query-and-set-state *application-frame* query rows page mediatypes))
	(console-err "No previous query! Run Search first"))))

;;; Fetch the previous page of results for the last search
(define-command (com-prev :command-table ia-command-table :name "Prev")
    ()
  (let* ((query (last-query *application-frame*))
	 (rows (last-rows *application-frame*))
	 (page (- (last-page *application-frame*) 1))
	 (mediatypes (last-mediatypes *application-frame*)))
    (if query
	(if (> page 0)
	    (setf (current-results *application-frame*) (run-query-and-set-state *application-frame* query rows page mediatypes))
	    (console-err "Already at beginning of search!"))
	(console-err "No previous query! Run Search first"))))

;;; Select an item, displaying its details.
(define-command (com-select-item :command-table ia-command-table :name "Select")
    ((which-item 'ia-item :prompt "Item"))
  (progn
    (if (not (find-tab-page-named which-item (ia-tab-layout)))
	(let* ((pane (make-clim-stream-pane
						  :type 'item-pane :display-function 'display-item-info
						  :identifier which-item :metadata (fetch-metadata which-item)))
	       (page (make-instance 'tab-page :title which-item
				    :pane pane)))
	  (add-page page (ia-tab-layout) t)
	  ;; I force a redisplay here because it's the only thing I could figure out to make the dang
	  ;; text flow properly instead of being squashed.
	  (redisplay-frame-panes *application-frame* :force-p t)))))

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

;;; close the tab with the given title
(define-command (com-close-tab :command-table ia-command-table :name "CloseTab")
    ((which-tab 'tab-page-name :prompt "Tab Name" :default (tab-page-title (tab-layout-enabled-page (ia-tab-layout)))))
  (remove-page-named which-tab (ia-tab-layout)))
  

(define-command (com-clear :command-table ia-command-table :name "Clear")
    ()
  (window-clear *standard-output*))

(defun desc-field (pane field metadata)
  (with-drawing-options (pane :ink +red2+)
    (format pane "~a: " field))
  (format pane "~a~%" (format-block (get-metadata-field field metadata) 1024 75)))

;;;
;;; Dump details about a single item (identifier) given its metadata
;;; to the specified pane.
;;;
(defun dump-metadata (pane identifier metadata)
  (let* ((text-pdf (get-file-info "Text PDF" metadata))
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


;  (format pane "~s" metadata))

(defun dump-results (pane results)
  "Display results of do-search"
  (let* ((docs (get-docs-from-results results))
	 (num-found (get-num-found-from-results results))
	 (rows (last-rows *application-frame*))
	 (page (last-page *application-frame*)))
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

;;; tab-page-name is the name of a tab page
(define-presentation-type tab-page-name ())

(defun ia ()
  (setf frame (make-application-frame 'ia-app))
  (run-frame-top-level frame))

(setf frame (make-application-frame 'ia-app))
(run-frame-top-level frame)
