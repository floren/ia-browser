(in-package :ia-browser)

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
	      :documentation "Most recent page selection")
   (last-mediatypes :accessor last-mediatypes :initform '()
		    :documentation "Most recent mediatypes"))
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

(defun run-query-and-set-state (frame query rows page mediatypes)
  "Runs the query and sets the state on the frame to reflect the query parameters"
  (setf (last-query frame) query)
  (setf (last-rows frame) rows)
  (setf (last-page frame) page)
  (setf (last-mediatypes frame) mediatypes)
  (do-search query rows page mediatypes))

(define-command (com-search :command-table ia-command-table :name "Search")
    ;; Run a search on all media types
    ((search-string 'string :prompt "Search string")
     (rows 'integer :prompt "Rows per page")
     (page 'integer :prompt "Page" :default 1))
  (progn
    (if (<= page 0)
	(setf page 1))
    (dump-results *standard-output* (run-query-and-set-state *application-frame* search-string rows page '()))))

(define-command (com-text-search :command-table ia-command-table :name "TextSearch")
    ;; Run a search over texts
    ((search-string 'string :prompt "Search string")
     (rows 'integer :prompt "Rows per page" :default 4)
     (page 'integer :prompt "Page" :default 1))
  (dump-results *standard-output*
		(run-query-and-set-state *application-frame* search-string rows page '("texts"))))

(define-command (com-vid-search :command-table ia-command-table :name "VidSearch")
    ;; Run a search over videos
    ((search-string 'string :prompt "Search string")
     (rows 'integer :prompt "Rows per page" :default 4)
     (page 'integer :prompt "Page" :default 1))
  (dump-results *standard-output*
		(run-query-and-set-state *application-frame* search-string rows page '("movies"))))

(define-command (com-next :command-table ia-command-table :name "Next")
    ;; Fetch the next page of results for the last search
    ()
  (let* ((query (last-query *application-frame*))
	 (rows (last-rows *application-frame*))
	 (page (+ 1 (last-page *application-frame*)))
	 (mediatypes (last-mediatypes *application-frame*)))
    (if query
	(dump-results *standard-output* (run-query-and-set-state *application-frame* query rows page mediatypes))
	(with-drawing-options (t :ink +red+)
	  (format t "No previous query! Run Search first")))))

(define-command (com-prev :command-table ia-command-table :name "Prev")
    ;; Fetch the previous page of results for the last search
    ()
  (let* ((query (last-query *application-frame*))
	 (rows (last-rows *application-frame*))
	 (page (- (last-page *application-frame*) 1))
	 (mediatypes (last-mediatypes *application-frame*)))
  (if query
      (if (> page 0)
	  (dump-results *standard-output* (run-query-and-set-state *application-frame* query rows page mediatypes))
	  (with-drawing-options (t :ink +red+)
	    (format t "Already at beginning of search!")))
      (with-drawing-options (t :ink +red+)
	(format t "No previous query! Run Search first")))))

(define-command (com-select-item :command-table ia-command-table :name "Select")
    ((which-item 'ia-item :prompt "Item"))
  (describe-item *standard-output* which-item))

(define-command (com-browser-open :command-table ia-command-table :name "BrowserOpen")
    ((which-item 'ia-item :prompt "Item"))
  (let ((uri (quri:render-uri
	      (quri:make-uri :scheme "https"
			     :host "archive.org"
			     :path (format nil "/details/~a" which-item)))))
    (uiop:launch-program (list "open" uri))))

(define-command (com-open-file :command-table ia-command-table :name "OpenFile")
    ((which-item 'ia-file :prompt "File"))
  (let ((uri (quri:render-uri
	      (quri:make-uri :scheme "https"
			     :host "archive.org"
			     :path (format nil "/download/~a" which-item)))))
    (uiop:launch-program (list "open" uri))))


(define-command (com-clear :command-table ia-command-table :name "Clear" :menu t)
    ()
  (window-clear *standard-output*))

(defun desc-field (pane field metadata)
  (with-drawing-options (t :ink +red2+)
    (format pane "~a: " field))
  (format pane "~a~%" (get-metadata-field field metadata)))

(defun describe-item (pane identifier)
  (let* ((metadata (fetch-metadata identifier))
	 (text-pdf (get-file-info "Text PDF" metadata))
	 (filename (cdr (assoc :name text-pdf))))
    (desc-field pane :title metadata)
    (desc-field pane :date metadata)
    (desc-field pane :creator metadata)
    (desc-field pane :collection metadata)
    (desc-field pane :isbn metadata)
    (desc-field pane :description metadata)
    (if filename
	(let ((path (format nil "~a/~a" identifier filename)))
	  (terpri)
	  (with-drawing-options (t :ink +red2+)
	    (format pane "PDF: "))
	  (surrounding-output-with-border (pane :shape :rectangle)
	    (with-output-as-presentation (t path 'ia-file)
	      (format pane "~a~%" path)))))))


;  (format pane "~s" metadata))

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
