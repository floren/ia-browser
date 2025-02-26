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
   (results (make-clim-stream-pane :type 'application-pane :display-time nil :incremental-redisplay t :end-of-page-action :allow))
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

(setf mcclim-truetype::*dpi* 90)
(setf frame (make-application-frame 'ia-app))
(run-frame-top-level frame)
