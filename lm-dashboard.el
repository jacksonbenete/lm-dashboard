;;; lm-dashboard.el --- Lisp Machine Inspired Dashboard -*- lexical-binding: t -*-

;; Author: Jackson Benete Ferreira <jacksonbenete@gmail.com>
;; URL: https://github.com/jacksonbenete/lm-dashboard
;; Package-Version: xx
;; Package-Commit: xx
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))

;; This file is NOT part of GNU Emacs.

;; License: GPLv3

;;; Commentary:
;; This package aims to create a dashboard like the startup screen of
;; some Lisp Machines such like of Symbolics Genera 8.3.
;; The purpose is to learn how to work with Emacs widgets.
;;
;; This code is inspired by emacs-dashboard.

;;; Code:

;; -------------------------------------------------------------------

(defgroup lm-dashboard nil
  "Lisp Machine dashboard."
  :group 'applications)

(defcustom lm-dashboard-after-initialize-hook nil
  "Hook that is run after dashboard buffer is initialized."
  :group 'lm-dashboard
  :type 'hook)

(defcustom lm-dashboard-items-per-widget 8
  "How many items per widget to be listed."
  :group 'lm-dashboard
  :type 'number)

;; (defcustom lm-dashboard-enabled-softwares '((proced . "p")
;; 					    (dired . "d"))
;;   "Enabled softwares and it's keybinding to be shown as widgets.
;; Must be an association list of type (<symbol> . <string>).")

;;; FIXME: add cider, geiser
(defcustom lm-dashboard-enabled-softwares '((ielm)
					    (slime)
					    (proced)
					    (dired . "~/")
					    (geiser . (geiser-repl--get-impl "Start Geiser for scheme implementation: "))
					    (("cider" . cider-jack-in)))
  "Enabled softwares and it's keybinding to be shown as widgets.
Must be an association list of type (<symbol> . <string>)."
  :group 'lm-dashboard
  :type 'list)

;;; The mode-map needs to be set before the define-derived-mode
;; (defvar lm-dashboard-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "TAB") 'widget-forward)
;;     (define-key map (kbd "<backtab>") 'widget-backward)
;;     (define-key map (kbd "RET") 'widget-button-press)
;;     (define-key map (kbd "<down-mouse-1>") 'widget-button-click)
;;     (define-key map (kbd "g") 'lm-dashboard-refresh)
;;     (define-key map (kbd "r") 'lm-dashboard-recent-files-jump)
;;     (define-key map (kbd "m") 'lm-dashboard-bookmarks-jump)
;;     (define-key map (kbd "q") 'quit-window)
;;     ;; enabled softwares
;;     (mapcar (lambda (el) (let ((fun (car el))
;; 			       (key (cdr el)))
;; 			   (define-key map (kbd key) fun)))
;; 	    lm-dashboard-enabled-softwares)
;;     map)
;;   "Keymap for interacting with dasboard, since it will be
;; a read-only buffer.")

(defvar lm-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Unset digits
    (define-key map (kbd "-") nil)
    (define-key map (kbd "1") nil)
    (define-key map (kbd "2") nil)
    (define-key map (kbd "3") nil)
    (define-key map (kbd "4") nil)
    (define-key map (kbd "5") nil)
    (define-key map (kbd "6") nil)
    (define-key map (kbd "7") nil)
    (define-key map (kbd "8") nil)
    (define-key map (kbd "9") nil)
    (define-key map (kbd "0") nil)
    ;; Widgets basics
    (define-key map (kbd "TAB") 'widget-forward)
    (define-key map (kbd "<backtab>") 'widget-backward)
    (define-key map (kbd "RET") 'widget-button-press)
    (define-key map (kbd "<down-mouse-1>") 'widget-button-click)
    ;; Dashboard functions
    (define-key map (kbd "g") 'lm-dashboard-refresh)
    (define-key map (kbd "r") 'lm-dashboard-recent-files-jump)
    (define-key map (kbd "m") 'lm-dashboard-bookmarks-jump)
    map)
  "Keymap for interacting with dasboard, since it will be a read-only buffer.")

;;; Jump variables
(defvar lm-dashboard-recent-files-position nil
  "Return the line number of recent files header.")

(defvar lm-dashboard-bookmarks-position nil
  "Return the line number of bookmarks header.")

(defvar lm-dashboard-enabled-softwares-position nil
  "Return position of softwares to be shown as buttons.")

(define-derived-mode lm-dashboard-mode special-mode "Lisp Machine Dashboard"
  "Major mode for startup screen.
\\<lm-dashboard-mode-map>"
  :group 'lm-dashboard
  :syntax-table nil
  :abbrev-table nil
  (buffer-disable-undo)
  (whitespace-mode -1)
  (linum-mode -1)
  (when (>= emacs-major-version 26)
    (display-line-numbers-mode -1))
  (page-break-lines-mode 1)
  (setq inhibit-startup-screen t
        buffer-read-only t
        truncate-lines t))

(defconst lm-dashboard-buffer-name "*lm-dashboard*"
  "Define the name of buffer being called.")

(defun lm-dashboard-center-line (string)
  "Center line according to next STRING size.
The formula is to subtract half of STRING size from half of window size."
  (let* ((max-size (- (window-body-width) 6))
	 (half-size (/ max-size 2)))
    (insert (make-string (- half-size (round (/ (length string) 2.0)))
			 ?\s))))

(defun lm-dashboard-last-quarter-line ()
  "Insert spaces until a quarter of line."
  (let* ((max-size (- (window-body-width) 6))
	 (quarter-size (/ max-size 4)))
    (while (< (current-line-length) (- max-size quarter-size))
      (insert " "))))

(defun lm-dashboard-center-string (string)
  "Insert a centered STRING."
  (lm-dashboard-center-line string)
  (insert string))

(defun current-line-length ()
  "Get current line length in current buffer.
Including when operating on hidden buffers."
  (- (line-end-position) (line-beginning-position)))

(defun lm-dashboard-list-files ()
  "Insert a list of widgets for each recent oppened files."
  (lm-dashboard-list-widgets recentf-list lm-dashboard-items-per-widget
		'(lambda (el &rest ignore) (find-file-existing el))))

(defun lm-dashboard-list-bookmarks ()
  "Insert a list of widgets for each bookmark."
  (require 'bookmark)
  (lm-dashboard-list-widgets (bookmark-all-names) lm-dashboard-items-per-widget
			     '(lambda (el &rest ignore) (bookmark-jump el)))
  (insert "\n\n\t")
  ;; (widget-create
  ;;  'link
  ;;    :help-echo "List All Bookmarks"
  ;;    :button-face 'default
  ;;    :notify (lambda () (funcall 'bookmark-bmenu-list))
  ;;    "List Bookmarks")
  (insert "List All Bookmarks: C-x r l"))

(defun lm-dashboard-get-items-per-line ()
  "If Window is big enough, list two items per line.
Else list only one item."
  (if (<= (window-width) 80) 1 2))

;;; Currently not in use. It's useful to keep it if the
;;; lm-dashboard-list-widgets broke again.
;;; The problem was not in lm-dashboard-list-widgets but on a bad naming,
;;; list-bookmarks would work in the first time but then
;;; calling it again would call list-bookmark on bookmark.el.
;;; That's the importance of using <mypackage>-<function-name>.
;;; You will never know if a function-name already exists.
(defun lm-dashboard-create-widgets-list (list-to-use list-size widget-action)
  "Receives a list LIST-TO-USE.
How many items to parse on the list LIST-SIZE.
A function/action WIDGET-ACTION.
Return a list of widgets."
  (defun helper (lista list-size return-list)
    (if (or (null lista) (= 0 list-size))
	return-list
      (let* ((half-size (/ (- (window-width) 6) 2))
	     (item (car lista))
	     (item-length (length item))
	     (item-name (abbreviate-file-name item))
	     (widget-notify `(lambda (&rest ignore) (funcall ,widget-action ,item)))
	     (widget-name (if (> item-length half-size)
		   (concat "..."
			   (substring ,item-name
				      (/ item-length 4)))
		 item-name))
	     (new-widget
	      `('link
	       :help-echo (abbreviate-file-name ,item)
	       :button-face 'default
	       :notify ,widget-notify
	       ;; if filename is too big, correct
	       ,widget-name)))
	(helper (cdr lista)
		(- list-size 1)
		(cons new-widget return-list)))))
  (helper list-to-use list-size '()))


(defun lm-dashboard-insert-enough-spaces ()
  "Insert enough spaces."
  (let* ((max-size (- (window-width) 6))
	 (half-size (/ max-size 2)))
    (while (< (current-line-length) half-size)
      (insert " "))))

;;; Currently not in use
(defun lm-dashboard-insert-widgets (list-to-use)
  "For each widget on list LIST-TO-USE insert an item.
Insert according to how many items per line it should insert."
  (defun helper (lista counter)
    (if (null lista)
	lista
      (let* ((item (car lista))
	     ;; For each widget, apply widget-create passing
	     ;; (cdar item) which is 'link,
	     ;; (cdr item) which is the rest of the widget.
	     (widget (apply 'widget-create (cdar item) (cdr item)))
	     (items-per-line (lm-dashboard-get-items-per-line))
	     (x (if (< counter 1) items-per-line counter)))
	(when (< counter 1) (insert "\n\t"))
	widget
	(lm-dashboard-insert-enough-spaces)
	(helper (cdr lista) (- x 1)))))
  (insert "\n\t")
  (helper list-to-use (lm-dashboard-get-items-per-line)))

(defun lm-dashboard-list-widgets (list-to-use list-size widget-action)
  "Receive a list LIST-TO-USE.
How many items per line LIST-SIZE.
An action to perform on widget as RET or click WIDGET-ACTION.
Then insert the list as widgets creating tabs, spaces and breaklines as needed."
  (defun helper (lista counter list-size)
    (let* ((max-size (- (window-width) 6))
	   (half-size (/ max-size 2))
	   (items-per-line (lm-dashboard-get-items-per-line))
	   (x (if (< counter 1) items-per-line counter))
	   (item (car lista))
	   (item-length (length item)))
      (if (or (null lista) (= 0 list-size))
	  lista
	(progn
	  ;; Insert \nt if reached items-per-line
	  (if (< counter 1)
	      (insert "\n\t"))
	  ;; Create a link with the name of the file
	  (widget-create
	   'link
	   :help-echo (abbreviate-file-name item)
	   :button-face 'default
	   :notify `(lambda (&rest ignore) (funcall ,widget-action ,item))
	   ;; if filename is too big, correct
	   (if (> item-length half-size)
	       (concat "..."
		       (substring (abbreviate-file-name item)
				  (- (/ item-length 2))))
	     (abbreviate-file-name item)))
	  ;; correct indentation based on last widget size
	  (lm-dashboard-insert-enough-spaces)
	  ;; compute rest of list
	  (helper (cdr lista) (- x 1) (- list-size 1))))))
  (insert "\n\t")
  (helper list-to-use (lm-dashboard-get-items-per-line) list-size))

(defun lm-dashboard-create-header ()
  "Create a header."
  (let ((lm-dashboard-header-name "Symbolics Genera 8.3"))
    (insert "\n")
    (lm-dashboard-center-line lm-dashboard-header-name)
    (insert (propertize
	     lm-dashboard-header-name
	     ;; 'face '(:family "LispM")
	     ))
    (insert "\n\n")))

;;; TODO: image-path should be arg or defcustom
(defun lm-dashboard-create-image-header ()
  "Create a banner on center of screen."
  (let* ((image-path "~/.emacs.d/lm-dashboard/genera-mini.png")
	 (image (create-image image-path))
	 (image-dimensions (image-size image))
         (width (car image-dimensions))
         ;; (height (cdr image-dimensions))
	 (max-size (- (window-body-width) 6))
	 (half-size (/ max-size 2)))
    (insert "\n")
    (insert (make-string (- half-size (round (/ width 2.0)))
			 ?\s))
    (insert-image image)))

(defun lm-dashboard-recent-files-header ()
  "Create a header for lm-dashboard-list-files.
Create a variable that records the line number."
  (insert "\n\t"
	  (propertize "R" 'face 'bold) "ecent Files:")
  (setq lm-dashboard-recent-files-position (line-number-at-pos)))

(defun lm-dashboard-recent-files-jump ()
  "Jump to first recent file widget."
  (interactive)
  (goto-char (point-min))
  (forward-line lm-dashboard-recent-files-position)
  (widget-forward 1))

(defun lm-dashboard-bookmarks-header ()
  "Create a header for lm-dashboard-list-bookmarks.
Create a variable that records the line number."
  (require 'bookmark)
  (insert "\n\t"
	  "Book" (propertize "m" 'face 'bold) "arks: "
	  "(" (int-to-string lm-dashboard-items-per-widget)
	  "/" (int-to-string (length (bookmark-all-names))) ")")
  (setq lm-dashboard-bookmarks-position (line-number-at-pos)))

(defun lm-dashboard-bookmarks-jump ()
  "Jump to first bookmark widget."
  (interactive)
  (goto-char (point-min))
  (forward-line lm-dashboard-bookmarks-position)
  (widget-forward 1))

(defun lm-dashboard-init-info ()
    "This function was extracted from emacs-dashboard.
Print the init-info."
  (let ((package-count 0) (time (emacs-init-time)))
    (when (bound-and-true-p package-alist)
      (setq package-count (length package-activated-list)))
    (when (boundp 'straight--profile-cache)
      (setq package-count (+ (hash-table-size straight--profile-cache) package-count)))
    (if (zerop package-count)
        (format "Startup time: %s" time)
      (format "Using %d packages\tLoaded in: %s" package-count time))))

(defun lm-dashboard-post-header ()
  "Print useful information after header."
  (insert "\n\n")
  (let ((login (with-temp-buffer
		  (progn
		    (insert "You're logged in as "
			    (user-login-name) "@"
			    (system-name))
		    (buffer-string))))
	(version (with-temp-buffer
			 (progn
			   (insert "Emacs version " emacs-version)
			   (buffer-string))))
	(process (with-temp-buffer
		   (progn
		     (insert "You're using "
			     invocation-name
			     " under PID "
			     (number-to-string (emacs-pid))
			     " on "
			     (symbol-name system-type))
		     (buffer-string))))
	;; (init-info (with-temp-buffer
	;; 	     (progn
	;; 	       (lm-dashboard-init-info)
	;; 	       (buffer-string))))
	)
    (lm-dashboard-center-string login)
    (insert "\n\n")
    (lm-dashboard-center-string version)
    (insert "\n")
    (lm-dashboard-center-string process)
    (insert "\n")
    (lm-dashboard-center-string (lm-dashboard-init-info)))
  (insert "\n\n"))

(defun lm-dashboard-widget-create (item-name widget-action &optional args)
  "Easier way to define a widget.
String ITEM-NAME.
Action WIDGET-ACTION.
Insert a widget at point.
ARGS will define any argument for WIDGET-ACTION."
  (let ((fun (if args
		 `(funcall ,widget-action ,args)
	       `(funcall ,widget-action))))
    (widget-create
	   'link
	   :help-echo item-name
	   :button-face 'default
	   :notify `(lambda (&rest ignore) ,fun)
	   ;; if filename is too big, correct
	   item-name)))

;; (defun lm-dashboard-useful-apps ()
;;   "Create widgets for useful apps."
;;   (insert "\t")
;;   (lm-dashboard-widget-create "proced" '(lambda (&rest ignore) (proced)))
;;   (insert "\t")
;;   (lm-dashboard-widget-create "dired" '(lambda (&rest ignore) (dired "~/"))))

(defun lm-dashboard-print-line-header ()
  "Print a line full of _ characters."
  (let* ((max-size (- (window-body-width) 6))
	(half-size (/ max-size 2)))
    (insert "\n\t")
    (while (< (current-line-length) (- max-size half-size 8))
      (insert (propertize "_" 'face '(:family "FreeSans" :weight bold))))))

(defun lm-dashboard-useful-apps-header ()
  "Print a header to Applications."
  (lm-dashboard-print-line-header)
  (insert "\n\n\t" "Applications:" "\n\n"))

;;; Fixme remove this comment
;;; (benchmark 1000000 (= (mod 12 4) 0));0.04
;;; (benchmark 1000000 (if (and (evenp 4) (evenp (ash 4 -1))) nil nil));0.07
;;; Just to remember the "smart" dumb way of doing it, mod is easier.
(defun lm-dashboard-useful-apps ()
  "Create widgets for useful apps."
  (defun foreach (lista counter)
    (if (null lista)
	lista
      (let* ((app (if (listp (caar lista)) (cdaar lista) (caar lista)))
	     (app-name (if (listp (caar lista)) (caaar lista) (symbol-name app)))
	     (app-args (cdar lista))
	     (number (int-to-string counter)))
	(insert "\t" (propertize number 'face '(:weight semi-bold)) ": ")
	;; First set keybinding equal to counter
	(define-key lm-dashboard-mode-map (kbd number) `(lambda ()
							  (interactive)
							  (goto-char ,(point))))
	;; Create widget right on point of keybinding
	(lm-dashboard-widget-create
	 app-name
	 `(lambda (&rest ignore) (,app (when ,app-args ,app-args))))

	;; Organize in multiple lines
	;; (if (and (evenp counter) (evenp (ash counter -1))) (insert "\n") nil)
	(if (= (mod counter 4) 0) (insert "\n") nil)

	(foreach (cdr lista) (+ counter 1)))))
  (foreach lm-dashboard-enabled-softwares 1))

(defun lm-dashboard-describe-help ()
  "Describe help functions."
  (let ((help-list '("[C-h e] Go to *Messages* buffer"
		     "[C-h i] List all manuals"
		     "[C-h k] Describe key sequence"
		     "[C-h l] List of last keys pressed"
		     "[C-h m] Describe all current modes"
		     "[C-h f] Describe doc for function"
		     "[C-h v] Describe doc for variable"
		     "[C-h o] Describe doc for symbol")))
    (defun helper (lista counter)
      (let* ((item (car lista))
	     (item-styled (if item (propertize item 'face 'luna-warm-gray) nil))
	     (c (if (< counter 1) 2 counter)))
	(if (null lista)
	    lista
	  (if (= c 1)
	      (progn (insert item-styled "\n")
		     (lm-dashboard-center-line ""))
	    (progn (insert "\t" item-styled)
		   (lm-dashboard-last-quarter-line)))
	  (helper (cdr lista) (- c 1)))))
    (lm-dashboard-center-line "")
    (helper help-list 2)))

(defun lm-dashboard-description ()
  "Print defined text at the right side on screen."
  (let ((help-list '("[g] refresh dashboard"
		     "[q] quit dashboard"
		     "")))
    (defun helper (lista counter)
      (let* ((item (car lista))
	     (item-styled (if item (propertize item 'face 'luna-warm-gray) nil))
	     (c (if (< counter 1) 2 counter)))
	(if (null lista)
	    lista
	  (if (= c 1)
	      (progn (insert item-styled "\n\t")
		     (lm-dashboard-center-line ""))
	    (progn (insert "\t" item-styled)
		   (lm-dashboard-last-quarter-line)))
	  (helper (cdr lista) (- c 1)))))
    (lm-dashboard-center-line "")
    (insert "\t")
    (helper help-list 2)))



(defun lm-dashboard-create ()
  "Create a read-only buffer and populate with desired widgets and information.
The buffer operates under specific keymap."
  (with-current-buffer
      lm-dashboard-buffer-name
    ;; Create dashboard
    (lm-dashboard-create-image-header)
    (lm-dashboard-post-header)
    ;; Recent Files
    (lm-dashboard-recent-files-header)
    (lm-dashboard-list-files)
    (insert "\n\n")
    ;; Bookmarks
    (lm-dashboard-bookmarks-header)
    (lm-dashboard-list-bookmarks)
    (insert "\n\n")
    ;; Super Powers
    (lm-dashboard-useful-apps-header)
    (lm-dashboard-useful-apps)
    ;; (lm-dashboard-print-line-header)
    (insert "\n")
    ;; Help functions
    ;; (lm-dashboard-description)
    ;; (lm-dashboard-describe-help)
    ;; Define buffer properties
    (lm-dashboard-mode)
    (goto-char (point-min))))

(defun lm-dashboard-refresh ()
  "Create or reload lm-dashboard for latest change on widget lists."
  (interactive)
  (when (get-buffer lm-dashboard-buffer-name)
    (kill-buffer lm-dashboard-buffer-name))
  (switch-to-buffer lm-dashboard-buffer-name)
  (goto-char (point-min))
  (redisplay)
  (run-hooks 'lm-dashboard-after-initialize-hook)
  (lm-dashboard-create))

;;;###autoload
(defun lm-dashboard-startup-hook ()
  "Initialize dashboard."
  ;; If there is no file being loaded (calling from command line)
  (when (< (length command-line-args) 2)
    (add-hook
     ;; 'emacs-startup-hook
     'window-setup-hook
	      '(lambda ()
		 (progn
		   (add-hook 'window-size-change-functions 'lm-dashboard-resize)
		   (lm-dashboard-refresh))))))

;;; TODO: why did this work only that way?
(defun lm-dashboard-resize (&optional _)
  "Refresh lm-dashboard when window size change."
  ;; That's whas copy from emacs-dashboard (dashboard-resize-on-hook).
  (let ((space-win (get-buffer-window lm-dashboard-buffer-name))
        (frame-win (frame-selected-window)))
    (when (and space-win
               (not (window-minibuffer-p frame-win)))
      (with-selected-window space-win
        (lm-dashboard-refresh)))))

(provide 'lm-dashboard)

;;; lm-dashboard.el ends here