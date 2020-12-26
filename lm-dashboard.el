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
;; The purpose is to learn how to work with emacs widgets.
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

;;; The mode-map needs to be set before the define-derived-mode
(defvar lm-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'widget-forward)
    (define-key map (kbd "<backtab>") 'widget-backward)
    (define-key map (kbd "RET") 'widget-button-press)
    (define-key map (kbd "<down-mouse-1>") 'widget-button-click)
    (define-key map (kbd "g") 'lm-dashboard-refresh)
    (define-key map (kbd "r") 'lm-dashboard-recent-files-jump)
    (define-key map (kbd "m") 'lm-dashboard-bookmarks-jump)
    (define-key map (kbd "q") 'quit-window)
    map)
  "Keymap for interacting with dasboard, since it will be 
a read-only buffer.")


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
  "Center line according to next string size.
The formula is to subtract half of string size from half of window size."
  (let* ((max-size (- (window-body-width) 6))
	 (half-size (/ max-size 2)))
    (insert (make-string (- half-size (round (/ (length string) 2.0)))
			 ?\s))))

(defun lm-dashboard-center-string (string)
  "Insert a centered string."
  (lm-dashboard-center-line string)
  (insert string))

(defun current-line-length ()
  "Get current line length in current buffer, including when 
operating on hidden buffers."
  (- (line-end-position) (line-beginning-position)))

(defun lm-dashboard-list-files ()
  "Insert a list of widgets for each recent oppened files."
  (lm-dashboard-list-widgets recentf-list 10
		'(lambda (el &rest ignore) (find-file-existing el))))

(defun lm-dashboard-list-bookmarks ()
  "Insert a list of widgets for each bookmark."
  (require 'bookmark)
  (lm-dashboard-list-widgets (bookmark-all-names) 10
		'(lambda (el &rest ignore) (bookmark-jump el))))

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
  "Receives a list, how many items to parse on the list and 
a widget-action.
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
	       :button-face 'nano-face-default
	       :notify ,widget-notify
	       ;; if filename is too big, correct
	       ,widget-name)))
	(helper (cdr lista)
		(- list-size 1)
		(cons new-widget return-list)))))
  (helper list-to-use list-size '()))


(defun lm-dashboard-insert-enough-spaces ()
  "Insert enough spaces"
  (let* ((max-size (- (window-width) 6))
	 (half-size (/ max-size 2))
	 (third-size (/ max-size 3))
	 (items-per-line (lm-dashboard-get-items-per-line)))
    (while (< (current-line-length) half-size)
      (insert " "))))

;;; Currently not in use
(defun lm-dashboard-insert-widgets (list-to-use)
  "For each widget on list, insert according to how many items 
per line it should insert."
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
  "Receive a list, how many items per line, how many lines of items, 
and an action to perform on widget as RET or click, then insert 
the list as widgets creating tabs, spaces and breaklines as needed."
  (defun helper (lista counter list-size)
    (let* ((max-size (- (window-width) 6))
	   (half-size (/ max-size 2))
	   (third-size (/ max-size 3))
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
	   :button-face 'nano-face-default
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
  (setq lm-dashboard-header-name "Symbolics Genera 8.3")
  (insert "\n")
  (lm-dashboard-center-line lm-dashboard-header-name)
  (insert (propertize
	   lm-dashboard-header-name
	   ;; 'face '(:family "LispM")
	   ))
  (insert "\n\n"))

;;; TODO: image-path should be arg or defcustom
(defun lm-dashboard-create-image-header ()
  "Create a banner on center of screen."
  (let* ((image-path "~/.emacs.d/luna/genera-mini.png")
	 (image (create-image image-path))
	 (image-dimensions (image-size image))
         (width (car image-dimensions))
         (height (cdr image-dimensions))
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
	  (propertize "R" 'face 'nano-face-strong) "ecent Files:")
  (setq lm-dashboard-recent-files-linum (line-number-at-pos)))

(defun lm-dashboard-recent-files-jump ()
  (interactive)
  (goto-line lm-dashboard-recent-files-linum)
  (widget-forward 1))

(defun lm-dashboard-bookmarks-header ()
  "Create a header for lm-dashboard-list-bookmarks.
Create a variable that records the line number."
  (insert "\n\t"
	  "Book" (propertize "m" 'face 'nano-face-strong) "arks:")
  (setq lm-dashboard-bookmarks-linum (line-number-at-pos)))

(defun lm-dashboard-bookmarks-jump ()
  (interactive)
  (goto-line lm-dashboard-bookmarks-linum)
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
  ""
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

(defun lm-dashboard-create ()
  "Create a read-only buffer and populate with desired widgets 
and information. The buffer operates under specific keymap."
  (with-current-buffer
      lm-dashboard-buffer-name
    ;; Create dashboard
    (lm-dashboard-create-image-header)
    (lm-dashboard-post-header)
    (lm-dashboard-recent-files-header)
    (lm-dashboard-list-files)
    (insert "\n\n")
    (lm-dashboard-bookmarks-header)
    (lm-dashboard-list-bookmarks)
    ;; Define buffer properties
    (lm-dashboard-mode)
    (goto-line 0)))

(defun lm-dashboard-refresh ()
  "Create or reload lm-dashboard for latest changes on widget lists."
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
  "Refresh lm-dashboard when window size changes."
  ;; That's whas copy from emacs-dashboard (dashboard-resize-on-hook).
  (let ((space-win (get-buffer-window lm-dashboard-buffer-name))
        (frame-win (frame-selected-window)))
    (when (and space-win
               (not (window-minibuffer-p frame-win)))
      (with-selected-window space-win
        (lm-dashboard-refresh)))))

(provide 'lm-dashboard)