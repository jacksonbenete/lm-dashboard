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
    ;; (define-key map (kbd "g") 'lm-dashboard-refresh)
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

(defun lm-center-line (string)
  "Center line according to next string size.
The formula is to subtract half of string size from half of window size."
  (let* ((max-size (- (window-body-width) 6))
	 (half-size (/ max-size 2)))
    (insert (make-string (- half-size (round (/ (length string) 2.0)))
			 ?\s))))

(defun current-line-length ()
  "Get current line length in current buffer, including when 
operating on hidden buffers."
  (- (line-end-position) (line-beginning-position)))

(defun list-files ()
  "Insert a list of widgets for each recent oppened files."
  (list-widgets recentf-list 10
		'(lambda (el &rest ignore) (find-file-existing el))))

(defun list-bookmarks ()
  "Insert a list of widgets for each bookmark."
  (require 'bookmark)
  (list-widgets (bookmark-all-names) 10
		'(lambda (el &rest ignore) (bookmark-jump el))))

(defun lm-get-items-per-line ()
  "If Window is big enough, list two items per line.
Else list only one item."
  (if (<= (window-width) 62) 1 2))

(defun list-widgets (list-to-use list-size widget-action)
  "Receive a list, how many items per line, how many lines of items, 
and an action to perform on widget as RET or click, then insert 
the list as widgets creating tabs, spaces and breaklines as needed."
  (defun helper (lista counter list-size)
    (let* ((max-size (- (window-width) 6))
	   (half-size (/ max-size 2))
	   (third-size (/ max-size 3))
	   (items-per-line (lm-get-items-per-line))
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
				  (/ item-length 4)))
	     (abbreviate-file-name item)))
	  ;; correct indentation based on last widget size
	  (while (< (current-line-length) half-size)
	    (insert " "))
	  ;; compute rest of list
	  (helper (cdr lista) (- x 1) (- list-size 1))))))
  (insert "\n\t")
  (helper list-to-use (lm-get-items-per-line) list-size))

(defun lm-dashboard-create-header ()
  "Create a header."
  (setq lm-dashboard-header-name "Symbolics Genera 8.3")
  (insert "\n")
  (lm-center-line lm-dashboard-header-name)
  (insert (propertize
	   lm-dashboard-header-name
	   ;; 'face '(:family "LispM")
	   ))
  (insert "\n\n"))

(defun lm-dashboard-create ()
  "Create a read-only buffer and populate with desired widgets 
and information. The buffer operates under specific keymap."
  (with-current-buffer
      lm-dashboard-buffer-name
    ;; Create dashboard
    (lm-dashboard-create-header)
    (list-files)
    (insert "\n\n")
    (list-bookmarks)
    ;; Define buffer properties
    (lm-dashboard-mode))) 

;;; TODO: for some reason it works when starting,
;;; but (list-bookmarks) doesn't work on second time
(defun lm-dashboard-refresh ()
  (interactive)
  (when (get-buffer lm-dashboard-buffer-name)
    (kill-buffer lm-dashboard-buffer-name))
  (switch-to-buffer lm-dashboard-buffer-name)
  (goto-char (point-min))
  (redisplay)
  (run-hooks 'lm-dashboard-after-initialize-hook)
  (lm-dashboard-create)
  )

;;;###autoload
(defun lm-dashboard-startup-hook ()
  "Initialize dashboard."
  ;; If there is no file being loaded (calling from command line)
  (when (< (length command-line-args) 2)
    (add-hook
     ;; 'emacs-startup-hook
     'window-setup-hook
	      '(lambda ()
		 (lm-dashboard-refresh))))
  ;; (add-hook 'window-size-hook
  ;; 	    (if (= (buffer-name) lm-dashboard-buffer-name)
  ;; 		(lm-dashboard-refresh)))
  )

(provide 'lm-dashboard)