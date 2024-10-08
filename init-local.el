;;; init-local.el
;;
;; Filename: init-local.el
;; Description: Customize the distribution.
;; Author: Yidong Ren
;; Created: Mon 23 Sep 2024 23:05:26 AEST
;; Version: 0
;; Last-Updated: Mon 23 Sep 2024 23:05:26 AEST

;;; Commentary:
;;
;;   ** Backup your current ~/.emacs.d
;;
;;   ** Start Emacs and exit, to generate ~/.emacs.d directory.
;;      - https://github.com/jkitchin/scimax
;;
;;   ** Install one of your favourite distributions
;;
;;   ** Put this in your ~/.emacs.d/init.el file:
;;     (load  "~/DotConfigurations/init-local.el")
;;
;;; Change Log:
;;
;; 2024/09/23
;;     Created.
;;
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init-local)

;; * Editing
(let ((enabled-lazycat-auto-save t))
  (if enabled-lazycat-auto-save
      (progn
	(if (not (package-installed-p 'auto-save))
	    (package-vc-install "https://github.com/manateelazycat/auto-save.git"))
	;; manually update in ~/.emacs.d/elpa/auto-save
	(use-package auto-save
	  :custom
	  (auto-save-silent t)
	  (auto-save-delete-trailing-whitespace t)
	  :config (auto-save-enable)))
    (progn
      (setq backup-directory-alist `(("." . "~/.saves")))
      (setq backup-by-copying t)

      ;; from http://xahlee.info/emacs/emacs/emacs_auto_save.html
      (when (>= emacs-major-version 26)
	;; real auto save
	(auto-save-visited-mode 1)
	(setq auto-save-visited-interval 60))

      (defun xah-save-all-unsaved ()
	"Save all unsaved files. no ask.
Version 2019-11-05"
	(interactive)
	(save-some-buffers t))

      (if (version< emacs-version "27.1")
	  (add-hook 'focus-out-hook 'xah-save-all-unsaved)
	(setq after-focus-change-function 'xah-save-all-unsaved)))))

(use-package vundo)

(use-package autorevert
  :init (setq auto-revert-interval 1)
  :config (global-auto-revert-mode))

(use-package recentf
  :hook (after-init . recentf-mode)
  :bind ("C-x C-r" . recentf-open-files)
  :config
  (setq recentf-max-saved-items 300)
  (setq recentf-exclude '("\\.?cache"
			  ".cask"
			  "url"
			  "COMMIT_EDITMSG\\'"
			  "bookmarks"
			  "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
			  "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
			  (lambda (file) (file-in-directory-p file package-user-dir))))
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (recentf-mode 1))

(use-package magit
  :bind ("C-x g" . magit-status)
  :custom
  ;; https://github.com/magit/magit/issues/1953
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

;; select window using shift and arrow keys, like
;; S-<right> runs the command windmove-right (found in
;; windmove-mode-map
;; https://www.emacswiki.org/emacs/WindMove
(use-package windmove
  :init (add-hook 'after-init-hook #'windmove-default-keybindings))

;; select window, https://github.com/abo-abo/ace-window
;; - select windows by characters
;; - swap window by C-u ace-window
(use-package ace-window
  :bind (("M-o" . ace-window)
	 ([remap other-window] . ace-window))
  :init (with-eval-after-load 'lispy
	  ;; remove lispy-left-mayb
	  (define-key lispy-mode-map (kbd "M-o") nil)))

;; a global minor mode allows you to undo and redo changes in
;; [[WindowConfiguration][https://www.emacswiki.org/emacs/WindowConfiguration]]
;; a window configurations is the layer of the various windows in an
;; Emacs frame. Changes can be undo with winner-undo, C-c <left>
;; redo C-c <right>
(use-package winner
  :commands (winner-undo winner-redo)
  :hook (after-init . winner-mode)
  :custom
  (winner-dont-bind-my-keys nil)
  (winner-boring-buffers '("*Completions*"
                           "*Compile-Log*"
                           "*inferior-lisp*"
                           "*Fuzzy Completions*"
                           "*Apropos*"
                           "*Help*"
                           "*cvs*"
                           "*Buffer List*"
                           "*Ibuffer*"
                           "*esh command on file*")))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package browse-kill-ring
  :bind ("C-y" . browse-kill-ring)
  :config (with-eval-after-load 'lispy
	    (define-key lispy-mode-map (kbd "C-y") nil)))

;; * Appearances
(setq require-final-newline t)

(display-time-mode)

;; disable the menu bar and the scroll bar
(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (menu-bar-mode -1)
      (scroll-bar-mode -1)))

(use-package beacon
  :config (beacon-mode 1))

(use-package dimmer
  :config (setq dimmer-fraction 0.1)
  (dimmer-configure-which-key)
  (dimmer-mode t))

(use-package hl-todo
  :init
  (add-hook 'after-init-hook #'global-hl-todo-mode)
  (setq hl-todo-keyword-faces
	'(("FIXME" . (face-foreground 'error))
	  ("ISSUE" . (face-foreground 'error))
	  ("BUG" .( (face-foreground 'error)))
	  ("DEBUG" . (face-foreground 'warning))
	  ("TODO" . (face-foreground 'warning))
	  ("TRICK" . (face-foreground 'warning))
	  ("HACK" . (face-foreground 'warning)))))

(use-package highlight-thing
  :config
  (setq highlight-thing-delay-seconds .3
	highlight-thing-exclude-thing-under-point t)
  (global-highlight-thing-mode))

(use-package highlight-quoted
  :config (add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode))

(use-package symbol-overlay
  :config (with-eval-after-load 'lispy
	    (define-key lispy-mode-map (kbd "M-i") nil)
	    (global-set-key (kbd "M-i") 'symbol-overlay-put)
	    (global-set-key (kbd "M-n") 'symbol-overlay-jump-next)
	    (global-set-key (kbd "M-p") 'symbol-overlay-jump-last)
	    (global-set-key (kbd "<f7>") 'symbol-overlay-mode)
	    (global-set-key (kbd "<f8>") 'symbol-overlay-remove-all)))

(use-package pangu-spacing
  :config (setq pangu-spacing-real-insert-separtor t)
  (global-pangu-spacing-mode 1))

(use-package keycast
  :config
  (keycast-header-line-mode -1)
  (keycast-log-mode -1))

(use-package nyan-mode
  :config (nyan-mode 1))

;; * Tools
;; (use-package eldoc-box
;;   :init (add-hook 'eldoc-mode-hook #'eldoc-box-hover-mode))

(use-package projectile
  :bind ("s-p". projectile-command-map)
  :config (projectile-mode +1))

(use-package avy
  :config (global-set-key (kbd "C-;") 'avy-goto-char-timer))

(defconst my/org-file-directory "~/Dropbox/org-roam")

(use-package org
  :ensure t
  :custom
  (org-agenda-files (list my/org-file-directory))

  (progn
    (if (not (package-installed-p 'org-clock-budget))
	(package-vc-install "https://github.com/Fuco1/org-clock-budget.git"))
    (use-package org-clock-budget)))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename my/org-file-directory))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (setq org-roam-capture-templates
	(append org-roam-capture-templates
		'(("w" "Word" plain
		   "%?"
		   :if-new (file+head "%<%Y%m%d%H%M%S>-Words-${slug}.org"
				      "#+title: ${title}\n#+filetags: :english:vocabulary:\n\n* TODO Collect\n\n** ${slug}\n")
		   :unnarrowed t))))

  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol)

  (use-package deft
    :after org
    :bind
    ("C-c n d" . deft)
    :custom
    (deft-recursive t)
    (deft-use-filter-string-for-filename t)
    (deft-default-extension "org")
    (deft-directory org-roam-directory)))

(use-package rg
  :ensure-system-package rg)

;; treemacs

(use-package which-key
  :diminish (which-key-mode)
  :config (add-hook 'after-init-hook #'which-key-mode)
  (setq which-key-idle-delay 0.5
	which-key-add-column-padding 0))

(use-package keyfreq
  :config (setq keyfreq-excluded-commands '(mwheel-scroll))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package yaml-mode)

(use-package company
  :config (add-hook 'after-init-hook 'global-company-mode))

(use-package helpful
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command)
  (global-set-key (kbd "C-h p") #'helpful-at-point)

  (use-package elisp-demos
    :config (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-local.el ends here
