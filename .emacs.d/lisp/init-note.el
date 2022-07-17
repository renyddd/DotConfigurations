;;; init-note --- initialize org mode settings
;;; Commentary:
;; take note by org or markdown

;;; Code:
(require 'init-const)

(straight-use-package '(tomelr
			:type git :host github :repo "kaushalmodi/tomelr"
			))

(straight-use-package 'ox-hugo)

(leaf org
  :straight t
  :config
  ;; https://orgmode.org/manual/Workflow-states.html#Workflow-states
  (progn
	;; variables
	;; https://cpbotha.net/2019/11/02/forming-and-maintaining-habits-using-orgmode/
	(with-eval-after-load 'org
	  (add-to-list 'org-modules 'org-habit t))


	(setq org-todo-keywords
		  ;; '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)"))

		  '(
			(sequence ; https://www.gtrun.org/custom/my-agenda.html#orge94abfc
			 "☞ TODO(t!)"	; A task that needs doing & is ready to do
			 "PROJ(p!)" ; An ongoing project that cannot be completed in one step
			 "⚔ INPROCESS(s!)"			; A task that is in progress
			 "⚑ WAITING(w!)" ; Something is holding up this task; or it is paused
			 "|"
			 "☟ NEXT(n!)"
			 "✰ Important(i!)" 
			 "✔ DONE(d!)"				; Task successfully completed
			 "✘ CANCELED(c@)") ; Task was cancelled, aborted or is no longer applicable
			(sequence
			 "✍ NOTE(N!)"
			 "FIXME(f!)"
			 "☕ BREAK(b!)"
			 "❤ Love(l!)"
			 "REVIEW(r!)"
			 )
			)							; Task was completed
		  )

	(setq org-log-done 'time
		  org-closed-keep-when-no-todo t
     	  org-log-into-drawer t
		  org-treat-insert-todo-heading-as-state-change t
		  org-display-remote-inline-images 'download ; not work so far

		  ;; it's better to see images in the output
		  org-export-with-broken-links t	; adapt org-roam export
		  org-image-actual-width (list 550) ; try to get the width from
										; #+ATTR.* keyword
										; #+ATTR_HTML: :width 300px
		  )
  
	;; some directories
	(setq org-directory "~/org"
		  org-agenda-files (list "~/work/NOTEs.org"
								 "~/work/named_by_week/"
								 "~/tmp/org_first.org")
		  org-default-notes-file (concat org-directory "~/notes.org")
		  )

	;; browse html with creating files in work directory
	(defun org-html-export-to-browser ()
	  (interactive)
	  (let ((cur-window-state (current-window-configuration))
			(org-exported-html-buffer (org-html-export-as-html))
			(org-exported-file (make-temp-file "org-export"
											   nil
											   ".html")))
		(set-buffer org-exported-html-buffer)
		(write-file org-exported-file)
		(kill-buffer org-exported-html-buffer)
		(browse-url (concat "file://" org-exported-file))
		(set-window-configuration cur-window-state)))

  
	;; settings for ox-hugo
	(with-eval-after-load 'ox
	  (require 'ox-hugo))
	(setq org-hugo-base-dir "~/git_repositories/renyddd.github.io/"
		  org-hugo-default-section-directory "posts")

	;; https://github.com/rlister/org-present
	;; start the minor mode with: M-x org-present
	;; left / right for movement
	;; C-c C-q for quit
	(leaf org-present
	  :straight t
	  :config
	  (progn
		(add-hook 'org-present-mode-hook
				  (lambda ()
					(org-present-big)
					(org-display-inline-images)
					(org-present-hide-cursor)
					(org-present-read-only)))
		(add-hook 'org-present-mode-quit-hook
				  (lambda ()
					(org-present-small)
					(org-remove-inline-images)
					(org-present-show-cursor)
					(org-present-read-write))))
	  )
	)
  :bind (("C-c a" . org-agenda)
		 ("C-c h" . org-html-export-to-browser))
  )

;; org-roam
(leaf org-roam
  :straight t
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (setq org-roam-directory (file-truename "~/org-roam/"))
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
				 (display-buffer-in-direction)
				 (direction . right)
				 (window-width . 0.33)
				 (window-height . fit-window-to-buffer)))
  (org-roam-db-autosync-mode)

  (setq org-roam-graph-viewer "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")

  ;; org-roam-ui
  (leaf org-roam-ui
	:straight t
	:bind (("C-c n u" . org-roam-ui-mode))
	:config
	(setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t)
	)

  ;; deft for quickly browsing, filtering of plain text notes
  ;; got from
  ;; https://lucidmanager.org/productivity/taking-notes-with-emacs-org-mode-and-org-roam/
  (leaf deft
	:straight t
	:bind (("C-c n d" . deft))
	:config
	(setq deft-directory org-roam-directory
          deft-recursive t
          deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
          deft-use-filename-as-title t)
	;; If no files match your search string, pressing RET will create
	;; a new file using the string as the title.
	;; C-o to open a file in another window.
	)
  )

;; easy-hugo just a posts manager and reviewer, `easy-hugo` to get menu
;; could not convert org files.
;; https://github.com/masasam/emacs-easy-hugo#installation
(leaf easy-hugo
  :straight t
  :config
  (setq easy-hugo-basedir "~/git_repositories/renyddd.github.io/")
  (setq easy-hugo-url "https://blog.renyddd.top/")
  (setq easy-hugo-root "~/git_repositories/renyddd.github.io/") ;; what's the difference
  (setq easy-hugo-previewtime "300")
  (setq easy-hugo-postdir "content/posts")
  (easy-hugo-enable-menu)
  )

;;; Dictionary
;;; https://www.reddit.com/r/emacs/comments/3yjzmu/dictionary_and_thesaurus_in_emacs/
;; youdao
(leaf youdao-dictionary
  :straight t
  :bind
  ("C-c d y" . youdao-dictionary-search-at-point))

;; called osx dictionary
(leaf osx-dictionary
  :straight t
  :bind
  ("C-c d o" . osx-dictionary-search-word-at-point))

;; define-word from https://oremacs.com/2015/05/22/define-word/
(leaf define-word
  :straight t
  :bind
  ("C-c d d" . define-word-at-point))

(load "~/wwc/wwc.el")
(require 'wwc)
(global-set-key (kbd "C-c d c") 'wwc/capture-word)

(provide 'init-note)
;;; init-note.el ends here
