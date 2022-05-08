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
  (setq org-todo-keywords
	'((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")
          ))
  ;; https://cpbotha.net/2019/11/02/forming-and-maintaining-habits-using-orgmode/
  ;; I prefer to log TODO creation also
  (setq org-treat-insert-todo-heading-as-state-change t)
  (setq org-log-into-drawer t)
  ;; some directories
  (setq org-directory "~/org")
  (setq org-default-notes-file (concat org-directory "~/notes.org"))
  ;;
  (setq org-display-remote-inline-images 'download)

  ;; settings for ox-hugo
  (with-eval-after-load 'ox
	(require 'ox-hugo))
  (setq org-hugo-base-dir "~/git_repositories/renyddd.github.io/"
		org-hugo-default-section-directory "posts")
  )

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

;; ;; shengci capture
;; (leaf shengci
;;   :straight '(shengci
;; 			  :type git
;; 			  :host github
;; 			  :repo "EvanMeek/shengci")
;;   :bind
;;   ("C-c d c" . shengci-capture-word-and-save)
;;   ("C-c d l" . shengci-show-recorded-word))

(provide 'init-note)
;;; init-note.el ends here
