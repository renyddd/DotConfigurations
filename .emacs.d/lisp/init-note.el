;;; init-note --- initialize org mode settings
;;; Commentary:
;; take note by org or markdown

;;; Code:
(require 'init-const)

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





(provide 'init-note)
;;; init-note.el ends here
