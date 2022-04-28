;;; init-note --- initialize org mode settings
;;; Commentary:
;; take note by org or markdown

;;; Code:
(require 'init-const)

;; https://orgmode.org/manual/Workflow-states.html#Workflow-states
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")
        ))

;; https://cpbotha.net/2019/11/02/forming-and-maintaining-habits-using-orgmode/
;; I prefer to log TODO creation also
(setq org-treat-insert-todo-heading-as-state-change t)
(setq org-log-into-drawer t)

(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "~/notes.org"))


(provide 'init-note)
;;; init-note.el ends here
