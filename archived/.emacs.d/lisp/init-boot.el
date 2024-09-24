;; init-boot.el -- init basic setting

;;; Commentary:
;; Edit the default Emacs config and internal pacakges

;;; Code:
(require 'init-const)

;; encoding
;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

(setq require-final-newline t)

;; show time
(display-time-mode)

;; keep ~/.emacs.d clean, https://github.com/emacscollective/no-littering
(leaf no-littering
  :straight t
  :leaf-defer nil
  :custom
  (auto-save-file-name-transforms . `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
;; (add-hook 'text-mode-hook 'display-fill-column-indicator-mode)

(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :custom ((auto-revert-interval . 0.1))
  :global-minor-mode global-auto-revert-mode)

(leaf recentf
  :hook
  (after-init-hook . recentf-mode)
  :bind
  ("C-x C-r" . recentf-open-files)
  :custom
  (recentf-max-saved-items . 300)
  (recentf-exclude . '("\\.?cache"
                       ".cask"
                       "url"
                       "COMMIT_EDITMSG\\'"
                       "bookmarks"
                       "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
                       "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
                       (lambda (file) (file-in-directory-p file package-user-dir))))
  :init
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude))

;; savehist
(leaf savehist
  :custom
  (enable-recursive-minibuffers . t) ; Allow commands in minibuffers
  (history-length . 1000)
  (savehist-additional-variables . '(mark-ring
                                     global-mark-ring
                                     search-ring
                                     regexp-search-ring
                                     extended-command-history))
  (savehist-autosave-interval . 300)
  :global-minor-mode savehist-mode)

;; show information in modeline
;; show line/column/filesize
(leaf simple
  :hook
  (after-init-hook . global-visual-line-mode)
  :custom 
  (line-number-mode . t)
  (column-number-mode . t)
  (size-infication-mode . t)
  (visual-line-fringe-indicators . '(nil right-curly-arrow))
  (save-interprogram-paste-before-kill . t)
  (what-cursor-show-names . t)
  (kill-whole-line . t))

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Minibuffer.html
(leaf minibuffer
  :bind
  ((:minibuffer-local-map
    ([remap escape] . abort-recursive-edit))
   (:minibuffer-local-ns-map
    ([remap escape] . abort-recursive-edit))
   (:minibuffer-local-completion-map
    ([remap escape] . abort-recursive-edit))
   (:minibuffer-local-must-match-map
    ([remap escape] . abort-recursive-edit))
   (:minibuffer-local-isearch-map
    ([remap escape] . abort-recursive-edit)))
  :custom
  (minibuffer-depth-indicate-mode . t)
  (minibuffer-electric-default-mode . t)
  (enable-recursive-minibuffers . t))

;; https://github.com/wakatime/wakatime-mode
;; (global-wakatime-mode t)

;; go-mode failed in kubernetes repo
(setq max-lisp-eval-depth 4000)

;; https://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)



(provide 'init-boot)
;;; init-boot.el ends here
