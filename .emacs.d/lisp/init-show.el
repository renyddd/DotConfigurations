(global-font-lock-mode 1)

;; disable the menu bar and the scroll bar
(if (display-graphic-p)
	(progn
	  (tool-bar-mode -1)
	  (menu-bar-mode -1)
	  (scroll-bar-mode -1)
	  ))

;; Also highlight parens
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(show-paren-mode 1)

;; http://stackoverflow.com/questions/294664/how-to-set-the-font-size-in-emacs
(set-face-attribute 'default nil :height 180)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; highlight matching parens
(leaf paren
  :hook (after-init-hook . show-paren-mode)
  :custom
  (show-paren-when-point-inside-paren . t)
  (show-paren-when-point-in-periphery . t))

;;; Some optional configurations
;; display-line-numbers-mode
;; hl-line-mode

;; highlight brackets according to their depth
(leaf rainbow-delimiters
  :straight t
  :hook (prog-mode-hook . rainbow-delimiters-mode))

;; highlight modified region using pulse
;; https://github.com/minad/goggles
(leaf goggles
  :straight t
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t)) ;; set to nil to disable pulsing

;; https://github.com/antonj/Highlight-Indentation-for-Emacs
(leaf highlight-indentation
  :straight t
  :bind
  ("C-c C-i" . highlight-indentation-aj-toggle-fold)
  :config
  (highlight-indentation-mode)
  (defun highlight-indentation-aj-toggle-fold ()
	"Toggle fold all lines larger than indentation on current line,
from https://blog.chmouel.com/2016/09/07/dealing-with-yaml-in-emacs/"
	(interactive)
	(let ((col 1))
      (save-excursion
		(back-to-indentation)
		(setq col (+ 1 (current-column)))
		(set-selective-display
		 (if selective-display nil (or col 1)))))))

;; never lose your cursor, https://github.com/Malabarba/beacon
(leaf beacon
  :straight t
  :config
  (beacon-mode 1))

;; highlight TODO and similar keywords in comments and strings
(leaf hl-todo
  :straight t
  :hook (after-init-hook . global-hl-todo-mode)
  :init
  (setq hl-todo-keyword-faces
        '(("FIXME" . (face-foreground 'error))
          ("ISSUE" . (face-foreground 'error))
          ("BUG" .( (face-foreground 'error)))
          ("DEBUG" . (face-foreground 'warning))
          ("TODO" . (face-foreground 'warning))
          ("TRICK" . (face-foreground 'warning))
          ("HACK" . (face-foreground 'warning)))))

;; display element of modeline, https://github.com/tarsius/moody
(leaf moody
  :straight t
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

;; select window using shift and arrow keys, like
;; S-<right> runs the command windmove-right (found in
;; windmove-mode-map
;; https://www.emacswiki.org/emacs/WindMove
(leaf windmove
  :hook (after-init-hook . windmove-default-keybindings))

;; select window, https://github.com/abo-abo/ace-window
;; - select windows by characters
;; - swap window by C-u ace-window
(leaf ace-window
  :straight t
  :bind
  (([remap other-window] . ace-window)
   ("M-o" . ace-window)))

;; a global minor mode allows you to undo and redo changes in
;; [[WindowConfiguration][https://www.emacswiki.org/emacs/WindowConfiguration]]
;; a window configurations is the layer of the various windows in an
;; Emacs frame. Changes can be undo with winner-undo, C-c <left>
;; redo C-c <right>
(leaf winner
  :straight t
  :commands (winner-undo winner-redo)
  :hook (after-init-hook . winner-mode)
  :custom (winner-dont-bind-my-key . nil)
  (winner-boring-buffers . '("*Completions*" ; windows not restore
                             "*Compile-Log*"
                             "*inferior-lisp*"
                             "*Fuzzy Completions*"
                             "*Apropos*"
                             "*Help*"
                             "*cvs*"
                             "*Buffer List*"
                             "*Ibuffer*"
                             "*esh command on file*")))

;; ibuffer
(leaf ibuffer
  :bind ("C-x C-b" . ibuffer)
  :custom
  (ibuffer-expert . t)
  (ibuffer-movement-cycle . nil)
  (ibuffer-show-empty-filter-groups . nil))

(straight-use-package 'ibuffer-project)
(add-hook 'ibuffer-hook
          (lambda ()
            (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))))

;; which-key
(leaf which-key
  :straight t
  :diminish (which-key-mode)
  :hook (after-init-hook . which-key-mode)
  :custom
  (which-key-idle-delay . 0.5)
  (which-key-add-column-padding . 0))

;; better *help* buffer, https://github.com/Wilfred/helpful
;; with source code, pretty doc, callers
(leaf helpful
  :straight t
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h p" . helpful-at-point)))

;; helm, https://github.com/emacs-helm/helm/wiki#helm-completion-vs-emacs-completion
;; helm completion is based on the completion window, `C-h m` display help
;; without qiting helm session.
;; `C-x c` default help-command-prefix

(leaf helm
  :straight t
  :bind
  ("M-x" . helm-M-x))

(provide 'init-show)
;;; init-show.el ends here
