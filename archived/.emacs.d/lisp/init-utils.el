;;; init-utils.el

;; https://github.com/hbin/smart-shift
;; default `C-C <left>` smart-shift-left conflict with winer-undo
;; (leaf smart-shift
;;   :straight t
;;   :config
;;   (global-smart-shift-mode 1))


;; keyfreq, https://github.com/dacap/keyfreq
(leaf keyfreq
  :straight t
  :config
  (setq keyfreq-excluded-commands
		'(mwheel-scroll))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  )

;; multiple-cursors, to get out of multiple-cursor-mode,
;; press C-g
(leaf multiple-cursors
  :straight t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
		 ;; when you have an active region that spans multiple
		 ;; lines, mc/edit-lines will add a cursor to each line
		 ("C->" . mc/mark-next-like-this)
		 ("C-<" . mc/mark-previous-like-this)
		 ("C-c C-<" . mc/mark-all-like-this)
		 ;; when you want to add multiple cursor not based on
		 ;; continuous line, but based on keywords in the
		 ;; buffer.
		 ;; first mark the world.

		 ;; Tips
		 ;; 1. pressing mc/mark-next-like-this with no region
		 ;; selected. It will just add a cursor on the next
		 ;; line.
		 )
  )

;; expand-region, https://github.com/magnars/expand-region.el
;; expand-region word, sentence, block, whole buffer
(leaf expand-region
  :straight t
  :bind (("C-=" . er/expand-region)
		 )
  )

;; string-edit, https://github.com/magnars/string-edit.el
;; edit the string with a pop up buffer without manually
;; escape characters
(leaf string-edit
  :straight t
  )

;; jq-mode
(leaf jq-mode
  :straight t)

;; avy
(leaf avy
  :straight t
  :bind
  ("C-:" . avy-goto-char)	  ; input one char, jump to it with a tree
  ("C-'" . avy-goto-char-2)	; input two consecutive chars, jump to the first one with a tree
  ("C-\"" . avy-goto-char-timer) ; input a arbitrary amount of consecutive chars,
  ;; jump to the first one with a tree
  ("M-g f" . avy-goto-line)	; input zero chars, jump to a line start with a tree
  )


;; helm, https://github.com/emacs-helm/helm/wiki#helm-completion-vs-emacs-completion
;; helm completion is based on the completion window, `C-h m` display help
;; without qiting helm session.
;; `C-x c` default help-command-prefix
;; (leaf helm
;;   :straight t
;;   :bind
;;   ("M-x" . helm-M-x))

;; ivy, a generic completion frontend
(leaf ivy
  :straight t
  :diminish t
  :config
  (ivy-mode 1)
  (setq ivy-use-virutal-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-height 10)
  (setq ivy-initial-inputs-alist nil)
  )

;; counsel, a collection of Ivy-enhanced versions of common Emacs commands.
(leaf counsel
  :straight t
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  )

;; swiper, isearch with a overview
(leaf swiper
  :straight t
  :bind
  ("C-s" . swiper))


(provide 'init-utils)
;;; init-utils.el ends here
