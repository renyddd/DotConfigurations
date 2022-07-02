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


(provide 'init-utils)
;;; init-utils.el ends here
