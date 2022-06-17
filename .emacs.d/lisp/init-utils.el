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


(provide 'init-utils)
;;; init-utils.el ends here
