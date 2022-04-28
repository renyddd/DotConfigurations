;;; early-init.el --- pre-initialize setting

;;; Commentary:
;; introduced in Emacs 27
;; only use it for internal setting, no pacakge init allowed here
;; copy from: https://github.com/AxiomCrisis/erupmacs/blob/master/early-init.el

;;; Code:
;; defer garbage collection further back in the startup process
;; (setq gc-cons-threshold most-positive-fixnum)

;; disable pacakge for speed up
(setq package-enable-at-startup nil
      package-quickstart nil)

;; suppress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t)

;; explicitly set the prefered coding systems
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)

(setq locale-coding-system 'utf-8
      default-process-coding-system '(utf-8 . utf-8))

;; better default
(setq default-directory "~/"
      read-answer-short t
      ring-bell-function 'ignore

      ;; Better scroll behavior
      mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil))
      mouse-wheel-progressive-speed nil)

;; native comp
(when (featurep 'comp)
  (setq native-comp-deferred-compilation (not noninteractive))
  (add-to-list 'native-comp-eln-load-path (concat user-emacs-directory ".local/data/eln-cache")))

;; better gui
(push '(vertical-scroll-bars) default-frame-alist) ;; no scroll bar
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

(fset 'yes-or-no-p 'y-or-n-p)

;;; early-init ends here
