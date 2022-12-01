(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq use-package-always-defer t)

;; So that I can use emacsclient as my editor.
(require 'server)
(unless (server-running-p) (server-start))

;; General editor behavior / appearance
(if (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(if (fboundp 'mouse-wheel-mode)
  (mouse-wheel-mode -1))

(global-display-line-numbers-mode)
(global-hl-line-mode 1)
(column-number-mode 1)

;; forward-sentence and related commands should expect sentences to
;; end with a single space.
(setq sentence-end-double-space nil)

(when (eq system-type 'gnu/linux)
  (set-face-attribute 'default nil :font "Source Code Pro-11:weight=regular")
  ;; Show emoji properly
  (when (member "Symbola" (font-family-list))
    (set-fontset-font t 'unicode "Symbola" nil 'prepend)))

(use-package solarized-theme
  :demand t
  :config
  (load-theme 'solarized-dark t))

(ido-mode 1)
(setq ido-enable-flex-matching t)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(use-package smex
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands)
   ("C-c C-c M-x" . execute-extended-command)))

;; Use mail-mode for my messages from mutt.
(setq auto-mode-alist (append '(("/mutt" . mail-mode))
		       auto-mode-alist))
(add-hook 'mail-mode-hook (lambda () (auto-fill-mode 1)))
(add-hook 'mail-mode-hook (lambda () (flyspell-mode 1)))
(add-hook 'mail-mode-hook
	  (lambda () (set (make-local-variable 'make-backup-files) nil)))

;; My main address
(setq user-mail-address "eddie@lemald.org")

;; General-purpose programming config
(use-package magit
  :bind
  ("C-x g" . magit-status))

(use-package exec-path-from-shell
  :demand t
  :config
  (delete "-i" exec-path-from-shell-arguments)
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Don't split off ediff into separate windows
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(use-package eglot
  :hook ((rust-mode . eglot-ensure)
	 (haskell-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs '(rust-mode "rustup" "run" "stable" "rust-analyzer")))

(use-package company
  :hook prog-mode)

(use-package flymake
  :bind
  (("M-n" . flymake-goto-next-error)
   ("M-p" . flymake-goto-prev-error)))

(use-package rust-mode)

(use-package haskell-mode)

(use-package markdown-mode
  :config
  (add-hook 'markdown-mode-hook (lambda () (auto-fill-mode 1)))
  (add-hook 'markdown-mode-hook (lambda () (flyspell-mode 1))))
