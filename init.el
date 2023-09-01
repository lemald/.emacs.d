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
(if (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

(global-display-line-numbers-mode)
(global-hl-line-mode 1)
(column-number-mode 1)

(setq inhibit-splash-screen t)

(setq confirm-kill-emacs 'y-or-n-p)

(setq indent-tabs-mode nil)

;; forward-sentence and related commands should expect sentences to
;; end with a single space.
(setq sentence-end-double-space nil)

(when (eq system-type 'gnu/linux)
  (set-face-attribute 'default nil :font "Source Code Pro-11:weight=regular")
  ;; Show emoji properly
  (when (member "Symbola" (font-family-list))
    (set-fontset-font t 'unicode "Symbola" nil 'prepend)))

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :font "Monaco-14"))

(use-package monokai-theme
  :demand t
  :config
  (load-theme 'monokai t))

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

;; My main address
(setq user-mail-address "eddie@lemald.org")

;; General-purpose programming config
(use-package magit
  :bind
  ("C-x g" . magit-status))

(use-package exec-path-from-shell
  :demand t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Don't split off ediff into separate windows
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(use-package eglot
  :hook ((rust-mode . eglot-ensure)
	 (haskell-mode . eglot-ensure)
	 (elixir-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs '(rust-mode "rustup" "run" "stable" "rust-analyzer")))

(use-package company
  :hook (prog-mode . company-mode))

(use-package flymake
  :bind
  (("M-n" . flymake-goto-next-error)
   ("M-p" . flymake-goto-prev-error)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Language-specific modes
(use-package rust-mode
  :config
  (add-hook 'rust-mode-hook (lambda () (add-hook 'before-save-hook 'eglot-format nil t))))

(use-package haskell-mode)

(use-package elixir-mode
  :config
  (add-hook 'elixir-mode-hook (lambda () (add-hook 'before-save-hook 'eglot-format nil t))))

(if (file-directory-p
      (concat (file-name-as-directory (getenv "HOME")) "elixir_ls"))
    (add-to-list
      'exec-path
      (concat (file-name-as-directory (getenv "HOME"))
	      (file-name-as-directory "elixir_ls"))))

(use-package prettier-js)

(use-package add-node-modules-path
  :config
  (setq add-node-modules-path-command '("echo \"$(npm root)/.bin\"")))

(use-package typescript-mode)

(use-package tide
  :init
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1)
    (add-node-modules-path)
    (prettier-js-mode))
  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  (add-hook 'web-mode-hook (lambda ()
			     (when (string-equal
				     "tsx"
				     (file-name-extension buffer-file-name))
			       (setup-tide-mode)))))

(use-package web-mode
  :mode
  "\\.html\\.h?eex\\'"
  "\\.tsx\\'")

(use-package markdown-mode
  :config
  (add-hook 'markdown-mode-hook (lambda () (flyspell-mode 1))))

(use-package dockerfile-mode
  :mode
  "Dockerfile\\'")

(use-package terraform-mode)
