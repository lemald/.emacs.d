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

;; Some of the following general-purpose configuration is roughly
;; lifted from Emacs Bedrock: https://sr.ht/~ashton314/emacs-bedrock/

;; General editor behavior / appearance
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))

;; Enable horizontal scrolling
(setq mouse-wheel-tilt-scroll t)
(setq mouse-wheel-flip-direction t)

(pixel-scroll-precision-mode)

(global-display-line-numbers-mode)
(global-hl-line-mode 1)
(column-number-mode 1)

(setq inhibit-splash-screen t)

(setq confirm-kill-emacs 'y-or-n-p)

(setq indent-tabs-mode nil)

(setq auto-revert-avoid-polling t)
(setq auto-revert-interval 5)
(setq auto-revert-check-vc-info t)
(global-auto-revert-mode)

(savehist-mode)

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

(use-package emacs
  :config
  (load-theme 'modus-vivendi))

;; https://github.com/radian-software/straight.el/issues/819
(use-package vertico
  :demand t
  :straight (vertico
	     :files (:defaults "extensions/*")
	     :includes (vertico-buffer
			vertico-directory
			vertico-flat
			vertico-indexed
			vertico-mouse
			vertico-quick
			vertico-repeat
			vertico-reverse))
  :init
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :demand t
  :bind (:map vertico-map
              ("M-DEL" . vertico-directory-delete-word)))

(use-package marginalia
  :demand t
  :config
  (marginalia-mode))

(use-package corfu
  :demand t
  :straight (corfu :files (:defaults "extensions/*")
                   :includes (corfu-popupinfo))
  :init
  (global-corfu-mode)
  (setq corfu-auto t)
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)))

;; Part of corfu
(use-package corfu-popupinfo
  :after corfu
  :demand t
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

;; Pretty icons for corfu
(use-package kind-icon
  :if (display-graphic-p)
  :demand t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package orderless
  :demand t
  :config
  (setq completion-styles '(orderless)))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(use-package avy
  :demand t
  :bind (("C-c j" . avy-goto-line)
	 ("s-j"   . avy-goto-char-timer)))

(use-package consult
  :ensure t
  :bind (
         ;; Drop-in replacements
         ("C-x b" . consult-buffer)     ; orig. switch-to-buffer
         ("M-y"   . consult-yank-pop)   ; orig. yank-pop
         ;; Searching
         ("M-s r" . consult-ripgrep)
	 ("M-s g" . consult-git-grep)
         ("M-s l" . consult-line)
         ("M-s s" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s o" . consult-outline)
         ;; Isearch integration
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)   ; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history) ; orig. isearch-edit-string
         ("M-s l" . consult-line)            ; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)      ; needed by consult-line to detect isearch
         )
  :config
  ;; Narrowing lets you restrict results to certain groups of candidates
  (setq consult-narrow-key "<"))

;; which-key: shows a popup of available keybindings when typing a
;; long key sequence (e.g. C-x ...)
(use-package which-key
  :demand t
  :config
  (which-key-mode))

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
  :hook ((rust-ts-mode . eglot-ensure)
	 (haskell-mode . eglot-ensure)
	 (elixir-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs '(rust-ts-mode "rustup" "run" "stable" "rust-analyzer"))
  (setq eglot-connect-timeout nil)
  (fset #'jsonrpc--log-event #'ignore))

(use-package flymake
  :bind
  (("M-n" . flymake-goto-next-error)
   ("M-p" . flymake-goto-prev-error)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Tree Sitter grammar installation
(setq treesit-language-source-alist
      '((typescript . ("https://github.com/tree-sitter/tree-sitter-typescript.git" "master" "typescript/src"))
	(tsx . ("https://github.com/tree-sitter/tree-sitter-typescript.git" "master" "tsx/src"))
	(rust . ("https://github.com/tree-sitter/tree-sitter-rust.git" "master"))))

(dolist (entry treesit-language-source-alist)
  (let ((name (car entry)))
    (unless (treesit-language-available-p name)
      (treesit-install-language-grammar name))))

;; Language-specific modes
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(add-hook 'rust-ts-mode-hook (lambda () (add-hook 'before-save-hook 'eglot-format nil t)))

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

(use-package prettier-js
  :hook ((typescript-ts-mode . prettier-js-mode)
	 (tsx-ts-mode . prettier-js-mode)))

(use-package add-node-modules-path
  :config
  (setq add-node-modules-path-command '("echo \"$(npm root)/.bin\""))
  :hook ((typescript-ts-mode . add-node-modules-path)
	 (tsx-ts-mode . add-node-modules-path)))

(require 'typescript-ts-mode)

(use-package web-mode
  :mode
  "\\.html\\.h?eex\\'")

(use-package markdown-mode
  :config
  (add-hook 'markdown-mode-hook (lambda () (flyspell-mode 1))))

(use-package dockerfile-mode
  :mode
  "Dockerfile\\'")

(use-package terraform-mode)
