;; -*- lexical-binding: t; -*-

(setq user-full-name "Ronnie Holm"
      user-mail-address "mail@bugfree.dk"
      calendar-latitude 55.58556
      calendar-longitude 12.13139
      calendar-location-name "Roskilde"
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      scroll-margin 1
      scroll-conservatively 100000
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      ring-bell-function 'ignore
      backup-inhibited t
      delete-by-moving-to-trash t
      gc-cons-threshold (* 50 1024 1024) ;; in bytes. Default is 800 KB
      compilation-ask-about-save nil ;; save all modified buffer without asking
      compile-command "dotnet build"
      ediff-split-window-function 'split-window-horizontally
      ediff-merge-split-window-function 'split-window-horizontally
      use-dialog-box nil
      confirm-kill-processes nil
      require-final-newline t
      warning-minimum-level :error
      use-short-answers t
      initial-scratch-message nil
      custom-file (locate-user-emacs-file "custom.el"))

(load custom-file 'noerror)

(setq-default fill-column 80
              indent-tabs-mode nil ;; spaces over tabs
              tab-width 4
              compilation-scroll-output t)

;; don't show the toolbar and scrollbar
(tool-bar-mode -1)
(scroll-bar-mode -1)

(unless (display-graphic-p)
  (menu-bar-mode -1))

;; line numbering
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'absolute)
(dolist (mode '(term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; change font
(defun rh/get-default-font ()
  (cond
   ((eq system-type 'windows-nt) "Consolas-10")
   ((eq system-type 'gnu/linux) "DejaVu Sans Mono-10")))

(setq default-frame-alist
      `((font . ,(rh/get-default-font))
        (width . 120)
        (height . 60)))

(defun rh/delete-trailing-blank-lines ()
  "Remove blank lines at end of buffer, leaving exactly one newline at EOF."
  (when (and (buffer-file-name) (not (string= (buffer-name) "*scratch*")))
    (save-excursion
      (goto-char (point-max))
      (skip-chars-backward " \t\n")
      (unless (bolp)
        (delete-region (point) (point-max))
        (insert "\n")))))

(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'before-save-hook #'rh/delete-trailing-blank-lines)

(global-font-lock-mode t)
(blink-cursor-mode 0)
(column-number-mode t)
(size-indication-mode t)

(global-set-key (kbd "<f12>") (lambda() (interactive) (find-file user-init-file)))
(global-set-key (kbd "C-x 2") (lambda() (interactive) (select-window (split-window-below))))
(global-set-key (kbd "C-x 3") (lambda() (interactive) (select-window (split-window-right))))
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)
(global-set-key (kbd "C-,") 'duplicate-line)
(global-set-key (kbd "<f5>") 'compile)
(global-set-key (kbd "<f6>") 'recompile)
(global-set-key (kbd "<f7>") 'previous-error)
(global-set-key (kbd "<f8>") 'next-error)

;; add paths recursively
(let ((default-directory "~/.emacs.d/site-lisp/"))
  (progn
    (add-to-list 'load-path default-directory)
    (normal-top-level-add-to-load-path '("."))
    (normal-top-level-add-subdirs-to-load-path)))

(use-package dired
  :config
  (setq dired-listing-switches "-alh")
  (setf dired-kill-when-opening-new-dired-buffer t))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)

(use-package minions
  :config (minions-mode t))

(use-package try)
(use-package helpful)

;; (use-package evil
;;   :init
;;   (setq evil-default-state 'emacs
;;         evil-want-C-w-in-emacs-state t
;;         evil-want-C-w-delete nil
;;         evil-want-Y-yank-to-eol t
;;         evil-want-C-u-scroll t
;;         evil-vsplit-window-right t
;;         evil-split-window-below t
;;         evil-undo-system 'undo-redo
;;         evil-symbol-word-search t
;;         evil-kill-on-visual-paste nil)
;;   :config
;;   (dolist (mode '(prog-mode
;;                   text-mode
;;                   conf-mode
;;                   fundamental-mode
;;                   emacs-lisp-mode))
;;     (evil-set-initial-state mode 'normal))
;;   (evil-set-initial-state 'git-commit-mode 'emacs)
;;   (defalias #'forward-evil-word #'forward-evil-symbol)
;;   (evil-mode 1))

(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode t))

(use-package evil-exchange
  :after evil
  :config (evil-exchange-install))

;; Sacha Chua: Emacs microhabit - Switching windows
;; https://www.youtube.com/watch?v=nKCKuRuvAOw
(use-package ace-window
  :bind ("C-x o" . ace-window))

(use-package which-key
  :config (which-key-mode))

(use-package vertico
  :init (vertico-mode)
  :custom (vertico-cycle t))

(use-package savehist
  :init (savehist-mode))

(use-package marginalia
  :after vertico
  :init (marginalia-mode)
  :custom
  (marginalia-annotators '(marginalia-annontations-heavy
                           marginalia-annotations-light
                           nil)))

(use-package editorconfig
  :config (editorconfig-mode t))

(use-package consult
  :bind
  (;; C-c bindings in `mode-specific-map'
   ("C-c M-x" . consult-mode-command)
   ("C-c h" . consult-history)
   ("C-c k" . consult-kmacro)
   ("C-c m" . consult-man)
   ("C-c i" . consult-info)
   ([remap Info-search] . consult-info)
   ;; C-x bindings in `ctl-x-map'
   ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
   ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
   ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
   ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
   ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
   ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
   ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
   ;; Custom M-# bindings for fast register access
   ("M-#" . consult-register-load)
   ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
   ("C-M-#" . consult-register)
   ;; Other custom bindings
   ("M-y" . consult-yank-pop)                ;; orig. yank-pop
   ;; M-g bindings in `goto-map'
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
   ("M-g g" . consult-goto-line)             ;; orig. goto-line
   ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
   ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ;; M-s bindings in `search-map'
   ("M-s d" . consult-find)                  ;; Alternative: consult-fd
   ("M-s c" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("C-s" . consult-line)                    ;; orig. isearch-forward
   ("C-S-s" . consult-ripgrep)               ;; orig. isearch-forward
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)
   ;; Isearch integration
   ("M-s e" . consult-isearch-history)
   :map isearch-mode-map
   ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
   ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
   ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
   ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
   ;; Minibuffer history
   :map minibuffer-local-map
   ("M-s" . consult-history)                 ;; orig. next-matching-history-element
   ("M-r" . consult-history))                ;; orig. previous-matching-history-element
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :config
  ;; preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<"
        register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles basic partial-completion)))))

;; https://leanpub.com/markdown-mode/read
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command
        (concat "pandoc"
                " --from=markdown --to=html"
                " --standalone --mathjax --highlight-style=pygments")))

(use-package markdown-toc)

(use-package magit
  :config
  (setq magit-push-always-verify nil
        git-commit-summary-max-length 50))

;; avoid typing y to quit ediff session
(defun disable-y-or-n-p (orig-fun &rest args)
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))
    (apply orig-fun args)))

(advice-add 'ediff-quit :around #'disable-y-or-n-p)

(use-package git-gutter)

(use-package company
  :config
  (setq company-idle-delay 1
        company-minimum-prefix-length 1))

(global-company-mode)

(use-package projectile
  :config
  (projectile-mode)
  (setq projectile-enable-caching t
        projectile-indexing-method 'alien
        projectile-globally-ignored-file-suffixes '("#" "~" ".swp" ".o" ".so" ".exe" ".dll" ".elc" ".pyc" ".jar")
        projectile-globally-ignored-directories '(".git" "node_modules" "__pycache__" ".vs")
        projectile-globally-ignored-files '("TAGS" "tags" ".DS_Store"))
  :bind-keymap ("C-c C-p" . projectile-command-map)
  :init
  (when (file-directory-p "~/git")
    (setq projectile-project-search-path '("~/git")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package neotree
  :bind (("<f2>" . neotree-toggle))
  :config (setq neo-window-fixed-size nil))

(use-package go-mode)
(add-hook 'go-mode-hook
          (lambda()
            (electric-pair-mode 1)
            (add-hook 'before-save-hook #'lsp-format-buffer t t)
            (add-hook 'before-save-hook #'lsp-organize-imports t t)))

(use-package fsharp-mode
  :hook (fsharp-mode .
         (lambda ()
           (electric-pair-mode 1)
           (add-hook 'before-save-hook #'lsp-format-buffer nil t))))

(use-package csharp-mode
  :hook (csharp-mode .
	      (lambda()
	        (electric-pair-mode 1))))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l"
        ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off
        lsp-lens-enable nil
        lsp-ui-sideline-enable t)
  :hook
  ((csharp-mode . lsp)
   (fsharp-mode . lsp)
   (go-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

;; https://www.youtube.com/watch?v=0bilcQVSlbM and https://emacs-lsp.github.io/dap-mode/page/configuration
(use-package dap-mode
  :after lsp-mode
  :init
  (setq dap-netcore-download-url "https://github.com/Samsung/netcoredbg/releases/download/3.1.2-1054/netcoredbg-linux-amd64.tar.gz")
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (require 'dap-netcore))

(use-package paredit)

(add-hook 'emacs-lisp-mode-hook
          '(lambda()
             (eldoc-mode 1)))

;; https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . 'mc/edit-lines)
  ("S-M-<down>" . 'mc/mark-next-like-this)
  ("S-M-<up>" . 'mc/mark-previous-like-this)
  ("C-c C-<" . 'mc/mark-all-like-this)
  ("C-\"" . 'mc/skip-to-next-like-this)
  ("C-:" . 'mc/skip-to-previous-like-this)
  ("S-M-<mouse-1>" . 'mc/add-cursor-on-click))

;; https://github.com/magnars/expand-region.el
(use-package expand-region
  :bind
  ("C-=" . er/expand-region)
  ("C--" . er/contract-region))

;; https://github.com/emacsfodder/move-text
(use-package move-text
  :bind
  ("<M-up>" . 'move-text-up)
  ("<M-down>" . 'move-text-down))

;; https://github.com/abo-abo/avy
(use-package avy
  :bind
  ("C-;" . 'avy-goto-char)
  ("C-:" . 'avy-goto-char-2))

(use-package git-gutter
  :config
  (global-git-gutter-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(load-theme 'modus-vivendi-tinted)

(use-package ispell
  :config
  (setq ispell-dictionary "en_US"
        ispell-highlight-face '(flyspell-incorrect)
        ispell-silently-savep t))

(use-package flyspell
  :hook
  ((message-mode git-commit-setup text-mode markdown-mode) . flyspell-mode))

(use-package erc
  :config
  (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                  "324" "329" "332" "333" "353" "477")
        erc-hide-list '("JOIN" "NICK" "PART" "QUIT")))
