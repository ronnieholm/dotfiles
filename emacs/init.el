(setq user-full-name "Ronnie Holm")
(setq user-mail-address "mail@bugfree.dk")
(setq calendar-latitude 55.58556)
(setq calendar-longitude 12.13139)
(setq calendar-location-name "Roskilde")
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq scroll-margin 1)              ;; do smooth scrolling
(setq scroll-conservatively 100000)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq ring-bell-function 'ignore)   ;; disable Emacs sound
(setq backup-inhibited t)
(setq delete-by-moving-to-trash t)  ;; delete moves to recycle bin
(setq-default fill-column 80)       ;; increase from default of 70.
(setq-default indent-tabs-mode nil) ;; spaces over tabs
(setq-default tab-width 4)
(setq-default compilation-scroll-output t)
(setq gc-cons-threshold (* 50 1024 1024)) ;; in bytes. Default is 800 KB
(setq compilation-ask-about-save nil) ;; save all modified buffer without asking
(setq compile-command "dotnet build")

;; don't show the toolbar and scrollbar
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; line numbering
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'absolute)
  (dolist (mode '(term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; (add-hook 'before-save-hook
;;           '(lambda()
;;              ;; Go mode registers hook for lsp-format-buffer which also removes
;;              ;; trailing whitespace. We should avoid double work here.
;;              (when (derived-mode-p 'prog-mode)
;;                (delete-trailing-whitespace))))

;; shortcut for typing yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; change font
(defun rh/get-default-font ()
  (cond
   ((eq system-type 'windows-nt) "Consolas-10")
   ((eq system-type 'gnu/linux) "DejaVu Sans Mono-10")))

(add-to-list 'default-frame-alist `(font . ,(rh/get-default-font)))

(global-font-lock-mode t)
(blink-cursor-mode 0)
(column-number-mode t)
(size-indication-mode t)

(global-set-key (kbd "<f11>") (lambda() (interactive) (find-file "~/Downloads/Life.md")))
(global-set-key (kbd "<f12>") (lambda() (interactive) (find-file "~/.emacs.d/init.el")))

;; dired
(setq dired-listing-switches "-alh")
(setf dired-kill-when-opening-new-dired-buffer t)

(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(global-set-key (kbd "C-,") 'duplicate-line)

;; add paths recursively
(let ((default-directory "~/.emacs.d/site-lisp/"))
  (setq load-path
    (append
         (let ((load-path (copy-sequence load-path)))
           (append
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

(package-initialize)
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package try)

(use-package helpful)

(use-package evil)

;; default is c-x w <number> but that's a lot of typing
;(winum-set-keymap-prefix (kbd "½"))

(setq winum-keymap
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "S-<f5>") 'winum-select-window-1)
      (define-key map (kbd "S-<f6>") 'winum-select-window-2)
      (define-key map (kbd "S-<f7>") 'winum-select-window-3)
      (define-key map (kbd "S-<f8>") 'winum-select-window-4)
      map))

;; alternative to windmove
(use-package winum)
(winum-mode)

;; Sacha Chua: Emacs microhabit - Switching windows
;; https://www.youtube.com/watch?v=nKCKuRuvAOw
(use-package ace-window
  :bind ("C-x o" . ace-window))

(use-package which-key
  :config (which-key-mode))

(use-package helm
  :config (helm-mode 1))

(global-set-key (kbd "<f5>") 'compile)
(global-set-key (kbd "<f6>") 'recompile)
(global-set-key (kbd "<f7>") 'next-error)
(global-set-key (kbd "<f8>") 'previous-error)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-c") 'helm-calcul-expression)
(global-set-key (kbd "C-s") 'helm-occur)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-h a") 'helm-apropos)

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

(use-package ox-twbs)

(use-package dashboard
  :config
    (dashboard-setup-startup-hook)
    (setq dashboard-items '((recents  . 10)))
    (setq dashboard-banner-logo-title
          (format "%s" (sunrise-sunset()))))

(use-package magit
  :config
  (setq magit-push-always-verify nil)
  (setq git-commit-summary-max-length 50)
  :bind
  ("C-c m s" . magit-status)
  ("C-c m l" . magit-log))

(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-merge-split-window-function 'split-window-horizontally)

;; avoid typing y to quit ediff session
(defun disable-y-or-n-p (orig-fun &rest args)
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))
    (apply orig-fun args)))

(advice-add 'ediff-quit :around #'disable-y-or-n-p)

(use-package git-gutter)

(use-package company
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

(global-company-mode)

(use-package projectile
  :config
  (projectile-mode)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  (setq projectile-globally-ignored-file-suffixes
      '("#" "~" ".swp" ".o" ".so" ".exe" ".dll" ".elc" ".pyc" ".jar"))
  (setq projectile-globally-ignored-directories
      '(".git" "node_modules" "__pycache__" ".vs"))
  (setq projectile-globally-ignored-files '("TAGS" "tags" ".DS_Store"))
  :bind-keymap
  ("C-c C-p" . projectile-command-map)
  :init
  (when (file-directory-p "~/git")
    (setq projectile-project-search-path '("~/git")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package helm-projectile)
(helm-projectile-on)

(use-package neotree
  :bind (("<f2>" . neotree-toggle))
  :config
  (setq neo-window-fixed-size nil))

(use-package go-mode)
(add-hook 'go-mode-hook
          '(lambda()
             (add-hook 'before-save-hook #'lsp-format-buffer t t)
             (add-hook 'before-save-hook #'lsp-organize-imports t t)))

(use-package rust-mode)
(use-package fsharp-mode)

(add-hook 'csharp-mode-hook
	      '(lambda()
	         (electric-pair-mode)
             (local-set-key (kbd "C-c c") 'projectile-compile-project)
             (setq truncate-lines -1)))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (
         (csharp-mode . lsp)
         (fsharp-mode . lsp)
         (go-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(require 'dap-netcore)

(use-package paredit)

(add-hook 'emacs-lisp-mode-hook
          '(lambda()
             (eldoc-mode 1)
             ))

;; https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors)

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;; https://github.com/magnars/expand-region.el
(use-package expand-region
  :bind
  ("C-=" . er/expand-region)
  ("C--" . er/contract-region))

;; use M-up/M-down to move selection up and down
(use-package move-text)
(global-set-key (kbd "<M-up>") 'move-text-up)
(global-set-key (kbd "<M-down>") 'move-text-down)

;; https://github.com/abo-abo/avy
(use-package avy)

(use-package git-gutter)
(global-git-gutter-mode 1)

(global-set-key (kbd "C-;") 'avy-goto-char)
(global-set-key (kbd "C-:") 'avy-goto-char-2)

(load-theme 'deeper-blue)

(sunrise-sunset)

(use-package helm-rg)
