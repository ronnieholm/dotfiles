(setq user-full-name "Ronnie Holm")
(setq user-mail-address "mail@bugfree.dk")
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
(setq-default indent-tabs-mode nil) ;; space over tabs
(setq-default tab-width 4)
(setq-default compilation-scroll-output t)

;; don't show the toolbar and scrollbar
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; shortcut for typing in yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; show argument list and help for identifier under cursor
(eldoc-mode 1)

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

(setq org-hide-leading-stars t
      org-add-levels-only t
      org-add-levels-only t
      org-clock-out-remove-zero-time-clocks t
      org-clock-into-drawer t) ;; clock time in :LOGBOOK: drawer

;; keep track of time across sessions
(setq org-clock-persist t)
(org-clock-persistence-insinuate)

;; resize windows
;; https://www.emacswiki.org/emacs/WindowResize
;;(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
;;(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
;;(global-set-key (kbd "S-C-<down>") 'shrink-window)
;;(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; move cursor between windows
;; https://www.emacswiki.org/emacs/WindMove
;; The defautl S-arrow keybinding is incompatible with org-mode
;;(when (fboundp 'windmove-default-keybindings)
;;  (windmove-default-keybindings 'S))
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

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

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

;; better minibuffer completions. Use of ido and helm are mutually exclusive.
;;(ido-mode t)
;;(setq ido-everywhere t)
;;(setq ido-enable-flex-matching t)

(use-package helm
  :ensure t
  :config (helm-mode 1))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package ox-twbs
  :ensure t)

(use-package dashboard
  :ensure t
  :config
    (dashboard-setup-startup-hook)
    (setq dashboard-items '((recents  . 10)))
    (setq dashboard-banner-logo-title "Welcome to Emacs"))

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
    (setq spaceline-buffer-encoding-abbrev-p nil)
    (setq spaceline-line-column-p t)
    (setq spaceline-line-p nil)
    (setq powerline-default-separator (quote arrow))
    (spaceline-spacemacs-theme))

(use-package magit
  :ensure t
  :config
  (setq magit-push-always-verify nil)
  (setq git-commit-summary-max-length 50)
  :bind
  ("C-c m s" . magit-status)
  ("C-c m l" . magit-log))

(use-package git-gutter
  :ensure t)

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 1)
  (setq company-minimum-prefix-length 3))
(global-company-mode)

(use-package company-lsp
  :ensure t
  :config
  (push 'company-lsp company-backends)) 

(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  (setq projectile-globally-ignored-file-suffixes
      '("#" "~" ".swp" ".o" ".so" ".exe" ".dll" ".elc" ".pyc" ".jar"))
  (setq projectile-globally-ignored-directories
      '(".git" "node_modules" "__pycache__" ".vs"))
  (setq projectile-globally-ignored-files '("TAGS" "tags" ".DS_Store")))

(use-package neotree
  :ensure t
  :bind (("<f2>" . neotree-toggle)))

(use-package csharp-mode
  :ensure t)

(add-hook 'csharp-mode-hook
	  '(lambda()
	     (electric-pair-mode)))

(use-package go-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package lsp-mode
  :ensure t
;;  :hook (csharp-mode . lsp)  ;; omnisharp not working with Emacs
  :commands lsp)

;; https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :ensure t)

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)

;; https://github.com/magnars/expand-region.el
(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region)
  ("C--" . er/contract-region))

;; use M-up/M-down to move selection up and down
(use-package move-text
  :ensure t)
(global-set-key (kbd "<M-up>") 'move-text-up)
(global-set-key (kbd "<M-down>") 'move-text-down)

;; https://github.com/abo-abo/avy
(use-package avy
  :ensure t)
(global-set-key (kbd "C-:") 'avy-goto-char)

(load-theme 'deeper-blue)

(defun rh/duplicate-line ()
  "Duplicate current line"
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (newline)
  (yank))

(global-set-key (kbd "C-,") 'rh/duplicate-line)
