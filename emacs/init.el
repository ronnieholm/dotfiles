(setq user-full-name "Ronnie Holm")
(setq user-mail-address "mail@bugfree.dk")
(setq calendar-latitude 55.58556)
(setq calendar-longitude 12.13139)
(setq calendar-location-name "Roskilde")
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq scroll-margin 1)
(setq scroll-conservatively 100000)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq ring-bell-function 'ignore)
(setq backup-inhibited t)
(setq delete-by-moving-to-trash t)
(setq-default fill-column 80)
(setq-default indent-tabs-mode nil) ;; spaces over tabs
(setq-default tab-width 4)
(setq-default compilation-scroll-output t)
(setq gc-cons-threshold (* 50 1024 1024)) ;; in bytes. Default is 800 KB
(setq compilation-ask-about-save nil) ;; save all modified buffer without asking
(setq compile-command "dotnet build")

;; don't show the toolbar and scrollbar
(tool-bar-mode -1)
(scroll-bar-mode -1)

(when (not (display-graphic-p))
  (menu-bar-mode -1))

;; line numbering
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'absolute)
  (dolist (mode '(term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

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

(global-set-key (kbd "<f11>") (lambda() (interactive) (find-file "~/Downloads/TODO.md")))
(global-set-key (kbd "<f12>") (lambda() (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-x 2") (lambda() (interactive) (select-window (split-window-below))))
(global-set-key (kbd "C-x 3") (lambda() (interactive) (select-window (split-window-right))))

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

(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :ensure t
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annontations-heavy marginalia-annotations-light nil))
  :init
  (marginalia-mode))

;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
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
         ("C-S-s" . consult-line-multi)            ;; orig. isearch-forward
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

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"
)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(global-set-key (kbd "<f5>") 'compile)
(global-set-key (kbd "<f6>") 'recompile)
(global-set-key (kbd "<f7>") 'previous-error)
(global-set-key (kbd "<f8>") 'next-error)

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

(use-package dashboard
  :config
    (dashboard-setup-startup-hook)
    (setq dashboard-items '((recents  . 10)))
    (setq dashboard-banner-logo-title
          (format "%s" (sunrise-sunset()))))

(use-package magit
  :config
  (setq magit-push-always-verify nil)
  (setq git-commit-summary-max-length 50))

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
  (setq company-idle-delay 1)
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
  :bind-keymap ("C-c C-p" . projectile-command-map)
  :init
  (when (file-directory-p "~/git")
    (setq projectile-project-search-path '("~/git")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package neotree
  :bind (("<f2>" . neotree-toggle))
  :config
  (setq neo-window-fixed-size nil))

(use-package go-mode)
(add-hook 'go-mode-hook
          '(lambda()
             (add-hook 'before-save-hook #'lsp-format-buffer t t)
             (add-hook 'before-save-hook #'lsp-organize-imports t t)))

(use-package fsharp-mode)
(add-hook 'fsharp-mode-hook
          '(lambda()
             (add-hook 'before-save-hook #'lsp-format-buffer t t)))

(add-hook 'csharp-mode-hook
	      '(lambda()
	         (electric-pair-mode)
             (setq truncate-lines -1)))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off
  (setq lsp-lens-enable nil)
  (setq lsp-ui-sideline-enable t)
  :hook (
         (csharp-mode . lsp)
         (fsharp-mode . lsp)
         (go-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package dap-mode)

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

(load-theme 'wombat)

(use-package ispell
  :no-require t
  :config
  (setq ispell-dictionary "en_US")
  (setq ispell-highlight-face (quote flyspell-incorrect))
  (setq ispell-silently-savep t))
  
(use-package flyspell
  :defer t
  :init
  (progn
    (add-hook 'message-mode-hook 'turn-on-flyspell)
    (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)
    (add-hook 'text-mode-hook 'flyspell-mode)
    (add-hook 'markdown-mode-hook 'flyspell-mode)))
