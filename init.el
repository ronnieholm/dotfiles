(setq user-full-name "Ronnie Holm")
(setq user-mail-address "mail@bugfree.dk")

;; don't show the toolbar and scrollbar
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; shortcut for typing in yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; don't show startup messages
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; increase from default of 70.
(setq-default fill-column 80)

;; change font
(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono-10"))

;; do smooth scrolling
(setq scroll-margin 1     
      scroll-conservatively 100000
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)

;; always do syntax highlighting
(global-font-lock-mode t)

;; don't blink cursor
(blink-cursor-mode 0)

;; disable Emacs sound
(setq ring-bell-function 'ignore)

;; no backup files
(setq backup-inhibited t)

;; delete moves to recycle bin
(setq delete-by-moving-to-trash t)

;; show column numbers
(column-number-mode t)

;; show file size
(size-indication-mode t)

;; hide but one star in outline
(setq org-hide-leading-stars t)

;; align items nicely
(setq org-add-levels-only t)

;; keep track of time across sessions
(setq org-clock-persist t)
(org-clock-persistence-insinuate)

;; remove 0-duration clocked
(setq org-clock-out-remove-zero-time-clocks t)

;; clock time in :LOGBOOK: draw
(setq org-clock-into-drawer t)

;; resize windows
;; https://www.emacswiki.org/emacs/WindowResize
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; move cursor between windows
;; https://www.emacswiki.org/emacs/WindMove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; add paths recursively
(let ((default-directory "~/.emacs.d/site-lisp/"))
  (setq load-path
    (append
         ;; Shadow
         (let ((load-path (copy-sequence load-path)))
           (append 
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

;; remapping caps-lock
;; http://emacs-fu.blogspot.dk/2008/12/remapping-caps-lock.html
(setq w32-enable-caps-lock nil)
(global-set-key [capslock] 'execute-extended-command)

(global-set-key (kbd "\el")
		(lambda () (interactive) (find-file "C:/Users/rh/Google Drive/Life.org")))

;; initialize MELPA
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
    ("M-g" . magit-status))

(use-package git-gutter
  :ensure t)

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3))

(use-package csharp-mode  
  :ensure t)

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

(load-theme 'deeper-blue)
