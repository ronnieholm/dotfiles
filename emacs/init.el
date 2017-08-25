(setq user-full-name "Ronnie Holm")
(setq user-mail-address "mail@bugfree.dk")

;; don't show the toolbar
(tool-bar-mode -1)

;; shortcut for typing in yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; don't show startup messages
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; do smooth scrolling
(setq scroll-margin 1     
      scroll-conservatively 100000
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)

;; always do syntax highlighting
(global-font-lock-mode t)

;; don't blink cursor
(blink-cursor-mode 0)

;; no backup files
(setq backup-inhibited t)

;; delete moves to recycle bin
(setq delete-by-moving-to-trash t)

;; show column numbers
(column-number-mode t)

;; show file size
(size-indication-mode t)

;; location of saveplace file
(setq save-place-file "~/.emacs.d/saveplace")

;; activate for all buffer
(setq-default save-place t)
(require 'saveplace)

;; hide but one star in outline
(setq org-hide-leading-stars t)

;; align items nicely
(setq org-add-levels-only t)

;; keep track of time across sessions
(setq org-clock-persist t)
(org-clock-persistence-insinuate)

;; remove 0-duration clocked
(setq org-clock-out-remove-zero-time-clocks t)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(defalias 'list-buffers 'ibuffer)

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
		(lambda () (interactive) (find-file "C:/Users/ronnie/Google Drive/Life.org")))

;; http://emacs-fu.blogspot.dk/2009/06/erc-emacs-irc-client.html
(erc-autojoin-mode t)
(setq erc-autojoin-channels-alist
  '((".*\\.freenode.net" "##c" "#haskell-beginners")))

;; check channels
(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                 "324" "329" "332" "333" "353" "477"))
;; don't show any of this
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

(defun rh-erc-start-or-switch ()
  "Connect to ERC or switch to last active buffer"
  (interactive)
  ;; ERC already active?      
  (if (get-buffer "irc.freenode.net:6667") 
    ;; yes: switch to last active
    (erc-track-switch-buffer 1) 
    ;; no: maybe start ERC
    (when (y-or-n-p "Start ERC? ") 
      (erc :server "irc.freenode.net" :port 6667 :nick "RonnieHolm"))))

;; initialize MELPA
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

;;; haskell config
(use-package haskell-mode
  :ensure t)

(use-package hindent
  :ensure t)

(use-package ghc
  :ensure t)

(use-package company-ghc
  :ensure t)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;; enable pressing F8 to jump to import section of Haskell source file
(eval-after-load 'haskell-mode
  '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))

;;; enable minor mode to re-indent Haskell code
(add-hook 'haskell-mode-hook #'hindent-mode)

;;; use hasktags.exe to generate tags for navigation on save
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-ghc-show-info t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote stack-ghci))
 '(haskell-tags-on-save t)
 '(package-selected-packages
   (quote
    (debbugs which-key use-package try solarized-theme powershell-mode powershell org-bullets markdown-mode hindent fsharp-mode company-ghc))))



(eval-after-load 'haskell-mode '(progn
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)))

(eval-after-load 'haskell-cabal '(progn
  (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

;;; configure haskell-mode to initialize ghc-mod each time we open a Haskell file
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

;;; add auto-completion popups in haskell mode
(require 'company)
(add-hook 'haskell-mode-hook 'company-mode)
(add-to-list 'company-backends 'company-ghc)


;;; install fsharp-mode
(unless (package-installed-p 'fsharp-mode)
  (package-install 'fsharp-mode))

(require 'fsharp-mode)

(setq inferior-fsharp-program "\"c:\\Program Files (x86)\\Microsoft SDKs\\F#\\3.0\\Framework\\v4.0\\fsi.exe\"")
(setq fsharp-compiler "\"c:\\Program Files (x86)\\Microsoft SDKs\\F#\\3.0\\Framework\\v4.0\\fsc.exe\"")
(setq fsharp-ac-debug t)

(load-theme 'deeper-blue)

;; gnus
(setq gnus-select-method
    '(nntp "news.gmane.org"
	   (nntp-port-number 119)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "outline" :family "DejaVu Sans Mono")))))
