(setq user-full-name "Ronnie Holm")
(setq user-mail-address "mail@bugfree.dk")

(tool-bar-mode -1)                             ;; don't show the toolbar

(setq inhibit-startup-message t                ;; don't show ...
      inhibit-startup-echo-area-message t)     ;; ... startup messages

(setq scroll-margin 1                          ;; do smooth scrolling
      scroll-conservatively 100000
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)

(global-font-lock-mode t)                      ;; always do syntax highlighting
(blink-cursor-mode 0)                          ;; don't blink cursor
(setq backup-inhibited t)                      ;; no backup files
(setq delete-by-moving-to-trash t)             ;; delete moves to recycle bin
(iswitchb-mode t)                              ;; easy buffer switching

(column-number-mode t)                         ;; show column numbers
(size-indication-mode t)                       ;; show file size

(setq save-place-file "~/.emacs.d/saveplace")  ;; location of saveplace file
(setq-default save-place t)                    ;; activate for all buffer
(require 'saveplace)

(setq org-hide-leading-stars t)                ;; hide but one star in outline
(setq org-add-levels-only t)                   ;; align items nicely
(setq org-clock-persist t)                     ;; keep track of time ...
(org-clock-persistence-insinuate)              ;; ... across sessions
(setq org-clock-out-remove-zero-time-clocks t) ;; remove 0-duration clocked

(let ((default-directory "~/.emacs.d/site-lisp/"))  ;; add paths recursively
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path))) ;; Shadow
           (append 
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

; remapping caps-lock
; http://emacs-fu.blogspot.dk/2008/12/remapping-caps-lock.html
(setq w32-enable-caps-lock nil)
(global-set-key [capslock] 'execute-extended-command)

;; from http://emacs-fu.blogspot.dk/2009/06/erc-emacs-irc-client.html
;; joining && autojoing
;; make sure to use wildcards for e.g. freenode as the actual server
;; name can be be a bit different, which would screw up autoconnect
(erc-autojoin-mode t)
(setq erc-autojoin-channels-alist
  '((".*\\.freenode.net" "##fsharp" "#clojure")))

;; check channels
(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                 "324" "329" "332" "333" "353" "477"))
;; don't show any of this
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

(defun rh-erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (if (get-buffer "irc.freenode.net:6667") ;; ERC already active?

    (erc-track-switch-buffer 1) ;; yes: switch to last active
    (when (y-or-n-p "Start ERC? ") ;; no: maybe start ERC
      (erc :server "irc.freenode.net" :port 6667 :nick "RonnieDk"))))

;;; Initialize MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(unless package-archive-contents (package-refresh-contents))
(package-initialize)

;;; Install fsharp-mode
(unless (package-installed-p 'fsharp-mode)
  (package-install 'fsharp-mode
		   'color-theme))

(require 'fsharp-mode)

(setq inferior-fsharp-program "\"c:\\Program Files (x86)\\Microsoft SDKs\\F#\\3.0\\Framework\\v4.0\\fsi.exe\"")
(setq fsharp-compiler "\"c:\\Program Files (x86)\\Microsoft SDKs\\F#\\3.0\\Framework\\v4.0\\fsc.exe\"")
