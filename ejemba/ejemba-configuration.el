(setq custom-file "~/.emacs.d/ejemba/emacs-custom.el")
(load custom-file 'noerror)

(global-set-key [f3] nil )
;(global-set-key [f4] nil )

;; Changes all yes/no questions to y/n type
;(fset 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top
;appearance
(setq visible-bell t
      inhibit-startup-message t
      font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil)

;; Highlight current line
;(global-hl-line-mode 1)




;; Font configuration
(global-font-lock-mode 1)

; does not cut word on truncate line mode
(global-visual-line-mode 1)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Save minibuffer history
(savehist-mode 1)
(setq history-length 1000)

;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)
(setq-default show-trailing-whitespace t)

;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Don't be so stingy on the memory, we have lots now. It's the distant future.
(setq gc-cons-threshold 20000000)

;; Scroll down with the cursor,move down the buffer one
;; line at a time, instead of in larger amounts.
(setq scroll-step 1)

;; do not make backup files
(setq make-backup-files nil)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)
(setq show-paren-style 'expression) ; highlight entire bracket expression
(show-paren-mode 1)
;(electric-pair-mode 1)
(transient-mark-mode t)
(column-number-mode t)
(global-auto-revert-mode 1)
(global-set-key [f4] 'keyboard-escape-quit)
                                        ;
; UI
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode 0)

;; Allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)

(bind-key "<RET>" 'newline-and-indent)
(bind-key "<C-return>" 'newline)

; Set up keybindings for `windmove'.
; Keybindings are of the form MODIFIER-{left,right,up,down}.
; Default MODIFIER is 'shift.
(windmove-default-keybindings)

;
; USE-PACKAGE section
; :init is always executed but :config will happen after
; :bind, commands: are defered
;  usage of
;
;(use-package ruby-mode
;  :mode "\\.rb\\'"
;  :interpreter "ruby")
;
;;; The package is "python" but the mode is "python-mode":
;(use-package python
;  :mode ("\\.py\\'" . python-mode)
;  :interpreter ("python" . python-mode))

(use-package key-chord
  :bind ("C-c n k" . key-chord-mode)
  :init (key-chord-mode 1)
  :config
  (progn
    (key-chord-define-global "##" 'server-edit)
    (key-chord-define-global "VV" 'other-window)
    (key-chord-define-global "KK" 'ido-kill-buffer)
    (key-chord-define-global "$$" 'ispell-buffer)
    (key-chord-define-global "BB" 'helm-buffers-list)
    ;; Pretty much everything in Enlish word beginning with 'q' is
    ;; follewed the vowel 'u'.  These chords take advantage of that.
    (key-chord-define-global "qq" 'read-only-mode)
    (key-chord-define-global "qs" 'save-buffer)
    (key-chord-define-global "q0" 'delete-window)
    (key-chord-define-global "JJ" 'ace-jump-word-mode)
    (key-chord-define-global "qf" 'flymake-popup-current-error-menu))
  :ensure t
  )


(use-package flycheck :ensure t)

(use-package guide-key
  :init (progn
          (setq guide-key/guide-key-sequence '("C-c" "C-c ESC" "C-c /" "C-c n" "C-c x"
                                               "C-x"  "C-x r" "C-x 4"))
          (guide-key-mode 1))
  :ensure t)

(use-package ace-jump-mode
  :bind (("C-c SPC" . ace-jump-word-mode)
         ("C-c C-SPC" . ace-jump-word-mode))
  :ensure t)

(use-package haskell-mode
  :commands haskell-mode
  :init ; add extension 
  (add-to-list 'auto-mode-alist '("\\.l?hs$" . haskell-mode))
  :config ; defer loading
  (progn
    (use-package inf-haskell)
    (use-package hs-lint)))


(use-package projectile
  :init (progn
          (projectile-global-mode)
          (setq projectile-enable-caching t)
          (add-to-list 'guide-key/guide-key-sequence "C-c p")
          )
  :ensure t)

(use-package helm-projectile
  :ensure t)

(use-package magit
  :bind ("C-c C-g" . magit-status)
  :ensure t)


;;
(use-package  undo-tree
  :init
  (global-undo-tree-mode)
  :ensure t)

(use-package  icicles
  :ensure t)

;(use-package  yasnippet
;  :idle
;  (yas-global-mode 1)
;  :ensure t)


;;; Engine Mode:

(use-package engine-mode
  :commands (engine-mode defengine)
  :init (engine-mode t)
  :config
  (progn
    (defengine duckduckgo
      "https://duckduckgo.com/?q=%s"
      "C-c e d")
    (defengine github
      "https://github.com/search?ref=simplesearch&q=%s"
      "C-c e g")
    (defengine wikipedia
      "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
      "C-c e w"))
  :ensure t)

(use-package recentf
  :init
  (progn
    (recentf-mode 1)
    ;(add-hook 'emacs-startup-hook 'recentf-open-files)
    )
  :bind ("<f9>" . recentf-open-files)
  :ensure t)

;;; howdoi:

(use-package howdoi
  :bind ("C-c x h" . howdoi)
  :ensure t
  )

(use-package fuzzy
  :ensure t)

(use-package auto-complete
  :init (define-key ac-mode-map (kbd "C-TAB") 'auto-complete)
  :idle (progn
         (ac-config-default)
         (ac-fuzzy-complete)
         )
  :ensure t)

(use-package org :ensure t)

(use-package imenu

  :ensure t)

;(use-package imenu+ :ensure t)

(use-package imenu-anywhere
  :init
  (key-chord-define-global "MM" 'helm-imenu)
  :ensure t)

(use-package deft
  :init (progn
	  ; deft settings
	  ;(setq deft-directory "E:/Personnel/notes")
	  (global-set-key [f8] 'deft)
	  (setq deft-extension "org")
	  (setq deft-text-mode 'org-mode)
	  (setq deft-use-filename-as-title t)
	  )
  :ensure t)

(use-package helm
  :bind (("C-x b" . helm-buffers-list)
	 ("C-x C-f" . helm-find-files)
	 )
  :init  (progn (helm-mode)
                (setq helm-buffers-fuzzy-matching t)
                ;(fset 'list-packages 'helm-list-elisp-packages)
                )

  :ensure t)

(use-package ac-helm
  :bind ("C-:" . ac-complete-with-helm)
    :ensure t)

(use-package helm-helm-commands :ensure t)
(use-package helm-swoop
  :bind (("C-s" . helm-swoop))
  :ensure t)

;;; Show key-bindings for the current major mode:

(use-package discover-my-major
  :commands discover-my-major
  :bind ("C-h C-m" . discover-my-major)
  :ensure t)


;(use-package helm-ack :ensure t)

;;; Replace 'C-x C-b' with Ibuffer:

;(use-package ibuffer
;  :bind ("C-x C-b" . ibuffer)
;  :config (setq ibuffer-default-sorting-mode 'major-mode)
;  :ensure t)
;



;; Expand Region
;;; I use these packages to navigate and edit text in semantic terms,
;;; with the Expand Region package being the foundation for the rest.

(use-package expand-region
  :bind ("C-=" . er/expand-region)
  :config
  (progn
    (use-package change-inner 
      :bind (("M-i" . change-inner)
             ("M-o" . change-outer))
      :ensure t )
    )
  :ensure t
  )

;; IDO ;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ido
  :init
  (progn
    (ido-mode 1)
    (ido-everywhere 1)
    (setq ido-use-faces nil)
    
    )
  :config
  (progn
    (use-package ido-vertical-mode
      :init (ido-vertical-mode 1)
      :ensure t)
    
    )
  :ensure t
  )

(use-package ido-yes-or-no
      :init (ido-yes-or-no-mode)
      :ensure t)

(use-package flx
  :ensure t
  :config
  (progn
    (use-package flx-ido
      :init (flx-ido-mode 1)
      :ensure t)
    )
  )

(use-package smex
  :bind (("M-x" . smex)
	 
	 ("M-m" . smex-major-mode-commands))
  :commands (smex-initialize)
  :ensure t
  )

(use-package paredit
;  :mode ("\\.el" . paredit-mode)
  :init (progn
	  (add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode 1)))
	  
	  )
  :ensure t)

;(use-package paredit-everywhere
;  :commands paredit-everywhere-mode
;  :init (progn
;	  (add-hook 'prog-mode-hook 'paredit-everywhere-mode)
;	  ;(paredit-everywhere-mode) 
;	  )
;  :ensure t)

;(use-package smartparens 
;  :init (progn 
;	  (smartparens-global-mode t) 
;	  (show-smartparens-global-mode t)
;	  )
;  :ensure t)

;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)


;; HOOKS 

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

; From http://stackoverflow.com/questions/885793/emacs-error-when-calling-server-start#1566618
; to remove directory unsafe message
(require 'server)
(when (and (>= emacs-major-version 23)
           (equal window-system 'w32))
  (defun server-ensure-safe-dir (dir) "Noop" t)) 
					; Suppress error "directory
					; ~/.emacs.d/server is unsafe"
					; on windows.

(server-start)

