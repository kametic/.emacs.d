(setq custom-file "~/.emacs.d/ejemba/emacs-custom.el")
(load custom-file 'noerror)

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
(global-hl-line-mode 1)

;; Font configuration
(global-font-lock-mode 1)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Save minibuffer history
(savehist-mode 1)
(setq history-length 1000)

;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

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

;; 
(use-package  undo-tree :init
  (global-undo-tree-mode)
  :ensure t)

(use-package org :ensure t)

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
                ( setq helm-buffers-fuzzy-matching t))
  
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

(use-package paredit-everywhere
  :commands paredit-everywhere-mode
  :init (progn
	  (add-hook 'prog-mode-hook 'paredit-everywhere-mode)
	  ;(paredit-everywhere-mode) 
	  )
  :ensure t)

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

