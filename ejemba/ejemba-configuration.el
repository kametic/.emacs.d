(setq custom-file "~/.emacs.d/ejemba/emacs-custom.el")
(load custom-file 'noerror)


(set-face-attribute 'default nil :height 85)

(global-set-key [f3] nil )
;(global-set-key [f4] nil )

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

;; selection region color
(set-face-attribute 'region nil :background "#666")


;appearance
(setq visible-bell t
      inhibit-startup-message t
      font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil)

;; Highlight current line
(global-hl-spotlight-mode t)
(setq hl-spotlight-height 0)


; Split horizontaly => one left one right
; http://stackoverflow.com/questions/2081577/setting-emacs-split-to-horizontal
(setq split-width-threshold 0)
;(setq split-height-threshold 100)
;(setq split-width-threshold 100)

(setq plantuml-jar-path "~/.emacs.d/plantuml.jar")

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
(setq-default show-trailing-whitespace nil)

;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Don't be so stingy on the memory, we have lots now. It's the distant future.
(setq gc-cons-threshold 20000000)

;; Scroll down with the cursor,move down the buffer one
;; line at a time, instead of in larger amounts.
(setq scroll-step 1)

;; do not make backup files
;(setq make-backup-files nil)
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.backupemacs"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups


;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)
(setq show-paren-style 'mixed) ; parenthesis highlight entire bracket expression parenthesis
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
(bind-key "<pause>" 'kill-this-buffer)


;(bind-key "<C-x ->" 'split-window-right)
;(bind-key "<C-x _>" 'split-window-below)

;(global-set-key "C-x -" 'split-window-right)


; Set up keybindings for `windmove'.
; Keybindings are of the form MODIFIER-{left,right,up,down}.
; Default MODIFIER is 'shift.
(windmove-default-keybindings)


; https://github.com/Kungsgeten/selected.el


(use-package epl
  :ensure t)


(use-package framemove
  :ensure t
  :config (progn
            (windmove-default-keybindings)
            (setq framemove-hook-into-windmove t)
            )
  )


(use-package selected
  :ensure t
  :init  (selected-minor-mode 1)
  :bind (:map selected-keymap
              ("q" . selected-off)
              ("u" . upcase-region)
              ("t" . string-rectangle)
              ("k" . kill-rectangle)
              ("c" . kill-ring-save)
              ("r" . projectile-replace)
              ("d" . downcase-region)
              ("w" . count-words-region)
              ("m" . apply-macro-to-region-lines)))

;(use-package selected
;  :commands selected-minor-mode
;  :config (progn
;            
;            (define-key selected-keymap (kbd "q") #'selected-off)
;            (define-key selected-keymap (kbd "u") #'upcase-region)
;            (define-key selected-keymap (kbd "d") #'downcase-region)
;            (define-key selected-keymap (kbd "w") #'count-words-region)
;            (define-key selected-keymap (kbd "m") #'apply-macro-to-region-lines)
;            ))


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


(use-package prodigy

  :config (progn

            (prodigy-define-service
             :name "Python app"
             :command "python2"
             :args '("-m" "SimpleHTTPServer" "6001")
             :cwd "/tmp"
             :tags '(work)
             :kill-signal 'sigkill
             :kill-process-buffer-on-stop t)
            )
  :ensure t
  )

(use-package key-chord
  :bind ("C-c n k" . key-chord-mode)
  :init (key-chord-mode 1)
  :config
  (progn
    (key-chord-define-global "##" 'server-edit)
    (key-chord-define-global "VV" 'other-window)
    (key-chord-define-global "OO" 'other-window)
    (key-chord-define-global "KK" 'ido-kill-buffer)
;    (key-chord-define-global "$$" 'ispell-buffer)
    (key-chord-define-global "BB" 'helm-buffers-list)
    ;; Pretty much everything in Enlish word beginning with 'q' is
    ;; follewed the vowel 'u'.  These chords take advantage of that.
    (key-chord-define-global "qq" 'read-only-mode)
    (key-chord-define-global "qs" 'save-buffer)
    (key-chord-define-global "$$" 'delete-window)
    (key-chord-define-global "jj" 'ace-jump-char-mode)
    (key-chord-define-global "zz" 'zap-to-char)
    (key-chord-define-global "TT" 'ejemba/showgotests)
    (key-chord-define-global "FF" 'helm-flycheck)
    (key-chord-define-global "qf" 'flymake-popup-current-error-menu)
    (key-chord-define-global "\t\t" 'end-of-visual-line)
    (key-chord-define-global "==" 'imenus-mode-buffers)
    (key-chord-define-global "++" 'helm-imenu)
    (key-chord-define-global "ii" 'ejemba/split-horizontally-other)
    (key-chord-define-global "__" 'ejemba/split-vertically-other)
    (key-chord-define-global "§§" 'end-of-buffer)
    (key-chord-define-global "%%" 'beginning-of-buffer)
    (key-chord-define-global "àà" 'delete-window))
  :ensure t
  )

;; weather from wttr.in
(use-package wttrin
  :ensure t
  :commands (wttrin)
  :init
  (setq wttrin-default-cities '("Poissy"
                                "Suresnes")))



;(use-package shackle
;  :init (shackle-mode t)
;  :config (progn
;           (setq shackle-default-rule '(:same t))
;           )
;  :ensure t
;  )
;
  
(use-package super-save
  :init (super-save-mode 1)
  :ensure t
  )


;(use-package blank-mode :ensure t)

(use-package clojure-mode :ensure t)

(use-package clojure-snippets :ensure t)

(use-package cider :ensure t)

(use-package highlight-symbol :ensure
  :init ( progn
          (add-hook 'prog-mode-hook 'highlight-symbol-mode)
          (add-hook 'prog-mode-hook 'highlight-symbol-nav-mode)
          )

  t)

(use-package bm :ensure t

  :init (progn
          ;(bm-buffer-persistence t)
         )
  
  :bind (
         ("<f4>" . helm-bm)
         ("C-<f4>" . bm-toggle)
         ("<f5>" . bm-next)
         ("C-<f5>" . bm-previous)
         )
  )

(use-package helm-bm :ensure t)

(use-package winner-mode-enable :ensure t)
(winner-mode t)

; revive to save buffer configuration
; * save-current-configuration
; * resume to resume
(use-package revive :ensure t)

(use-package flycheck :ensure t)

(use-package hydra :ensure t); ++

(use-package real-auto-save

  :init (progn
          ;enable auto save in all programming modes
          ;(add-hook 'prog-mode-hook 'real-auto-save-mode)
          (setq real-auto-save-interval 3) ;; in seconds
          )

  :ensure t
  )



(use-package guide-key
  :init (progn
          (setq guide-key/guide-key-sequence '("C-c" "C-c ESC" "C-c /" "C-c n" "C-c x"
                                               "C-x"  "C-x r" "C-x 4" "<f2>"))
          (guide-key-mode 1))
  :ensure t)

(use-package ace-jump-mode
  :bind (("C-c SPC" . ace-jump-word-mode)
         ("C-c C-SPC" . ace-jump-word-mode))
  :ensure t)

(use-package ace-window :ensure t)

(use-package intero

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
          (setq projectile-ignored-directories  '("Godeps" "_output"))
          (setq projectile-ignored-files '(".DS_Store" ".gitmodules" ".gitignore" "pkg" "bin") )
          (add-to-list 'guide-key/guide-key-sequence "C-c p")
          )
  :bind (
         ("<f3>" . helm-projectile-switch-project)
         ("<f6>" . helm-projectile)
         ("<f12>" . helm-projectile)
         ("C-o" . helm-projectile)
         ("C-S-o" . helm-projectile-switch-to-buffer)
         ("C-<f6>" . helm-projectile-switch-to-buffer)
         )
  :ensure t)


(use-package org-projectile
  :bind (("C-c n p" . org-projectile:project-todo-completing-read)
         ("C-c c" . org-capture))
  :config
  (progn
    
    (org-projectile:per-repo)
    (setq org-projectile:per-repo-filename "todo.org")
    
    )
  :ensure t
  )


(use-package iflipb
  
  :bind (
         ( "C-²" . iflipb-next-buffer) 
         ( "C-&" . iflipb-previous-buffer) 
         )
  :init (progn
          (setq iflipb-wrap-around t)
          )
  :config
  (progn
; this does not work    (key-chord-define-global "œœ" 'iflipb-next-buffer)
    (key-chord-define-global "&&" 'iflipb-previous-buffer)
    )

  :ensure t)



;(use-package sourcegraph
;  :config (progn
;            (add-hook 'prog-mode-hook 'sourcegraph-mode)
;            )
;  :ensure    t)


(use-package company :ensure t)

;(use-package company-emacs-eclim :ensure t)

;(use-package emacs-eclim :ensure t)

;(progn
;  (custom-set-variables
;   '(eclim-eclipse-dirs '("/home/ejemba/Apps/eclim/eclipse"))
;   '(eclim-executable "/home/ejemba/Apps/eclim/eclipse/eclim"))
;  
;  (setq eclim-auto-save t
;        eclim-executable  "/home/ejemba/Apps/eclim/eclipse/eclim" 
;        eclimd-executable "/home/ejemba/Apps/eclim/eclipse/eclimd" 
;        eclimd-wait-for-process nil
;        eclimd-default-workspace "~/workspace/"
;        help-at-pt-display-when-idle
;        )
;
;  ;; Call the help framework with the settings above & activate
;  ;; eclim-mode
;  (help-at-pt-set-timer)
;  ;; keep consistent which other auto-complete backend.
;  (custom-set-faces
;   '(ac-emacs-eclim-candidate-face ((t (:inherit ac-candidate-face))))
;   '(ac-emacs-eclim-selection-face ((t (:inherit ac-selection-face)))))
;  
;  ;; Hook eclim up with auto complete mode
;  (require 'ac-emacs-eclim-source)
;  ;; (ac-emacs-eclim-config)
;  
;  (require 'eclimd)
;  
;  (add-hook 'java-mode-hook
;            (lambda ()
;              (add-to-list 'ac-sources 'ac-source-emacs-eclim)
;              (eclim-mode t))))


;; ================================================================================


(use-package helm-projectile
  :ensure t)

(use-package ibuffer-projectile
  :ensure t)


; https://github.com/sigma/magit-gh-pulls
;(use-package magit
;  :bind ("C-c C-g" . magit-status)
;   :config
;
;   (progn
;     (setq magit-last-seen-setup-instructions "1.4.0")
;     )
;  :ensure t)

;(use-package magit-gh-pulls
;  :init (progn
;          (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
;          )
;  :ensure t
;  )

(use-package elisp-slime-nav
  :bind (("C-;" . elisp-slime-nav-find-elisp-thing-at-point)
         ("C-c C-d C-d" . elisp-slime-nav-describe-elisp-thing-at-point)
         ("C-c C-d d" . elisp-slime-nav-describe-elisp-thing-at-point))
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

;(use-package engine-mode
;  :commands (engine-mode defengine)
;  :init (engine-mode t)
;  :config
;  (progn
;    (defengine duckduckgo
;      "https://duckduckgo.com/?q=%s"
;      "C-c e d")
;    (defengine github
;      "https://github.com/search?ref=simplesearch&q=%s"
;      "C-c e g")
;    (defengine wikipedia
;      "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
;      "C-c e w"))
;  :ensure t)
;

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
  :init 
  (progn
    
    (ac-config-default)
    (ac-fuzzy-complete)
    (setq ac-auto-start 4)
    (define-key ac-mode-map (kbd "C-TAB") 'auto-complete)
    )
  :ensure t)

(use-package org
  :init
  (progn
    (org-indent-mode t)
    (setq org-confirm-babel-evaluate nil
          org-src-fontify-natively t
          org-src-tab-acts-natively t)
    )
  

  :ensure t)

(use-package imenu

  :ensure t)

;(use-package imenu+ :ensure t)

;(use-package imenu-anywhere
;  :init
;  (key-chord-define-global "MM" 'helm-imenu)
;  :ensure t)
;
(use-package deft
  :init (progn
	  ; deft settings
	  (global-set-key [f8] 'deft)
	  (setq deft-extension "org")
	  (setq deft-text-mode 'org-mode)
	  (setq deft-use-filename-as-title t)
	  )
  :ensure t)

(use-package helm
  :bind (("C-x b" . helm-buffers-list)
	 ("C-x C-f" . helm-find-files)
	 ;("M-x" . helm-M-x )
	 ("M-y" . helm-show-kill-ring)
	 )
  :init  (progn (helm-mode)
                (setq helm-buffers-fuzzy-matching t)
                (setq helm-split-window-default-side 'right)
                                        ;(fset 'list-packages 'helm-list-elisp-packages)
                (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
                )

  :ensure t)

(use-package helm-themes :ensure t)

(use-package ac-helm
  :bind ("C-:" . ac-complete-with-helm)
    :ensure t)

(use-package helm-helm-commands :ensure t)
(use-package helm-swoop
  :bind (("C-s" . helm-swoop))
  :init (progn
          ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
          (setq helm-swoop-split-direction 'split-window-horizontally)
          ;; removing search on current key word
          ; From https://github.com/ShingoFukuyama/helm-swoop/issues/25
          (setq helm-swoop-pre-input-function (lambda () nil))
          ;(key-chord-define-global "ff" 'helm-swoop)
          )
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
;(add-hook 'prog-mode-hook 'real-auto-save-mode)
(add-hook 'prog-mode-hook 'subword-mode)
;(add-hook 'prog-mode-hook 'selected-minor-mode)
;(add-hook 'prog-mode-hook 'glasses-mode)

                                        ;(add-hook 'emacs-lisp-mode-hook 'highlight-symbol-mode)
;(add-hook 'emacs-lisp-mode-hook 'highlight-symbol-nav-mode)

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
(toggle-truncate-lines)

(defun ejemba-load-org-beautify-theme ()
  "manually loading the org-theme"

  (load-file "" )
  
  )

;;; ejemba-configuration.el ends here
