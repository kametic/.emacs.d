; first of all we initialize package system
(require 'package)
(package-initialize)

;(exec-path-from-shell-initialize)


; copied from https://github.com/redguardtoo/emacs.d/blob/master/init.el
;;----------------------------------------------------------------------------
;; Which functionality to enable (use t or nil for true and false)
;;----------------------------------------------------------------------------
(setq *is-a-mac* (eq system-type 'darwin))
(setq *is-carbon-emacs* (and *is-a-mac* (eq window-system 'mac)))
(setq *win32* (eq system-type 'windows-nt) )
(setq *cygwin* (eq system-type 'cygwin) )
(setq *is-cocoa-emacs* (and *is-a-mac* (eq window-system 'ns)))
(setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)) )
(setq *unix* (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)) )
(setq *linux-x* (and window-system *linux*) )
(setq *xemacs* (featurep 'xemacs) )
(setq *emacs23* (and (not *xemacs*) (or (>= emacs-major-version 23))) )
(setq *emacs24* (and (not *xemacs*) (or (>= emacs-major-version 24))) )
(setq *no-memory* (cond
                   (*is-a-mac*
                    (< (string-to-number (nth 1 (split-string (shell-command-to-string "sysctl hw.physmem")))) 4000000000))
                   (*linux* nil)
                   (t nil)
                   ))



; Package repository
;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-refresh-contents)
(when (not package-archive-contents)
  (package-refresh-contents))
(if (not (package-installed-p 'use-package))
  (package-install 'use-package)
 )


(add-to-list 'load-path "~/.emacs.d/ejemba")
(add-to-list 'load-path "~/.emacs.d/ejemba/bootstrap/use-package")
(add-to-list 'load-path "~/.emacs.d/ejemba/bootstrap/emacs-sourcegraph-mode")
; replace-colortheme is not in *elpa
(add-to-list 'custom-theme-load-path "~/.emacs.d/ejemba/bootstrap/replace-colorthemes")
(add-to-list 'custom-theme-load-path  "~/.emacs.d/elpa/org-beautify-theme-20150106.956/" )


(require 'ox-taskjuggler)
(require 'sourcegraph-mode)


;(add-to-list 'org-export-backends 'taskjuggler)

;; load your favorite theme
;;(load-theme 'classic t t)
;;(enable-theme 'classic)
(load-theme 'desert t t)
(enable-theme 'desert)

(require 'use-package)

;(load "ejemba-proxy")
(load "ejemba-configuration")
(load "ejemba-golang")
(load "ejemba-web")
(load "ejemba-functions")
(load "ejemba-java")
(when (equal window-system 'w32)
  (load "ejemba-win32")
  )


; helm
; ido flex
; smex
; paredit
; paredit-everywhere

;ac-helm ou helm-company
(put 'scroll-left 'disabled nil)
