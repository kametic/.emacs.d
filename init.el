; first of all we initialize package system 
(require 'package)
(package-initialize)

; Package repository
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

;(package-refresh-contents)
(when (not package-archive-contents)
  (package-refresh-contents))
;(if (not (package-installed-p 'use-package))
;  (package-install 'use-package)
; )


(add-to-list 'load-path "~/.emacs.d/ejemba/bootstrap")
(add-to-list 'load-path "~/.emacs.d/ejemba")

(require 'use-package)

(load "ejemba-proxy")
(load "ejemba-configuration")
(load "ejemba-golang")
(load "ejemba-web")
(load "ejemba-functions")
(when (equal window-system 'w32) 
  (load "ejemba-win32")
  )


; helm
; ido flex
; smex
; paredit 
; paredit-everywhere

;ac-helm ou helm-company
