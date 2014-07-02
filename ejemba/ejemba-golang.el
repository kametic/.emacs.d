(use-package go-mode
  :mode ("\\.go" . go-mode)
  :config (progn
            (setq tab-width 2)
            (setq standard-indent 2) 
            (setq indent-tabs-mode -1)
            
            (add-hook 'go-mode-hook
                      (lambda () (add-hook 'before-save-hook 'gofmt-before-save)))
            (add-hook 'go-mode-hook
                      (lambda () (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))
            (add-hook 'go-mode-hook
                      (lambda () (local-set-key (kbd "C-c i") 'go-goto-imports)))
            (add-hook 'go-mode-hook
                      (lambda () (local-set-key (kbd "<f3>") 'godef-jump)))
            (add-hook 'go-mode-hook
                      (lambda () (local-set-key (kbd "C-c C-c") 'go-errcheck)))
            )
  :ensure t)

;; Go Oracle
;; http://yousefourabi.com/blog/2014/05/emacs-for-go/
;; 

;; go get -u github.com/nsf/gocode
(use-package go-eldoc
  :init (add-hook 'go-mode-hook 'go-eldoc-setup)
  :ensure t)

(use-package go-autocomplete
  :ensure t)

(use-package go-snippets
  :ensure t)

;; go get github.com/kisielk/errcheck
(use-package go-errcheck
  :ensure t)

; go direx 
(use-package popwin ; needed by direx
  :init (popwin-mode 1)
  :ensure t)
; go get -u github.com/jimweirich/gotags
; use rake 
(use-package go-direx
  :init 
  :ensure t)

; go get github.com/golang/lint/golint
(use-package golint
  :ensure t)

;(use-package go-snippets :ensure t)

; Flycheck
;(eval-after-load "go-mode"
;  '(progn
;
;     (flycheck-describe-checker go-fmt
;       "A Go syntax and style checker using the gofmt utility."
;       :command '("gofmt" source-inplace)
;       :error-patterns '(("^\\(?1:.*\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): \\(?4:.*\\)$" error))
;       :modes 'go-mode)
;     (golint-mode)
;     (add-to-list 'flycheck-checkers 'go-gofmt)))

;  go-direx           20140303.447 available  Tree style source code viewer for Go language
;  go-errcheck        20140107.... available  errcheck integration for go-mode
;  go-play            20120914.... available  Paste to play.golang.org
;  go-projectile      20140603.... available  Go add-ons for Projectile
;  golint             20140122.... available  lint for the Go source code
