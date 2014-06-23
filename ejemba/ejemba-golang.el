(use-package go-mode
  :mode ("\\.go" . go-mode)
  :init (progn
          (setq tab-width 4)
          (setq indent-tabs-mode 1)
          (add-hook 'go-mode-hook
                    (lambda () (add-hook 'before-save-hook 'gofmt-before-save)))
          (add-hook 'go-mode-hook
                    (lambda () (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))
          (add-hook 'go-mode-hook
                    (lambda () (local-set-key (kbd "C-c i") 'go-goto-imports)))
          (add-hook 'go-mode-hook
                    (lambda () (local-set-key (kbd "C-c C-j") 'godef-jump)))
          (add-hook 'go-mode-hook
                    (lambda () (local-set-key (kbd "C-c C-c") 'go-errcheck)))
          )
  :ensure t)

(use-package go-snippets
  
  :ensure t)

;  go-autocomplete    20140527.... available  auto-complete-mode backend for go-mode
;  go-direx           20140303.447 available  Tree style source code viewer for Go language
;  go-eldoc           20140608.... available  eldoc for go-mode
;  go-errcheck        20140107.... available  errcheck integration for go-mode
;  go-play            20120914.... available  Paste to play.golang.org
;  go-projectile      20140603.... available  Go add-ons for Projectile
;  go-snippets        20130821.844 available  Yasnippets for go
;  god-mode           20140608.252 available  God-like command entering minor mode
;  gold-mode          20140606.... available  Major mode for editing .gold files
;  golden-ratio       20130921.144 available  Automatic resizing of Emacs windows to the golden ratio
;  golint             20140122.... available  lint for the Go source code
