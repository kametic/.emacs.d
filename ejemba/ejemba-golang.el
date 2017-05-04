;;; ejemba-golang.el --- Elisp Golang configuration

;;; Commentary:
;;

;;; Code:




(use-package go-mode
  :mode ("\\.go" . go-mode)
  :config (progn
            
            (add-hook 'go-mode-hook
                      (lambda ()
                        ( add-hook 'before-save-hook 'gofmt-before-save)
                        ;( add-hook 'before-save-hook 'indent-region)
                        )
                      )

            ;; -- Use the style of `gofmt -tabs=false -tabwidth=2` to format code
            (add-hook 'go-mode-hook (lambda ()
                                      (setq tab-width 3)
                                      (setq standard-indent 3)
                                      (setq indent-tabs-mode nil) ))



            (setq gofmt-command "/home/ejemba/.emacs.d/gotools/bin/goimports")

            

            ;(setq gofmt-command (cond
            ;                     ((executable-find "goimports")
            ;                      "goimports")
            ;                     (t "gofmt")))

            ;(setq gofmt-args "-tabs=false -tabwidth=2" )
            
            

            (add-hook 'go-mode-hook 'rats-mode)
            (add-hook 'go-mode-hook 'flycheck-mode)
            (add-hook 'go-mode-hook 'yas-minor-mode)
            (add-hook 'go-mode-hook 'highlight-symbol-mode)
            (add-hook 'go-mode-hook 'highlight-symbol-nav-mode)

            (add-hook 'go-mode-hook
                      (lambda () (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))
            (add-hook 'go-mode-hook
                      (lambda () (local-set-key (kbd "C-<tab>") 'ac-complete-yasnippet)))
            
            (add-hook 'go-mode-hook
                      (lambda () (local-set-key (kbd "C-c i") 'go-goto-imports)))

            (add-hook 'go-mode-hook
                      (lambda () (local-set-key (kbd "C-<f3>") 'godef-jump)))

;            (add-hook 'go-mode-hook
;                      (lambda () (local-set-key (kbd "C-c C-c") 'go-errcheck)))

            (fset 'ejemba/go/topointer
                  [C-right C-left ?\( ?* C-right ?\)])

            
            (add-hook 'go-mode-hook
                      (lambda () (local-set-key (kbd "C-c C-p") 'ejemba/go/topointer)))


                                        ; ejemba/go/topointer
            (add-hook 'go-mode-hook
                      (lambda () (progn (set (make-local-variable 'compile-command)
                                             "go generate && go build -v && go test -v && go vet") )))

                                        ;
            
            (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/yasnippet-golang")
            
            )
            
            

  :ensure t)


(prodigy-define-tag
              :name 'goconvey
              :command "goconvey"
              :kill-signal 'sigkill
              :kill-process-buffer-on-stop t

              :ready-message "Serving HTTP at")

;; Go Oracle
;; http://yousefourabi.com/blog/2014/05/emacs-for-go/
;;
(load-file "~/.emacs.d/gotools/src/golang.org/x/tools/cmd/oracle/oracle.el")

(use-package go-projectile :ensure t)

;(use-package flycheck-gometalinter :ensure t)

;; go get -u github.com/nsf/gocode
(use-package go-eldoc
  :init (add-hook 'go-mode-hook 'go-eldoc-setup)
  :ensure t)

(use-package go-autocomplete
  :ensure t)

(use-package go-snippets
  :ensure t)

; add go rename
(load-file "~/.emacs.d/gotools/src/golang.org/x/tools/refactor/rename/rename.el")
;(add-to-list 'load-path "~/.emacs.d/gotools/src/golang.org/x/tools/refactor/rename/")



;; go get github.com/kisielk/errcheck
(use-package go-errcheck   :ensure t)

; go direx
;(use-package popwin ; needed by direx
;  :init (popwin-mode 1)
;  :ensure t)
;;; go get -u github.com/jimweirich/gotags
;;; use rake
;(use-package go-direx
;  :init
;  :ensure t)

; go get github.com/golang/lint/golint
(use-package golint
  :ensure t)

;(use-package go-snippets :ensure t)

(global-flycheck-mode t)
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

(add-to-list 'projectile-globally-ignored-directories "pkg")
(add-to-list 'projectile-globally-ignored-directories "bin")
(add-to-list 'projectile-globally-ignored-directories "src/.git")
(add-to-list 'projectile-globally-ignored-directories "src/vendor")
(add-to-list 'projectile-globally-ignored-file-suffixes "*.a")
(add-to-list 'projectile-globally-ignored-file-suffixes "*.test")
(add-to-list 'projectile-globally-ignored-files "dslgen")

(add-to-list 'grep-find-ignored-directories "pkg")
(add-to-list 'grep-find-ignored-directories "bin")
(add-to-list 'grep-find-ignored-directories "src/.git")
(add-to-list 'grep-find-ignored-directories "src/vendor")
(add-to-list 'grep-find-ignored-files "*.a")
(add-to-list 'grep-find-ignored-files "*.test")
(add-to-list 'grep-find-ignored-files "dslgen")


(provide 'ejemba-golang)

;;; ejemba-golang.el ends here
