(load-file "~/Apps/meghanada-emacs/meghanada.el")
;; optional
(load-file "~/Apps/meghanada-emacs/company-meghanada.el")
;; optional
(load-file "~/Apps/meghanada-emacs/flycheck-meghanada.el")

(require 'meghanada)
(require 'company-meghanada)
(require 'flycheck-meghanada)

(add-hook 'meghanada-mode-hook
  (lambda ()
    (add-to-list 'company-backends '(company-meghanada :with company-dabbrev-code))
    (setq company-transformers '(company-sort-by-backend-importance))
    (add-hook 'before-save-hook 'delete-trailing-whitespace)))

(add-to-list 'auto-mode-alist '("\\.java\\'" . meghanada-mode))
