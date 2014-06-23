;;; Zencoding and Emmet

(use-package emmet-mode
  :commands emmet-mode
  :bind ("C-c n e" . emmet-mode)
  :init
  (progn
    (add-hook 'sgml-mode-hook 'emmet-mode)
    (add-hook 'html-mode-hook 'emmet-mode)
    (add-hook 'nxml-mode-hook 'emmet-mode)
    (add-hook 'css-mode-hook 'emmet-mode))
  :ensure t
  )

; add js mode 
; add angular snippet 
