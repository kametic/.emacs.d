(defun ejemba/syntax-color-hex ()
  "Syntax color hex color spec such as 「#ffdead」 in current buffer."
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[abcdefABCDEF[:digit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :foreground (match-string-no-properties 0)))))))
  (font-lock-fontify-buffer)
  )

;C-a			;; 
;TAB			;; 

(defun ejemba/tab ()
  "Tabulation"
  (interactive)
  (move-beginning-of-line 1)
  (indent-for-tab-command)
  )

(defun ejemba/move-line-up ()
  "Move the current line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun ejemba/move-line-down ()
  "Move the current line down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))


(defun ejemba/open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun ejemba/open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(defun split-window-prefer-horizonally (window)
  "If there's only one window (excluding any possibly active
         minibuffer), then split the current window horizontally."
  (split-window-right window)
;  (if (and (one-window-p t)
;           (not (active-minibuffer-window)))
;      (let ((split-height-threshold nil))
;        (split-window-sensibly window))
;    (split-window-sensibly window))
  )


(require 'hydra)
(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("=" text-scale-increase "in")
  ("-" text-scale-decrease "out")
  ; left right up down
  ("k" left-char "Left")
  ("m" right-char "Right")
  ("o" previous-line "Up")
  ("l" next-line   "Down")
  ("<f2>" nil "cancel")
  )

(require 'hydra-examples)
(defhydra hydra-splitter (global-map "<f5>")
  "slitter"
  ("h" hydra-move-splitter-left "<-")
  ("j" hydra-move-splitter-down "down")
  ("k" hydra-move-splitter-up "up")
  ("l" hydra-move-splitter-right "->"))




(global-set-key
 (kbd "C-M-o")
 (defhydra hydra-window ()
   "window"
   ("h" windmove-left)
   ("j" windmove-down)
   ("u" windmove-up)
   ("k" windmove-right)
   ("a" (lambda ()
          (interactive)
          (ace-window 1)
          (add-hook 'ace-window-end-once-hook
                    'hydra-window/body))
        "ace")
   ("v" (lambda ()
          (interactive)
          (split-window-right)
          (windmove-right))
        "vert")
   ("x" (lambda ()
          (interactive)
          (split-window-below)
          (windmove-down))
        "horz")
   ("s" (lambda ()
          (interactive)
          (ace-window 4)
          (add-hook 'ace-window-end-once-hook
                    'hydra-window/body))
        "swap")
   ("d" (lambda ()
          (interactive)
          (ace-window 16)
          (add-hook 'ace-window-end-once-hook
                    'hydra-window/body))
        "del")
   ("o" delete-other-windows "1" :color blue)
   ("i" ace-maximize-window "a1" :color blue)
   ("q" nil "cancel")))



(defun switch-to-previous-buffer ()
      (interactive)
      (switch-to-buffer (other-buffer (current-buffer) 1)))

;(setq split-window-preferred-function 'split-window-prefer-horizonally)

;replacing (split-window-sensibly) ;

(bind-key "<M-up>" 'ejemba/move-line-up)
(key-chord-define-global "UU" 'ejemba/move-line-up)

(bind-key "<M-down>" 'ejemba/move-line-down)
(key-chord-define-global "DD" 'ejemba/move-line-down)

(global-set-key (kbd "<C-return>") 'ejemba/open-line-below)
(global-set-key (kbd "<C-S-return>") 'ejemba/open-line-above)

(bind-key "<C-d>" 'kill-whole-line)
(key-chord-define-global "--" 'kill-whole-line)

(bind-key "TAB" 'ejemba/tab)
(bind-key "C-TAB" 'switch-to-previous-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun epo-run-current-file ()
  "Execute the current file.
For example, if the current buffer is the file xx.py,
then it'll call “python xx.py” in a shell.
The file can be php, perl, python, ruby, javascript, bash, ocaml, vb, elisp.
File suffix is used to determine what program to run.

If the file is modified, ask if you want to save first.

If the file is emacs lisp, run the byte compiled version if exist."
  (interactive)
  (let* (
         (suffixMap
          `(
            ("php" . "php")
            ("pl" . "perl")
            ("py" . "python")
            ("py3" . ,(if (string-equal system-type "windows-nt") "c:/Python32/python.exe" "python3"))
            ("rb" . "ruby")
            ("js" . "node")             ; node.js
            ("sh" . "bash")
            ("ml" . "ocaml")
            ("go" . "go build")
            )
          )
         (fName (buffer-file-name))
         (fSuffix (file-name-extension fName))
         (progName (cdr (assoc fSuffix suffixMap)))
         (cmdStr (concat progName " \""   fName "\""))
         )
    
    (when (buffer-modified-p)
      (when (y-or-n-p "Buffer modified. Do you want to save first?")
          (save-buffer) ) )
    
    (if (string-equal fSuffix "el") ; special case for emacs lisp
        (load (file-name-sans-extension fName))
      (if progName
          (progn
            (message "Running…")
            (shell-command cmdStr "*xah-run-current-file output*" )
            )
        (message "No recognized program file suffix for this file.")
        ) ) ))
