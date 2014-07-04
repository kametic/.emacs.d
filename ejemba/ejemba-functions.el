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

(setq split-window-preferred-function 'split-window-prefer-horizonally)

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
