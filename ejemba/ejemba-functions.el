;;; ejemba-function --- blah
;;; Commentary:
;;; Code: blah


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
(defun ejemba/showgotests ()
  "Showing Rats Test Windows in View Mode."
  (interactive)
  (rats-run-test-under-point)
  (view-buffer "*rats-test*"))


(defun ejemba/split-horizontally-other ()
  "Shortcut to split H then other window."
  (interactive)
  (split-window-horizontally)
  (other-window 1)
  )

(defun ejemba/split-vertically-other ()
  "Shortcut to split V then other window."
  (interactive)
  (split-window-vertically)
  (other-window 1)
  )


(defun ejemba/tab ()
  "Tabulation."
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
  "If there's only one window (excluding any possibly active. minibuffer), then split the current window horizontally."
  (split-window-right window)
;  (if (and (one-window-p t)
;           (not (active-minibuffer-window)))
;      (let ((split-height-threshold nil))
;        (split-window-sensibly window))
;    (split-window-sensibly window))
  )


;(require 'hydra)
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



(fset 'ejemba/duplicate
      (lambda (&optional arg) "Keyboard macro."
        (interactive "p") (kmacro-exec-ring-item (quote ([1 11 25 C-return 25] 0 "%d")) arg)))

;; http://rejeep.github.io/emacs/elisp/2010/03/11/duplicate-current-line-or-region-in-emacs.html
(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))



(defun switch-to-previous-buffer ()
      (interactive)
      (switch-to-buffer (other-buffer (current-buffer) 1)))

(setq initial-major-mode (quote text-mode))

(defun xah-new-empty-buffer ()
  "Open a new empty buffer.
URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2015-06-12"
  (interactive)
  (let ((-buf (generate-new-buffer "untitled")))
    (switch-to-buffer -buf)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))


;(setq split-window-preferred-function 'split-window-prefer-horizonally)

;replacing (split-window-sensibly) ;

(bind-key "<M-up>" 'ejemba/move-line-up)
(key-chord-define-global "UU" 'ejemba/move-line-up)

(key-chord-define-global "ww" 'save-buffer )
(key-chord-define-global "NN" 'xah-new-empty-buffer )
(key-chord-define-global "éé" 'duplicate-current-line-or-region )
(key-chord-define-global "aa" 'beginning-of-line-text )
(key-chord-define-global "EE" 'end-of-visual-line )
(key-chord-define-global "MM" 'set-mark-command)

(key-chord-define-global "JJ" 'left-char)
(key-chord-define-global "LL" 'right-char)
(key-chord-define-global "II" 'previous-line)
(key-chord-define-global "KK" 'next-line)

(bind-key "<M-down>" 'ejemba/move-line-down)
(key-chord-define-global "DD" 'ejemba/move-line-down)

(global-set-key (kbd "<C-return>") 'ejemba/open-line-below)
(global-set-key (kbd "<C-S-return>") 'ejemba/open-line-above)

(bind-key "<C-d>" 'ejemba/duplicate)
(key-chord-define-global "kk" 'kill-whole-line)

(bind-key "TAB" 'ejemba/tab)
(bind-key "C-TAB" 'switch-to-previous-buffer)

(bind-key "<menu>" 'ace-jump-char-mode); this absolutly rocks rocks !



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


(defun xah-open-file-at-cursor ()
  "Open the file path under cursor.
If there is text selection, uses the text selection for path.
If the path starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.
Path may have a trailing “:‹n›” that indicates line number. If so, jump to that line number.
If path does not have a file extension, automatically try with “.el” for elisp files.
This command is similar to `find-file-at-point' but without prompting for confirmation.

URL `http://ergoemacs.org/emacs/emacs_open_file_path_fast.html'"
  (interactive)
  (let ((ξpath (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (let (p0 p1 p2)
                   (setq p0 (point))
                   ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
                   (skip-chars-backward "^  \"\t\n`'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\`")
                   (setq p1 (point))
                   (goto-char p0)
                   (skip-chars-forward "^  \"\t\n`'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\'")
                   (setq p2 (point))
                   (goto-char p0)
                   (buffer-substring-no-properties p1 p2)))))
    (if (string-match-p "\\`https?://" ξpath)
        (browse-url ξpath)
      (progn ; not starting “http://”
        (if (string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\'" ξpath)
            (progn
              (let (
                    (ξfpath (match-string 1 ξpath))
                    (ξline-num (string-to-number (match-string 2 ξpath))))
                (if (file-exists-p ξfpath)
                    (progn
                      (find-file ξfpath)
                      (goto-char 1)
                      (forward-line (1- ξline-num)))
                  (progn
                    (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" ξfpath))
                      (find-file ξfpath))))))
          (progn
            (if (file-exists-p ξpath)
                (find-file ξpath)
              (if (file-exists-p (concat ξpath ".el"))
                  (find-file (concat ξpath ".el"))
                (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" ξpath))
                  (find-file ξpath ))))))))))

(key-chord-define-global "OO" 'xah-open-file-at-cursor)
