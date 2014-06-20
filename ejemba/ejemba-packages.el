(defvar ejemba-packages
  '( deft markdown-mode )
  "A list of packages to ensure are installed at launch.")

(defun ejemba-packages-installed-p ()
  (loop for p in ejemba-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)) 
  (return nil))

(unless (ejemba-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs Ejemba is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p ejemba-packages)
    (when (not (package-installed-p p))
      (message "Prepare to install %s" p)
      (package-install p))))

(provide 'ejemba-packages)
