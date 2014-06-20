; Proxy configuration
; http://stackoverflow.com/questions/10787087/use-elpa-emacs-behind-proxy-requiring-authentication
; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=12069

;(setq url-proxy-services '(("no_proxy" . "host")
;                           ("http" . "")
;                           ("https" . ""))
;)
;
;(setq url-http-proxy-basic-auth-storage
;      (list (list "httphost:80"
;		  (cons "Input your LDAP UID !"
;			(base64-encode-string "blahblah:xxxxx")))))
