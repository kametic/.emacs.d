; Proxy configuration
; http://stackoverflow.com/questions/10787087/use-elpa-emacs-behind-proxy-requiring-authentication
; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=12069

;(setq url-proxy-services '(("no_proxy" . "inetpsa\\.com")
;                           ("http" . "http.internetpsa.inetpsa.com:80")
;                           ("https" . "http.internetpsa.inetpsa.com:80"))
;)
;
;(setq url-http-proxy-basic-auth-storage
;      (list (list "http.internetpsa.inetpsa.com:80"
;		  (cons "Input your LDAP UID !"
;			(base64-encode-string "e364467:xxxxx")))))
