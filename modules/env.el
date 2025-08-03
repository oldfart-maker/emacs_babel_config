(setq my/env "emacs-prod")
(setq server-name "emacs-prod")

(require 'server)
(unless (server-running-p)
   (server-start))

;; Turn of eval protection
(setq org-confirm-babel-evaluate nil)
