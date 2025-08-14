(use-package dired
  :straight (:type built-in)
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump)))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; Launch apps based on content.
(use-package dired-open
  :config
  (setq dired-open-extensions
	'(("png" . "imv")
	  ("jpg" . "imv")
	  ("pdf" . "zathura")
	  ("mp4" . "mpv")
	  ("mkv" . "mpv")
	  ("html" . "floorp"))))

;; Bind enter to launch associated file app.
(with-eval-after-load 'dired
;; Replace RET behavior
(define-key dired-mode-map (kbd "RET") #'dired-open-file))


(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode))

;; Must have dired extensions
(use-package peep-dired
  :ensure t
  :bind (:map dired-mode-map
              ("P" . peep-dired))
  :hook (peep-dired-mode . (lambda () (setq-local image-dired-display-image-buffer 'other))))

(use-package dired-subtree
  :ensure t
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)))

(use-package vterm
  :commands vterm
  :bind ("C-c v" . vterm)
  :config
  (setq vterm-shell "/usr/bin/fish")
  (setq vterm-max-scrollback 10000))
