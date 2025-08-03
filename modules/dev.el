(use-package typescript-mode
  :mode "\\.ts\\'")

(use-package python-mode
  :hook (python-mode . eglot-ensure))

(use-package pyvenv
  :config (pyvenv-mode 1))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Documents/Code")
    (setq projectile-project-search-path '("~/Documents/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package request
:ensure t)

(require 'request)
(require 'json)

;; ChatGPT AI integration.
(setq chatgpt-shell-save-session t)
(global-set-key (kbd "C-c g") #'chatgpt-shell)

(setq chatgpt-shell-openai-key "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")

(setq chatgpt-shell-anthropic-key "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")

(setq chatgpt-shell-google-key "AIzaSyDkF0ZNBH208yLG48x_iv6kDVk0pjo6g3E")
