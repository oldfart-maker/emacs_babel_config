;; Font sizing defaults for UI scaling (override per-host if needed)
     (defvar my/default-font-size 100)
     (defvar my/default-variable-font-size 100)

     ;; Frame transparency defaults
     (defvar my/frame-transparency '(90 . 90))

     ;; Disable unnecessary UI elements
     (scroll-bar-mode -1)
     (tool-bar-mode -1)
     (tooltip-mode -1)
     (menu-bar-mode -1)
     (set-fringe-mode 10)

     ;; Set up the visible bell
     (setq visible-bell t)

     ;; Show column and line numbers
     (column-number-mode)
     (global-display-line-numbers-mode t)

     ;; Set frame font and theme
     (set-face-attribute 'default nil :font "JetBrains Mono" :height my/default-font-size)
     (set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height my/default-font-size)
     (set-face-attribute 'variable-pitch nil :font "Cantarell" :height my/default-variable-font-size :weight 'regular)

     ;; Apply frame transparency
     (set-frame-parameter (selected-frame) 'alpha my/frame-transparency)
     (add-to-list 'default-frame-alist `(alpha . ,my/frame-transparency))

     ;; Icons and modeline
     (use-package all-the-icons)

     (use-package doom-themes
     :init (load-theme 'doom-palenight t))

     (use-package doom-modeline
       :init (doom-modeline-mode 1)
       :custom ((doom-modeline-height 15)))

     ;; Shows window numbers to select to change window
     (use-package ace-window
     :ensure t
     :bind (("M-o" . ace-window)))

     ;; Focus follows mouse
   (setq mouse-autoselect-window t)

    ;; Setup window borders like wtm
   (window-divider-mode 1)
   (setq window-divider-default-places t)
   (setq window-divider-default-bottom-width 1)
   (setq window-divider-default-right-width 1)

   ;; Set all borders to orange
  (set-face-attribute 'window-divider nil :foreground "orange")
  (set-face-attribute 'vertical-border nil :foreground "orange")

  ;; Mode line borders - also orange
  (set-face-attribute 'mode-line nil
                    :background "#4c566a"
                    :foreground "#eceff4"
                    :box '(:line-width 1 :color "orange"))

  (set-face-attribute 'mode-line-inactive nil
                    :background "#2e3440"
                    :foreground "#88909f"
                    :box '(:line-width 1 :color "orange"))

;;  Window shading - active window much darker
  (defvar my-active-window-background "#000000")    ; Very dark for active
  (defvar my-inactive-window-background "#2a2a2a")  ; Lighter for inactive

  (defun my-apply-window-shading ()
  "Apply shading - active window darker, inactive lighter."
     (dolist (window (window-list))
       (with-current-buffer (window-buffer window)
         (face-remap-reset-base 'default)
         (if (eq window (selected-window))
             ;; Active window - much darker
             (face-remap-add-relative 'default :background my-active-window-background)
           ;; Inactive windows - lighter
           (face-remap-add-relative 'default :background my-inactive-window-background)))))

  ;; Apply shading on window changes
  (add-hook 'window-selection-change-functions 
          (lambda (&rest _) (my-apply-window-shading)))

  ;; Protect settings from being overridden
  (defun my-protect-window-settings (&rest _)
     (when window-divider-mode
       (setq window-divider-default-bottom-width 1)
       (setq window-divider-default-right-width 1))
     (set-face-attribute 'window-divider nil :foreground "orange")
     (set-face-attribute 'vertical-border nil :foreground "orange")
     (my-apply-window-shading))

  (advice-add 'load-theme :after #'my-protect-window-settings)

  ;; Initialize everything
  (my-apply-window-shading)

  ;; End of Window Configuration
  (put 'erase-buffer 'disabled nil)
