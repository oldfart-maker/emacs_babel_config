(defun niri-babel-build-and-deploy ()
  "Execute all necessary blocks (except kdl), tangle, and deploy to ~/.config/niri/config.kdl"
  (interactive)
  (let* ((org-file "~/projects/niri_babel_config/niri_config.org")
         (output-dir "~/projects/niri_babel_config/")
         (output-file (expand-file-name "config.kdl" output-dir))
         (target-file "~/.config/niri/config.kdl"))
    (with-current-buffer (find-file-noselect org-file)
      ;; Execute only non-KDL blocks
      (org-babel-map-src-blocks org-file
        (let* ((info (org-babel-get-src-block-info))
               (lang (nth 0 info)))
          (unless (string= lang "kdl")
            (org-babel-execute-src-block))))
      ;; Tangle everything
      (org-babel-tangle)
      ;; Copy the output
      (when (file-exists-p output-file)
        (copy-file output-file target-file t)
        (message "Tangled and copied config.kdl to %s" target-file)))))

(require 'image-dired)

(defun my/image-dired-copy-and-exit ()
  "Copy image under point in image-dired and exit Emacsclient."
  (interactive)
  (let* ((file (image-dired-original-file-name))
         (copy-prog (or (executable-find "wl-copy")
                        (executable-find "xclip"))))
    (unless copy-prog
      (error "No clipboard utility (wl-copy or xclip) found"))
    (unless (and file (file-exists-p file))
      (error "No image found under cursor"))
    (with-temp-buffer
      (insert-file-contents-literally file)
      (call-process-region
       (point-min) (point-max)
       copy-prog nil nil nil "-t" "image/png"))
    (save-buffers-kill-terminal)))

(with-eval-after-load 'image-dired
  ;; `m` to copy and exit
  (define-key image-dired-thumbnail-mode-map (kbd "m") #'my/image-dired-copy-and-exit)
  ;; `q` to just quit
  (define-key image-dired-thumbnail-mode-map (kbd "q")
    (lambda ()
      (interactive)
      (save-buffers-kill-terminal))))

(defun my/image-picker-thumbnail-mode ()
  "Launch thumbnail-only image picker. Press `m` to copy & exit."
  (interactive)
  (let ((dir "~/Pictures/screenshots/"))
    ;; Save current window configuration, run image-dired
    (image-dired dir)
    ;; Force delete all windows except the one showing *image-dired*
    (let ((image-buffer "*image-dired*"))
      (dolist (win (window-list))
        (unless (eq (window-buffer win) (get-buffer image-buffer))
          (delete-window win)))
      (select-window (get-buffer-window image-buffer)))))

;; Show the server name that this emacsclient is connected to.
(defun show-current-server-name ()
  "Display the name of the Emacs server this client is connected to."
  (interactive)
  (message "Connected to Emacs server: %s" server-name))

;; Then bind it in the startup hook
(add-hook 'emacs-startup-hook
          (lambda ()
            (global-set-key (kbd "<f12>") #'show-current-server-name)))

;; Output niri-windows to new buffer
(defun niri-windows ()
  "Show Niri windows in a new buffer."
  (interactive)
  (let ((buf (get-buffer-create "*niri-windows*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (call-process "~/projects/niri_toolkit/niri-windows.py" nil buf)
      (goto-char (point-min))
      (read-only-mode 1))
    (pop-to-buffer buf)))

;;Output niri-event-stream via IPC to new buffer
(defun niri-event-stream ()
  "Show the Niri event stream in a new buffer."
  (interactive)
  (let ((buf (get-buffer-create "*Niri Event Stream*")))
    (apply 'make-comint-in-buffer
           "Niri Event Stream"
           buf
           (expand-file-name "~/projects/niri_toolkit/niri-tail-event-stream.py")
           nil)
    (pop-to-buffer buf)))
