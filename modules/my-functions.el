(defun niri-babel-build-and-deploy ()
  "Tangle and deploy config.kdl to ~/.config/niri/config.kdl with 5 rotating backups."
  (interactive)
  (let* ((org-file "~/projects/niri_babel_config/niri_config.org")
         (output-dir "~/projects/niri_babel_config/")
         (output-file (expand-file-name "config.kdl" output-dir))
         (target-file "~/.config/niri/config.kdl"))
    
    ;; Execute all non-KDL blocks first
    (with-current-buffer (find-file-noselect org-file)
      (org-babel-map-src-blocks org-file
        (let* ((info (org-babel-get-src-block-info 'light))
               (lang (nth 0 info)))
          (unless (string= lang "kdl")
            (org-babel-execute-src-block))))
      ;; Tangle everything
      (org-babel-tangle))

    ;; Backup rotation (keep last 5)
    (when (file-exists-p target-file)
      ;; Shift old backups
      (dotimes (i 5)
        (let* ((n (- 5 i))
               (old (format "%s.%03d" target-file n))
               (new (format "%s.%03d" target-file (1+ n))))
          (when (file-exists-p old)
            (rename-file old new t))))
      ;; Save current file as .001
      (copy-file target-file (format "%s.001" target-file) t))

    ;; Deploy new config
    (when (file-exists-p output-file)
      (copy-file output-file target-file t)
      (message "Tangled and deployed config.kdl to %s" target-file))))

(defun emacs-babel-build-and-deploy ()
  "Tangle and deploy Emacs config to proper env directory with backup and timestamp."
  (interactive)
  (let* ((target-env "emacs-prod")  ;; Assumes `target-env` is a custom function
         (org-file "~/projects/emacs_babel_config/emacs_config.org")
         (modules-dir (expand-file-name (format "~/.config/%s/modules" target-env)))
         (src-dir (expand-file-name "~/projects/emacs_babel_config/modules/"))
         (timestamp-file (expand-file-name
                          (format "~/.config/%s/last_deployed.org" target-env))))

    ;; Debug
    (message "Target env: %s" target-env)

    ;; Run non-Elisp blocks to update values
    (with-current-buffer (find-file-noselect org-file)
      (org-babel-map-src-blocks org-file
        (let* ((info (org-babel-get-src-block-info 'light)))
          (when info
            (let ((lang (nth 0 info)))
              (unless (string= lang "emacs-lisp")
                (org-babel-execute-src-block))))))

    ;; Tangle all blocks
    (org-babel-tangle)

    ;; Copy modules
    (when (file-directory-p src-dir)
      (make-directory modules-dir t)
      (dolist (file (directory-files src-dir t "^[^.].*"))  ; skip dotfiles
        (copy-file file
                   (expand-file-name (file-name-nondirectory file) modules-dir)
                   t)))

    ;; Write timestamp
    (with-temp-file timestamp-file
      (insert (format "* Last Deployed\n\nDeployed at: %s\n" (current-time-string))))

    (message "Emacs config deployed to %s" modules-dir))))

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

(defun open-timeshift-backup ()
  "Open already-mounted Timeshift backup in Dired."
  (interactive)
  (let ((mount-point "/mnt/timeshift"))
    (if (file-directory-p mount-point)
        (dired mount-point)
      (message "Mount point does not exist or is not accessible: %s" mount-point))))
