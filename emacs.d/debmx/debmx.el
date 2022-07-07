;;; Code: debmx custom functions

(defun debmx/new-buffer ()
  "Create a new untitled buffer."
  (interactive)
  (let ((debmx/buf (generate-new-buffer "untitled")))
    (switch-to-buffer debmx/buf)
    (fundamental-mode)
    (setq buffer-offer-save t)
    debmx/buf))

(defun debmx/open-init-file ()
  "Open user init file."
  (interactive)
  (find-file user-init-file))

(defun debmx/duplicate-line ()
  "Duplicate current line and keep it in kill ring."
  (interactive)
  (kill-whole-line)
  (yank)
  (yank))

(defun debmx/duplicate-region ()
  "Duplicate selected region and keep it in kill ring."
  (interactive)
  (copy-region-as-kill (region-beginning) (region-end) nil)
  (open-line 1)
  (forward-line)
  (yank))


;; from: https://emacs.stackexchange.com/a/34307
(defun debmx/move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

;; from: https://emacs.stackexchange.com/a/34307
(defun debmx/move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

;; not a debmx function, but useful anyway
(defun wc ()
  "Count words in current buffer."
  (interactive)
  (shell-command (concat "echo $(wc -w " buffer-file-name " | cut -d' ' -f1) words")))

;; org mode defaults
(defun debmx/defaults-org ()
  "Defaults for org mode."
  (org-indent-mode)
  (auto-fill-mode 0)
  (display-line-numbers-mode -1))

;; visual fill column defaults
(defun debmx/visual-fill-column-defaults ()
  (setq visual-fill-column-width 80
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(provide 'debmx)
;;; debmx.el ends here
