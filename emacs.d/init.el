;; Remove "obsolete cl" warning
(setq byte-compile-warnings '(cl-functions))

;; Variables
(setq inhibit-startup-message t                    ; no welcome buffer
      visible-bell t                               ; blinks at cursor limits
      history-length 20                            ; max history saves
      use-dialog-box nil                           ; no ugly dialogs
      global-auto-revert-non-file-buffers t        ; update buffers thar are non-files too
      tab-always-indent 'complete                  ; use TAB to complete symbols
      mouse-wheel-scroll-amount '(2 ((shift) . 1)) ; scroll 2 lines
      mouse-wheel-progressive-speed nil            ; don't accelerate
      mouse-wheel-follow-mouse 't                  ; scroll window under mouse cursor
      scroll-step 1)                               ; scroll 1 line with keyboard

(setq x-select-enable-clipboard-manager nil)

;; Modes
(menu-bar-mode -1)                      ; no menu bar
(tool-bar-mode -1)                      ; no tools bar
(scroll-bar-mode -1)                    ; no scroll bars
(set-fringe-mode 10)                    ; frame edges set to 10px
(column-number-mode 1)                  ; show current column in modeline
(recentf-mode 1)                        ; remember recent files
(save-place-mode 1)                     ; remember cursor position
(savehist-mode 1)                       ; enable history saving
(delete-selection-mode t)               ; overwrite selected text when typing
(global-hl-line-mode 1)                 ; enable current line highlight
(global-visual-line-mode t)             ; visual line breaking
(global-auto-revert-mode 1)             ; update externaly edited files
(global-display-line-numbers-mode 1)    ; always show line numbers

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Enable flyspell for text-mode buffers
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

;; Disable C-z
(global-unset-key (kbd "C-z"))

;; Activate new window
(global-set-key "\C-x2" (lambda ()
              (interactive)
              (split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda ()
              (interactive)
              (split-window-horizontally) (other-window 1)))

;; Enable case changes (C-x C-u and C-x C-l)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Backups dir
(setq backup-directory-alist `(("." . ,(expand-file-name "saves/" user-emacs-directory))))

;; Custom settings file
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror 'nomessage)

;; Load my functions
(load "~/.emacs.d/debmx/debmx-faces.el")
(load "~/.emacs.d/debmx/debmx.el")
(load "~/.emacs.d/debmx/debmx-map.el")

;; Default fonts
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (debmx/set-font-faces))))
    (debmx/set-font-faces))

;; package system initialization
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
             ("org"   . "https://orgmode.org/elpa/")
             ("elpa"  . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Package 'use-package'
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Package 'modus-themes'
(use-package modus-themes)
(setq modus-themes-mode-line '(accented borderless)
      modus-themes-region '(bg-only)
      modus-themes-paren-match '(bold intense underline)
      modus-themes-headings '((t . (rainbow bold)))
      modus-themes-org-blocks 'gray-background)
;; Load Modus Theme
(load-theme 'modus-vivendi t)

;; Package 'rainbow-delimiters'
(use-package rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'text-mode-hook 'rainbow-delimiters-mode)

;; Package 'diminish'
(use-package diminish)
(diminish 'visual-line-mode "")
(diminish 'eldoc-mode "")
(diminish 'flyspell-mode "Spell")

;; Package 'smartparens
(use-package smartparens
  :ensure t
  :init
  (require 'smartparens-config)
  (smartparens-global-mode t)
  :diminish smartparens-mode
  :config
  (show-smartparens-mode t))

;; Package 'beacon'
(use-package beacon
  :diminish beacon-mode
  :config (beacon-mode 1))

;; Package 'which-key'
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3)
  (setq which-key-show-early-on-C-h t))

;; Package 'company'
(use-package company
  :diminish company-mode
  :hook (after-init . global-company-mode))

;; Package 'vertico'
(use-package vertico
  :init (vertico-mode)
  :config (setq vertico-cycle t))

;; Package 'marginalia'
(use-package marginalia
  :init (marginalia-mode))

;; Package 'drag-stuff'
(use-package drag-stuff
  :init (drag-stuff-global-mode))

;; Package 'magit'
(use-package magit)

;; Package 'flycheck'
(use-package flycheck
  :ensure t
  :hook (prog-mode-hook . flycheck-mode))

;; Package 'sly'
(use-package sly
  :config (setq inferior-lisp-program "/usr/bin/sbcl"))

;; Package 'markdown-mode'
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

;; Package 'php-mode'
(use-package php-mode
  :mode ("\\.php\\'" . php-mode))

;; Package 'web-mode'
(use-package web-mode
  :mode ("\\.phtml\\.tpl\\.html\\.twig\\.html?\\'" . web-mode))

;; Package 'rainbow-mode'
(use-package rainbow-mode)


;; org-mode (start) ---------------------------------------------

(use-package org
  :ensure org-plus-contrib
  :hook (org-mode . debmx/defaults-org)
  :config
  (setq org-support-shift-select t
        org-hide-emphasis-markers t
        org-ellipsis " ▾"
        org-babel-default-header-args '((:results . "output"))
        org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((awk . t)
     (css . t)
     (emacs-lisp . t)
     (lisp . t)
     (lua . t)
     (shell . t)
     (php . t)
     (C . t))))

(use-package visual-fill-column
  :hook ('org-mode-hook . 'debmx/visual-fill-column-defaults))

(require 'org-indent)
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("awk" . "src awk"))
(add-to-list 'org-structure-template-alist '("css" . "src css"))
(add-to-list 'org-structure-template-alist '("lua" . "src lua"))
(add-to-list 'org-structure-template-alist '("php" . "src php"))
(add-to-list 'org-structure-template-alist '("sh"  . "src shell"))
(add-to-list 'org-structure-template-alist '("cfg" . "src unix-config"))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "●" "●" "●" "●")))

;; Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; org-mode (end) -----------------------------------------------

;; Append auto-modes
(setq auto-mode-alist
      (append '(("\\.bash_aliases\\'" . sh-mode)
                ("\\.cl\\'" . common-lisp-mode)) auto-mode-alist))

(provide 'init)
;;; init.el ends here

