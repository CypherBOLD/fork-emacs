;; Disable startup screen
(setq inhibit-startup-message t)

(scroll-bar-mode -1)    ; Disable visible scrollbar
(tool-bar-mode -1)      ; Disable toolbar
(tooltip-mode -1)       ; Disable tooltips
(set-fringe-mode 0)     ; No side paddings!
(menu-bar-mode -1)      ; Disable menu bar

;; Use visual bell
(setq visible-bell t)

;; Scroll settings
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))  ; one line at a time
(setq mouse-wheel-progressive-speed t)               ; accelerate scrolling
(setq mouse-wheel-follow-mouse 't)                   ; scroll window under mouse
(setq scroll-step 1)                                 ; keyboard scroll one line at a time

;; Disable Ctrl-Z (suspend frame)
(global-unset-key (kbd "C-z"))

;; Line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
		shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elap.gnu.org/packages/")))

;; Initialize package system
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Check for use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Initialize use-package
(require 'use-package)
(setq use-package-always-ensure t)


;; Sends keystrokes to a designated buffer (good for presentations!)
(use-package command-log-mode)

;; Remove abbreviate minor mode indicators in modeline
(use-package diminish)

;; An interactive interface for command completion
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; An even more friendly interface for ivy and counsel
(use-package ivy-rich
  :init (ivy-rich-mode 1))

;; Completion framework for ivy
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))

;; More helpful help info
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))


;; Modeline style from doom-emacs
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-height 30))

;; Themes from doom-emacs
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-palenight t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-palenight") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Set different colors for different delimiters levels
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Displays the key bindings following currently entered incomplete prefixes 
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; (use-package general
;;   :config
;;   (general-create-definer global-definer
;;     :keymaps 'override
;;     :states  '(insert normal insert visual emacs operator motion hybrid)
;;     :prefix  "SPC"
;;     :global-prefix "C-SPC")
;;   (global-definer
;;     "t"  '(:ignore t :which-key "toggles")
;;     "tt" '(counsel-load-theme :which-key "chose theme")
;;     "!"  'shell-command
;;     ":"  'eval-expression))

;; Moves lines and words
(use-package drag-stuff)
(drag-stuff-global-mode 1)
(drag-stuff-define-keys)

;; A more convenient method for binding keys
(use-package general)

;; More key bindings
(general-define-key
  "C-M-<down>" 'counsel-switch-buffer
  "<escape>"   'keyboard-escape-quit
  "M-#"        'comment-or-uncomment-region)




;; Font face
(set-face-attribute 'default nil :font "Inconsolata" :height 130)
(set-face-attribute 'mode-line nil :font "Inconsolata" :height 100)

;; Did this come with doom-themes?!
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(ispell-dictionary "brasileiro")
 '(ivy-mode t)
 '(package-selected-packages
   '(drag-stuff general helpful ivy-rich which-key rainbow-delimiters doom-themes doom-modeline use-package diminish counsel command-log-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
