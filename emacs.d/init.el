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

;; Typed text replaces the selection and delete (not kill)
;; the highlighted region by pressing DEL
(delete-selection-mode t)

;; Line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
( hl-line-mode t)


;; Indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; Recent files
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

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
			             ("elpa" . "https://elpa.gnu.org/packages/")))

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
  "M-#"        'comment-or-uncomment-region
  "C-x C-r"    'recentf-open-files
  "C-x w"      'elfeed)

;; Corfu completion
(use-package corfu
  :init
  (corfu-global-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

;; Dabbrev works with Corfu
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete))

;; --- Recomendações do Vinicius ---

;; Ícones no dired
(use-package all-the-icons-dired)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; Cliente RSS
(use-package elfeed)
(setq elfeed-feeds
      '("http://www.dicas-l.com.br/index.xml"
        "https://elias.praciano.Languages/com/"))

;; PHP mode
(use-package php-mode)

;; Markdown mode
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Edit indirect
(use-package edit-indirect)

;; Dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))
(setq dashboard-startup-banner "~/.emacs.d/emacs-debxp.txt")
(setq dashboard-center-content t)
(setq dashboard-show-shortcuts t)
(setq dashboard-items '((bookmarks . 10)
                        (projects . 10)
		                (recents   . 5)))
;; Projectile
(use-package projectile
  :init (projectile-mode))

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Font face
(set-face-attribute 'default nil :font "Inconsolata" :height 130)
(set-face-attribute 'mode-line nil :font "Inconsolata" :height 100)
(set-face-attribute 'fixed-pitch nil :font "Inconsolata" :height 130)
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 130 :weight 'regular)


;; Did this come with doom-themes?!
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(elfeed-feeds
   '("http://www.dicas-l.com.br/index.xml" "https://elias.praciano.com/feed/"))
 '(ispell-dictionary "brasileiro")
 '(ivy-mode t)
 '(package-selected-packages
   '(projectile dashboard edit-indirect markdown-mode elfeed all-the-icons-dired all-the-icons-dired-mode orderless corfu php-mode drag-stuff general helpful ivy-rich which-key rainbow-delimiters doom-themes doom-modeline use-package diminish counsel command-log-mode))
 '(recentf-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
