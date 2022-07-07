;; Configuração básica do Emacs
(setq inhibit-startup-message t)     ; Adeus, buffer assustador!

(tool-bar-mode   -1)                 ; Oculta a barra de ferramentas
(menu-bar-mode   -1)                 ; Oculta a barra de menu
(scroll-bar-mode -1)                 ; Oculta a barra de rolagem
(tooltip-mode    -1)                 ; Oculta dicas

(global-display-line-numbers-mode t) ; Exibe numeração de linhas
(column-number-mode t)               ; Exibe coluna atual na modeline
(global-hl-line-mode t)              ; Exibe destaque de linha

;; Alertas visuais
(setq visible-bell t)

;; Espaçamento das bordas laterais
(set-fringe-mode 10)

;; Ajustes para a minha sanidade mental
(global-unset-key (kbd "C-z")) ; Inibe Ctrl-Z (suspend frame)
(delete-selection-mode t)      ; O texto digitado substitui a seleção

;; Rolagem mais suave
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1)) ; 2 linhas por vez
      mouse-wheel-progressive-speed nil            ; Não acelera a rolagem
      mouse-wheel-follow-mouse 't                  ; Rola a janela sob o mouse
      scroll-step 1)                               ; Rola 1 linha com teclado

;; Quebras de linha
(global-visual-line-mode t)

;; Definição temporária do tema
(load-theme 'tango-dark t)

;; Tipo de cursor (box, bar ou hbar)
(setq-default cursor-type 'box)

;; Fonte padrão
(set-face-attribute 'default nil :font "Roboto Mono" :height 120)

;; Função para criar um novo buffer
(defun debmx-new-buffer ()
  "Cria um novo buffer `sem nome'."
  (interactive)
  (let ((debmx/buf (generate-new-buffer "sem-nome")))
    (switch-to-buffer debmx/buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    debmx/buf))
;; Modo inicial
(setq initial-major-mode 'prog-mode)
(setq initial-buffer-choice 'debmx-new-buffer)

;; Organizando os backups
(setq backup-directory-alist `(("." . "~/.saves")))

;; Verifica e inicia o package.el
(require 'package)
;; Definição de repositórios
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")))
;; Inicialização do sistema de pacotes
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

; Instalação do use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
  
(require 'use-package)
(setq use-package-always-ensure t)

;; Instalação do auto-update

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "21:00"))






(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ispell-dictionary "brasileiro")
 '(package-selected-packages '(auto-package-update use-package cmake-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
