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

;; Organizando os backups
(setq backup-directory-alist `(("." . "~/.saves")))

;; Definição temporária do tema
(load-theme 'tango-dark t)
