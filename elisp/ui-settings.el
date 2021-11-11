;; Configurações da interface

(setq inhibit-startup-message t)     ; Adeus, buffer assustador!

(tool-bar-mode -1)                   ; Oculta a barra de ferramentas
(menu-bar-mode -1)                   ; Oculta a barra de menu
(scroll-bar-mode -1)                 ; Oculta a barra de rolagem
(tooltip-mode -1)                    ; Oculta dicas

(global-display-line-numbers-mode t) ; Exibe numeração de linhas
(column-number-mode t)               ; Exibe coluna atual na modeline
(global-hl-line-mode t)              ; Exibe destaque de linha

;; Rolagem mais suave (https://www.emacswiki.org/emacs/SmoothScrolling)

(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))) ; 2 linhas por vez
(setq mouse-wheel-progressive-speed nil)            ; Não acelera a rolagem
(setq mouse-wheel-follow-mouse 't)                  ; Rola a janela sob o mouse
(setq scroll-step 1)                                ; Rola 1 linha com teclado (nos limites)

;; Alertas visuais

(setq visible-bell t)

;; Fonte padrão

(set-face-attribute 'default nil
		    :font "Inconsolata"
		    :height 130)

;; Fonte de largura varável

(set-face-attribute 'variable-pitch nil
		    :font "Cantarell"
		    :height 120
		    :weight 'regular)

;; Fonte de largura fixa

(set-face-attribute 'fixed-pitch nil
		    :font "Inconsolata"
		    :height 130)

;; Espaçamento das bordas laterais

(set-fringe-mode 10)

;; Ajusta as cores das bordas para as cores do tema

(set-face-attribute 'fringe nil
		    :foreground (face-foreground 'default)
		    :background (face-background 'default))

;; Ajustes para a minha sanidade mental

(global-unset-key (kbd "C-z")) ; Desabilita Ctrl-Z (suspend frame)
(delete-selection-mode t)      ; O texto digitado substitui a seleção

;; Quebras de linha

(global-visual-line-mode t)

;; Faz com que a navegação por linhas quebradas aconteça
;; pelas linhas lógicas em vez das linhas visuais quando
;; visual-line-mode está ativado.

;;   (setq line-move-visual nil)

;; Carrega um tema (https://emacsthemes.com/)
;; Tema atom-one-dark: https://github.com/jonathanchu/atom-one-dark-theme

(add-to-list 'custom-theme-load-path "~/gits/emacs/themes/")
(load-theme 'atom-one-dark t)

;; Customizações

(custom-set-faces
;; modeline ativa
 '(mode-line ((t (:box (:color "#181C24" :line-width 5)
		       :height 100
		       :background "#181C24"))))
;; modeline inativa
 '(mode-line-inactive ((t (:box (:color "#383C44" :line-width 5)
				:height 100
				:background "#383C44"
				:foreground "#686c74")))))
