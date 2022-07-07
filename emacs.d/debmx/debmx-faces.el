;;; Fix font faces in daemon mode

(defun debmx/set-font-faces ()
  "Load font settings."
  (message "Setting faces...")
    ;; Fonte padrão
    (set-face-attribute 'default nil :font "Roboto Mono" :height 130)
    ;; Fonte de largura fixa
    (set-face-attribute 'fixed-pitch nil :font "Roboto Mono" :height 130)
    ;; Fonte de largura varável
    (set-face-attribute 'variable-pitch nil :font "Open Sans" :height 120 :weight 'regular))

