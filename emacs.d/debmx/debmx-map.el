;;; code: keys definitions

;; Move line up/down
(global-set-key (kbd "C-M-<prior>") 'debmx/move-line-up)
(global-set-key (kbd "C-M-<next>") 'debmx/move-line-down)
;; Move cursor
(global-set-key (kbd "M-,") 'beginning-of-buffer)
(global-set-key (kbd "M-.") 'end-of-buffer)

;; C-z (prefix)
(progn
  ;; debmx key bindings definitions
  (define-prefix-command 'debmx-map)
  (define-key debmx-map (kbd "k") 'kill-whole-line)
  (define-key debmx-map (kbd "<up>") 'windmove-up)
  (define-key debmx-map (kbd "<down>") 'windmove-down)
  (define-key debmx-map (kbd "<left>") 'windmove-left)
  (define-key debmx-map (kbd "<right>") 'windmove-right)
  (define-key debmx-map (kbd "w") 'wc)
  (define-key debmx-map (kbd "b") 'debmx-bookmark)  ; Bookmark menu
  (define-key debmx-map (kbd "d") 'debmx-duplicate) ; Duplicate menu
  (define-key debmx-map (kbd "e") 'debmx-eval)      ; Eval menu
  (define-key debmx-map (kbd "f") 'debmx-file)      ; Files menu
  (define-key debmx-map (kbd "o") 'debmx-cycle)     ; Cycle menu
  (define-key debmx-map (kbd "s") 'debmx-spell)     ; Spell menu
  (global-set-key (kbd "C-z") debmx-map))

;; Duplicate menu
(progn
  ;; debmx keys for duplacte lines and regions
  (define-prefix-command 'debmx-duplicate)
  (define-key debmx-duplicate (kbd "l") 'debmx/duplicate-line)
  (define-key debmx-duplicate (kbd "r") 'debmx/duplicate-region))

;; Spell submenu
(progn
  ;; debmx keys for spell check
  (define-prefix-command 'debmx-spell)
  (define-key debmx-spell (kbd "d") 'ispell-change-dictionary)
  (define-key debmx-spell (kbd "b") 'ispell-buffer)
  (define-key debmx-spell (kbd "r") 'ispell-region)
  (define-key debmx-spell (kbd "t") 'flyspell-mode)  
  (define-key debmx-spell (kbd "w") 'ispell-word))


;; Cycle submenu
(progn
  ;; debmx keys for cycle through visible windows and frames
  (define-prefix-command 'debmx-cycle)
  (define-key debmx-cycle (kbd "w") 'other-window)
  (define-key debmx-cycle (kbd "f") 'other-frame))

;; Eval submenu
(progn
  ;; debmx keys for evaluating
  (define-prefix-command 'debmx-eval)
  (define-key debmx-eval (kbd "b") 'eval-buffer)
  (define-key debmx-eval (kbd "e") 'eval-expression)
  (define-key debmx-eval (kbd "r") 'eval-region))

;; Bookmark submenu
(progn
  ;; debmx keys for bookmarks
  (define-prefix-command 'debmx-bookmark)
  (define-key debmx-bookmark (kbd "a") 'bookmark-set)
  (define-key debmx-bookmark (kbd "j") 'bookmark-jump)
  (define-key debmx-bookmark (kbd "l") 'bookmark-bmenu-list)
  (define-key debmx-bookmark (kbd "o") 'bookmark-jump-other-window))

;; Files menu
(progn
  ;; file actions
  (define-prefix-command 'debmx-file)
  (define-key debmx-file (kbd "i") 'debmx/open-init-file) ; open user init file
  (define-key debmx-file (kbd "n") 'debmx/new-buffer)     ; create new buffer
  (define-key debmx-file (kbd "f") 'ffap)                 ; open file under cursor
  (define-key debmx-file (kbd "m") 'ffap-menu))           ; list files mentioned on buffer

(provide 'debmx-map)
;;; debmx-map.el ends here
