;;; keybindings.el --- dsdshcym Layer key-bindings File
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
(spacemacs/set-leader-keys "fs" (kbd "C-x C-s"))

;; ---------------------------------------------------------------------------
;; Dired
;; ---------------------------------------------------------------------------
(evil-define-key 'normal dired-mode-map "y" 'private/dired-kill-filename-for-visit)

;; ---------------------------------------------------------------------------
;; Pdf-view mode
;; ---------------------------------------------------------------------------
(evil-define-key 'normal pdf-view-mode-map "j" 'pdf-view-next-line-or-next-page)
(evil-define-key 'normal pdf-view-mode-map "k" 'pdf-view-previous-line-or-previous-page)
(evil-define-key 'normal pdf-view-mode-map "/" 'isearch-forward)
(evil-define-key 'normal pdf-view-mode-map "?" 'isearch-backward)
(evil-define-key 'normal pdf-view-mode-map "o" 'pdf-outline)

(evil-define-key 'normal pdf-outline-buffer-mode-map (kbd "<tab>") 'outline-toggle-children)
(evil-define-key 'normal pdf-outline-buffer-mode-map (kbd "S-<tab>") 'outline-show-all)
(evil-define-key 'normal pdf-outline-buffer-mode-map (kbd "<return>") 'pdf-outline-follow-link)
(evil-define-key 'normal pdf-outline-buffer-mode-map "o" 'pdf-outline-follow-link)
(evil-define-key 'normal pdf-outline-buffer-mode-map "O" 'pdf-outline-follow-link-and-quit)
(evil-define-key 'normal pdf-outline-buffer-mode-map "q" 'pdf-outline-quit)
(evil-define-key 'normal pdf-outline-buffer-mode-map "Q" 'pdf-outline-quit-and-kill)

;; ---------------------------------------------------------------------------
;; Ruby mode
;; ---------------------------------------------------------------------------
(spacemacs/set-leader-keys-for-major-mode 'ruby-mode
  "sb" 'ruby-send-buffer
  "sB" 'ruby-send-buffer-and-go)

;; ---------------------------------------------------------------------------
;; image-mode
;; ---------------------------------------------------------------------------
(evil-define-key 'normal image-mode-map "h" 'image-backward-hscroll)
(evil-define-key 'normal image-mode-map "l" 'image-forward-hscroll)
(evil-define-key 'normal image-mode-map "j" 'image-next-line)
(evil-define-key 'normal image-mode-map "k" 'image-previous-line)
(evil-define-key 'normal image-mode-map (kbd "\C-d") 'image-scroll-up)
(evil-define-key 'normal image-mode-map (kbd "\C-u") 'image-scroll-down)

;; --------------------------------------------------------------------
;; helm keybindings
;; --------------------------------------------------------------------
(eval-after-load 'helm
  (lambda ()
    ;; Fix issues with org-refile or org-jump and other helm keybindings
    (define-key helm-map (kbd "<escape>") 'helm-keyboard-quit)
    (define-key helm-map (kbd "C-u") 'backward-kill-sentence)
    ))

;; --------------------------------------------------------------------
;; smartparens keybindings
;; --------------------------------------------------------------------
(define-key evil-insert-state-map (kbd "C-s") 'sp-forward-slurp-sexp)
(define-key evil-insert-state-map (kbd "S-C-S") 'sp-backward-slurp-sexp)

;; --------------------------------------------------------------------
;; company keybindings
;; --------------------------------------------------------------------
(eval-after-load 'company
  (lambda ()
    (define-key company-active-map (kbd "\C-n") 'company-select-next)
    (define-key company-active-map (kbd "\C-p") 'company-select-previous)
    (define-key company-active-map (kbd "\C-w") 'evil-delete-backward-word)
    ))

;; --------------------------------------------------------------------
;; org-mode keybindings
;; --------------------------------------------------------------------
(evil-declare-key 'normal evil-org-mode-map "o" 'evil-open-below)
(evil-declare-key 'normal evil-org-mode-map "O" 'evil-open-above)
(evil-declare-key 'normal evil-org-mode-map "<" 'evil-shift-left)
(evil-declare-key 'normal evil-org-mode-map ">" 'evil-shift-right)
(evil-declare-key 'normal evil-org-mode-map (kbd "C-o") (lambda () (interactive) (evil-end-of-line) (org-insert-heading-respect-content) (evil-append nil)))
(evil-declare-key 'normal evil-org-mode-map (kbd "M-o") (lambda () (interactive) (evil-end-of-line) (evil-append nil) (org-meta-return)))
(evil-declare-key 'insert evil-org-mode-map (kbd "C-o") 'org-insert-heading-respect-content)
(evil-declare-key 'insert evil-org-mode-map (kbd "M-o") 'org-meta-return)
(evil-declare-key 'normal evil-org-mode-map (kbd "C-S-o") (lambda () (interactive) (org-insert-todo-heading-respect-content) (evil-append nil)))
(evil-declare-key 'insert evil-org-mode-map (kbd "C-S-o") 'org-insert-todo-heading-respect-content)
(evil-declare-key 'normal evil-org-mode-map (kbd "<RET>") 'org-open-at-point)

(evil-declare-key 'normal evil-org-mode-map "gj" 'outline-next-visible-heading)
(evil-declare-key 'normal evil-org-mode-map "gk" 'outline-previous-visible-heading)
(evil-declare-key 'normal evil-org-mode-map "gh" 'outline-up-heading)
(evil-declare-key 'normal evil-org-mode-map "gl" 'org-forward-heading-same-level)

(evil-declare-key 'normal evil-org-mode-map "H" 'evil-window-top)
(evil-declare-key 'normal evil-org-mode-map "L" 'evil-window-bottom)

(spacemacs/set-leader-keys
  "aa" 'org-agenda-list
  "oa" 'org-agenda
  "ol" 'org-store-link
  "ob" 'org-iswitchb
  "os" '(lambda () (interactive) (insert (concat " " (org-mac-safari-get-frontmost-url))))
  "og" 'org-clock-goto
  "oo" 'org-clock-out
  "op" 'org-pomodoro
  "oc" 'org-capture
  "oj" '(lambda () (interactive) (org-refile '(4))))

(spacemacs/set-leader-keys-for-major-mode 'org-mode
                              "/" 'org-sparse-tree
                              "<"  'private/org-begin-template)
