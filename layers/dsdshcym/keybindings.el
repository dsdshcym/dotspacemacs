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

;; ---------------------------------------------------------------------------
;; evil
;; ---------------------------------------------------------------------------
(define-key evil-normal-state-map "+" 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map "-" 'evil-numbers/dec-at-pt)
;; Make evil-mode up/down operate in screen lines instead of logical lines
(define-key evil-normal-state-map "j" 'evil-next-visual-line)
(define-key evil-normal-state-map "k" 'evil-previous-visual-line)
;; Also in visual mode
(define-key evil-visual-state-map "j" 'evil-next-visual-line)
(define-key evil-visual-state-map "k" 'evil-previous-visual-line)

(define-key evil-visual-state-map "<" 'evil-shift-left)
(define-key evil-visual-state-map ">" 'evil-shift-right)

(define-key evil-insert-state-map (kbd "C-u")
  (lambda ()
    (interactive)
    (evil-delete (point-at-bol) (point))))

(evil-leader/set-key
  "ww" 'ace-window
  "wW" 'other-window
  )

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
(evil-declare-key 'normal evil-org-mode-map (kbd "M-o") (lambda () (interactive) ((lambda () (evil-end-of-line) (evil-append nil) (org-meta-return) ))))
(evil-declare-key 'insert evil-org-mode-map (kbd "C-o") 'org-insert-heading-respect-content)
(evil-declare-key 'insert evil-org-mode-map (kbd "M-o") (lambda () (interactive) ((lambda () (evil-end-of-line) (evil-append nil) (org-meta-return) ))))
(evil-declare-key 'normal evil-org-mode-map (kbd "C-S-o") (lambda () (interactive) (org-insert-todo-heading-respect-content) (evil-append nil)))
(evil-declare-key 'insert evil-org-mode-map (kbd "C-S-o") 'org-insert-todo-heading-respect-content)
(evil-declare-key 'normal evil-org-mode-map (kbd "<RET>") 'org-open-at-point)

(evil-declare-key 'normal evil-org-mode-map "gj" 'outline-next-visible-heading)
(evil-declare-key 'normal evil-org-mode-map "gk" 'outline-previous-visible-heading)
(evil-declare-key 'normal evil-org-mode-map "gh" 'org-backward-heading-same-level)
(evil-declare-key 'normal evil-org-mode-map "gl" 'org-forward-heading-same-level)

(evil-leader/set-key
  "aa" 'org-agenda-list
  "oa" 'org-agenda
  "ol" 'org-store-link
  "os" 'org-iswitchb
  "og" 'org-clock-goto
  "oo" 'org-clock-out
  "op" 'org-pomodoro
  "oc" 'org-capture
  "oj" '(lambda () (interactive) (org-refile (universal-argument))))

(evil-leader/set-key-for-mode 'org-mode
  "<"  'private/org-begin-template
  "ns" 'org-narrow-to-subtree
  "nb" 'org-narrow-to-block
  "ne" 'org-narrow-to-element
  "m/" 'org-sparse-tree)