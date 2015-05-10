;;; packages.el --- org-tree-slide Layer packages File for Spacemacs
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

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(defvar org-tree-slide-packages
  '(
    org-tree-slide
    ))

;; List of packages to exclude.
(defvar org-tree-slide-excluded-packages '())

;; For each package, define a function org-tree-slide/init-<package-org-tree-slide>
;;
;; (defun org-tree-slide/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun org-tree-slide/init-org-tree-slide ()
  (use-package org-tree-slide
    :defer t
    :config
    (progn
      (evil-define-key 'normal org-tree-slide-mode-map "gj" 'org-tree-slide-move-next-tree)
      (evil-define-key 'normal org-tree-slide-mode-map "gk" 'org-tree-slide-move-previous-tree)
      ;; (define-key org-tree-slide-mode-map (kbd "gk")
        ;; 'org-tree-slide-move-next-tree)
      ;;   (define-key org-tree-slide-mode-map (kbd "gj")
      ;;     'org-tree-slide-move-next-tree)
      ;;   (define-key org-tree-slide-mode-map (kbd "<f11>")
      ;;     'org-tree-slide-content)
      (org-tree-slide-narrowing-control-profile)
      (setq org-tree-slide-skip-outline-level 4)
      (setq org-tree-slide-skip-done nil)))
)
