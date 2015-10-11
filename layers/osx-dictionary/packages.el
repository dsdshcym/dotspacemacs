;;; packages.el --- osx-dictionary Layer packages File for Spacemacs
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

(defvar osx-dictionary-packages
  '(osx-dictionary
    chinese-word-at-point))

(defun osx-dictionary/init-osx-dictionary ()
  (use-package osx-dictionary
    :defer t
    :init
    (progn
      (evil-leader/set-key
        "xdd" 'osx-dictionary-search-pointer))
    :config
    (progn
      ;; http://blog.binchen.org/posts/use-git-timemachine-with-evil.html
      (evil-make-overriding-map osx-dictionary-mode-map 'normal)
      (add-hook 'osx-dictionary-mode-hook #'evil-normalize-keymaps)
      (add-hook 'osx-dictionary-mode-hook
                (lambda ()
                  (setq show-trailing-whitespace nil))))
    )
  )
