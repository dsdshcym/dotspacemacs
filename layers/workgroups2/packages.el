;;; packages.el --- workgroups2 Layer packages File for Spacemacs
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

(defvar workgroups2-packages
  '(workgroups2)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar workgroups2-excluded-packages '()
  "List of packages to exclude.")

(defun workgroups2/init-workgroups2 ()
  (use-package workgroups2
    :init
    (progn
      (spacemacs/declare-prefix "W" "workgroups")
      (evil-leader/set-key
        "Wc" 'wg-create-workgroup
        "Wr" 'wg-rename-workgroup
        "WS" 'wg-save-session
        "Wo" 'wg-open-session
        "Ws" 'wg-switch-to-workgroup
        "Wp" 'wg-switch-to-workgroup-left
        "Wn" 'wg-switch-to-workgroup-right
        "Wk" 'wg-kill-workgroup
        "WK" 'wg-kill-workgroup-and-buffers
        )
      )
    :config
    (progn
      (setq wg-session-file (concat configuration-layer-private-directory "workgroups/default_session"))
      ;; (wg-open-session wg-session-file)
      )
    )
  )
