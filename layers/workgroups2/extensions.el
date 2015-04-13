;;; extensions.el --- workgroups2 Layer extensions File for Spacemacs
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

(defvar workgroups2-pre-extensions
  '(
    ;; pre extension workgroups2s go here
    )
  "List of all extensions to load before the packages.")

(defvar workgroups2-post-extensions
  '(
    ;; post extension workgroups2s go here
    )
  "List of all extensions to load after the packages.")

;; For each extension, define a function workgroups2/init-<extension-workgroups2>
;;
;; (defun workgroups2/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
