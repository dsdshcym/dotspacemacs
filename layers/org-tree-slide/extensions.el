;;; extensions.el --- org-tree-slide Layer extensions File for Spacemacs
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

(setq org-tree-slide-pre-extensions
  '(
    ;; pre extension org-tree-slides go here
    ))

(setq org-tree-slide-post-extensions
  '(
    ;; post extension org-tree-slides go here
    ))

;; For each extension, define a function org-tree-slide/init-<extension-org-tree-slide>
;;
;; (defun org-tree-slide/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
