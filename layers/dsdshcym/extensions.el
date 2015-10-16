;;; extensions.el --- dsdshcym Layer extensions File for Spacemacs
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

(setq dsdshcym-pre-extensions
      '(
        ;; pre extension names go here
        ))

(setq dsdshcym-post-extensions
      '(clip2org))

(defun dsdshcym/init-clip2org ()
  (use-package clip2org
    :config
    (progn
      (setq clip2org-clippings-file "/Volumes/Kindle/documents/My Clippings.txt")
      (setq clip2org-persistence-file (expand-file-name "clip2org-persist.txt" spacemacs-cache-directory))))
  )
