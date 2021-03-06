;;; packages.el --- dsdshcym Layer packages File for Spacemacs
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
(setq dsdshcym-packages
      '(
        avy
        evil
        evil-org
        fcitx
        flycheck
        ;; org is installed by `org-plus-contrib'
        (org :location built-in)
        (org-plus-contrib :step pre)
        org-page
        flycheck
        org-tree-slide
        (mu4e :location "/usr/share/emacs/site-lisp/mu4e")
        (org-mu4e :location "/usr/share/emacs/site-lisp/mu4e")
        (mu4e-contrib :location "/usr/share/emacs/site-lisp/mu4e")
        org-pdfview
        smartparens
        spaceline
        undo-tree
        pangu-spacing
        elfeed
        (clip2org :location local)
        zeal-at-point
        ))

;; List of packages to exclude.
(setq dsdshcym-excluded-packages '())

(defun dsdshcym/post-init-zeal-at-point ()
  (use-package zeal-at-point
    :config
    (push '(ruby-mode . "ruby,rails") zeal-at-point-mode-alist)
    )
  )

(defun dsdshcym/init-clip2org ()
  (use-package clip2org
    :config
    (progn
      (setq clip2org-clippings-file "/mnt/Kindle/documents/My Clippings.txt")
      (setq clip2org-persistence-file (expand-file-name "clip2org-persist.txt" spacemacs-cache-directory))))
  )

(defun dsdshcym/init-pangu-spacing ()
  (use-package pangu-spacing
    :config
    (progn
      (global-pangu-spacing-mode 1)
      ;; (setq pangu-spacing-real-insert-separtor t)
      ))
  )

(defun dsdshcym/init-fcitx ()
  (use-package fcitx
    :defer nil
    :init
    (progn
      (spacemacs|add-toggle rimeime
        :status private/toggle-rimeime-mode
        :on (private/turn-on-rimeime-mode)
        :off (private/turn-off-rimeime-mode)
        :documentation "Use Chinese Input when exiting the insert mode"
        :evil-leader "tR")
      )
    )
  )

(defun dsdshcym/post-init-avy ()
  (setq avy-all-windows nil)
  )

(defun dsdshcym/post-init-undo-tree ()
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/.cache/undo-tree-history")))
  (setq undo-tree-auto-save-history t)
  )

(defun dsdshcym/init-org-pdfview ()
  (use-package org-pdfview
    :config
    (eval-after-load 'arg
      (progn
        (add-to-list 'org-file-apps '("\\.pdf\\'" . org-pdfview-open))
        (add-to-list 'org-file-apps '("\\.pdf::\\([[:digit:]]+\\)\\'" . org-pdfview-open)))))
  )

(defun dsdshcym/init-mu4e ()
  (use-package mu4e
    :init
    (spacemacs/set-leader-keys "am" 'mu4e)
    :config
    (progn
      (setq mu4e-confirm-quit nil)

      ;; default
      (setq mu4e-maildir (expand-file-name "~/Maildir"))

      (setq mu4e-refile-folder "/Gmail/[Gmail].All Mail")
      (setq mu4e-drafts-folder "/Gmail/[Gmail].Drafts")
      (setq mu4e-sent-folder   "/Gmail/[Gmail].Sent Mail")
      (setq mu4e-trash-folder  "/Gmail/[Gmail].Trash")

      ;; don't save message to Sent Messages, GMail/IMAP will take care of this
      (setq mu4e-sent-messages-behavior 'delete)

      (setq mu4e-update-interval 1800)

      (setq mu4e-html2text-command "pandoc -f html -t plain")

      ;; setup some handy shortcuts
      (setq mu4e-maildir-shortcuts
            '(("/Gmail/INBOX"             . ?i)
              ("/Gmail/[Gmail].Sent Mail" . ?s)
              ("/Gmail/[Gmail].Trash"     . ?t)
              ("/Gmail/[Gmail].All Mail"  . ?a)
              ("/FudanMail/INBOX"         . ?f)
              ("/FudanMail/Lab"           . ?l)
              ("/FudanMail/13CS"          . ?c)
              ("/FudanMail/Course"        . ?C)))

      (setq mu4e-bookmarks
            '(
              ("flag:unread AND \
                NOT flag:trashed AND \
                NOT maildir:\"/Gmail/[Gmail].All Mail\" AND \
                NOT maildir:\"/Strikingly/[Gmail].All Mail\" AND \
                NOT maildir:\"/Gmail/[Gmail].Spam\""
               "Unread messages"  ?u)
              ("date:today..now AND NOT maildir:\"/Gmail/[Gmail].All Mail\""
               "Today's messages" ?t)
              ("date:7d..now AND NOT maildir:\"/Gmail/[Gmail].All Mail\""
               "Last 7 days"      ?w)
              )
            )

      ;; allow for updating mail using 'U' in the main view:
      (setq mu4e-get-mail-command "offlineimap")

      (setq mu4e-attachment-dir "~/Downloads")

      (setq mu4e-view-prefer-html nil)

      ;; Store link to message if in header view, not to header query
      (setq org-mu4e-link-query-in-headers-mode nil)

      (add-to-list 'mu4e-view-actions
                   '("browser" . mu4e-action-view-in-browser) t)

      (setq mu4e-org-contacts-file (expand-file-name "~/Org/contacts.org"))
      (add-to-list 'mu4e-headers-actions
                   '("org-contact-add" . mu4e-action-add-org-contact) t)
      (add-to-list 'mu4e-view-actions
                   '("org-contact-add" . mu4e-action-add-org-contact) t)

      (setq message-send-mail-function 'smtpmail-send-it
            starttls-use-gnutls t
            smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
            smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
            smtpmail-default-smtp-server "smtp.gmail.com"
            smtpmail-smtp-server "smtp.gmail.com"
            smtpmail-smtp-service 587)

      (defvar my-mu4e-account-alist
        '(("Gmail"
           (mu4e-sent-folder   "/Gmail/[Gmail].Sent Mail")
           (mu4e-drafts-folder "/Gmail/[Gmail].Drafts")
           (user-mail-address "dsdshcym@gmail.com")
           (smtpmail-default-smtp-server "smtp.gmail.com")
           (smtpmail-smtp-user "dsdshcym")
           (smtpmail-smtp-server "smtp.gmail.com")
           ;; (smtpmail-stream-type starttls)
           (smtpmail-smtp-service 587))
          ("FudanMail"
           (mu4e-sent-folder "/FudanMail/Sent Items")
           (mu4e-drafts-folder "/FudanMail/Drafts")
           (user-mail-address "12307130174@fudan.edu.cn")
           (smtpmail-default-smtp-server "mail.fudan.edu.cn")
           (smtpmail-smtp-user "12307130174@fudan.edu.cn")
           (smtpmail-smtp-server "mail.fudan.edu.cn")
           (smtpmail-stream-type ssl)
           (smtpmail-smtp-service 465))))

      (defun my-mu4e-set-account ()
        "Set the account for composing a message."
        (let* ((account
                (if mu4e-compose-parent-message
                    (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                      (string-match "/\\(.*?\\)/" maildir)
                      (match-string 1 maildir))
                  (completing-read (format "Compose with account: (%s) "
                                           (mapconcat #'(lambda (var) (car var))
                                                      my-mu4e-account-alist "/"))
                                   (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                                   nil t nil nil (caar my-mu4e-account-alist))))
               (account-vars (cdr (assoc account my-mu4e-account-alist))))
          (if account-vars
              (mapc #'(lambda (var)
                        (set (car var) (cadr var)))
                    account-vars)
            (error "No email account found"))))

      (add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

      (evil-make-overriding-map mu4e-main-mode-map 'normal t)
      (evil-define-key 'normal mu4e-main-mode-map
        "j" 'mu4e~headers-jump-to-maildir
        "RET" 'mu4e-view-message)

      (evil-make-overriding-map mu4e-headers-mode-map 'normal t)
      (evil-define-key 'normal mu4e-headers-mode-map
        "J" 'mu4e~headers-jump-to-maildir
        "j" 'evil-next-line
        "k" 'evil-previous-line
        "C" 'mu4e-compose-new
        "o" 'mu4e-view-message
        )

      (evil-make-overriding-map mu4e-view-mode-map 'normal t)
      (evil-define-key 'normal mu4e-view-mode-map
        "J" 'mu4e~headers-jump-to-maildir
        "j" 'evil-next-line
        "k" 'evil-previous-line
        "C" 'mu4e-compose-new
        "o" 'mu4e-view-message
        "Q" 'mu4e-raw-view-quit-buffer)

      (evil-set-initial-state 'mu4e-mode 'normal)
      (evil-set-initial-state 'mu4e-main-mode 'normal)
      (evil-set-initial-state 'mu4e-headers-mode 'normal)
      (evil-set-initial-state 'mu4e-view-mode 'normal)
      )
    )
  )

(defun dsdshcym/init-org-mu4e ()
  (use-package org-mu4e
    :config
    (progn
      (setq org-mu4e-convert-to-html nil)
      (add-hook 'mu4e-compose-mode-hook 'org~mu4e-mime-switch-headers-or-body)))
  )

(defun dsdshcym/init-mu4e-contrib ()
  (use-package mu4e-contrib
    :config
    (progn
      (setq mu4e-html2text-command 'mu4e-shr2text)))
  )

(defun dsdshcym/init-org-tree-slide ()
  (use-package org-tree-slide
    :defer t
    :config
    (progn
      (spacemacs/set-leader-keys-for-minor-mode 'org-tree-slide "." 'org-tree-slide-move-next-tree)
      (spacemacs/set-leader-keys-for-minor-mode 'org-tree-slide "," 'org-tree-slide-move-previous-tree)
      (org-tree-slide-narrowing-control-profile)
      (setq org-tree-slide-skip-outline-level 4)
      (setq org-tree-slide-skip-done nil)))
)

(defun dsdshcym/post-init-flycheck ()
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  )

(defun dsdshcym/init-org-page ()
  (use-package org-page
    :config
    (progn
      (setq op/repository-directory "~/Github/dsdshcym.github.io")
      (setq op/site-domain "http://dsdshcym.github.io")
      (setq op/personal-github-link "https://github.com/dsdshcym")
      (setq op/site-main-title "dsdshome")
      (setq op/site-sub-title "This is my small blog :)")
      ))
  )

(defun dsdshcym/post-init-evil ()
  (progn
    (setq evil-want-fine-undo 'No)
    (setq evil-move-beyond-eol nil)
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

   (mapc #'evil-declare-ignore-repeat
         '(spacemacs/describe-key
           spacemacs/describe-function
           spacemacs/describe-variable
           spacemacs/write-file
           kill-this-buffer
           ido-kill-buffer
           outline-next-visible-heading
           outline-previous-visible-heading
           outline-up-heading
           spacemacs/enter-ahs-forward
           spacemacs/enter-ahs-backward
           evil-visualstar/begin-search-forward
           evil-visualstar/begin-search-backward
           org-export-dispatch
           org-end-of-line
           org-beginning-of-line
           split-window-below
           split-window-below-and-focus
           split-window-right
           split-window-right-and-focus))
    )

  (evil-define-motion evil-goto-line (count)
    "Go to the first non-blank character of line COUNT.
By default the (truly) last line."
    :jump t
    :type line
    (if (null count)
        (goto-char (buffer-size))
      (goto-char (point-min))
      (forward-line (1- count)))
    (evil-first-non-blank))
  )

(defun dsdshcym/post-init-smartparens ()
  (progn
    (sp-with-modes '(emacs-lisp-mode
                     inferior-emacs-lisp-mode
                     dotspacemacs-mode
                     lisp-interaction-mode
                     lisp-mode)
      (sp-local-pair "'" nil :actions nil)
      (sp-local-pair "`" "'" :when '(sp-in-string-p) :actions '(insert wrap)))

    (sp-with-modes '(tex-mode
                     plain-tex-mode
                     latex-mode
                     org-mode)
      (sp-local-pair "（" "）")
      (sp-local-pair "「" "」")
      (sp-local-pair "『" "』")
      (sp-local-pair "$" "$")
      (sp-local-pair "\\[" "\\]")
      (sp-local-pair "\\(" "\\)")
      (sp-local-pair "\\{" "\\}")
      (sp-local-pair "\\left(" "\\right)")
      (sp-local-pair "\\left\\{" "\\right\\}"))
    )
  )

(defun dsdshcym/post-init-spaceline ()
  (progn
    (setq spaceline-org-clock-p t)
  )
)

(defun dsdshcym/post-init-org ()
  (progn
    ;; --------------------------------------------------------------------
    ;; Functions which name starts with "bh" are from
    ;; http://doc.norang.ca/org-mode.html
    ;; --------------------------------------------------------------------
    (mapc #'evil-declare-ignore-repeat
          '(org-cycle
            org-shifttab
            org-ctrl-c-ctrl-c
            org-next-visible-heading
            org-previous-visible-heading))

    (setq org-startup-indented nil)

    (add-hook 'org-mode-hook (lambda () (setq evil-shift-width 2)))

    ;; -----------------------------
    ;; Agenda
    ;; -----------------------------
    (setq evil-leader/no-prefix-mode-rx '("Org-Agenda.*mode"))
    (setq org-agenda-diary-file "~/Org/diary.org")
    (setq org-agenda-files '("~/Org"
                             "~/Org/Notes"))

    ;; Overwrite the current window with the agenda
    (setq org-agenda-window-setup 'current-window)

    (setq org-agenda-restore-windows-after-quit t)

    (setq org-agenda-span 'day)

    (setq org-agenda-custom-commands
          '(("h" "Agenda and Home-related tasks"
             ((agenda "")
              (tags-todo "@HOME")))
            ("s" "Agenda and School-related tasks"
             ((agenda "")
              (tags-todo "@SCHOOL")))
            ("w" "Animations or TVs to watch"
             ((agenda "")
              (tags-todo "TOWATCH")))))

    ;; Agenda clock report parameters
    (setq org-agenda-clockreport-parameter-plist
          '(:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80))

    ;; -----------------------------
    ;; Tasks and States
    ;; -----------------------------
    (setq org-todo-keywords
          '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
            (sequence "WAITING(w@/!)" "SOMEDAY(s)" "|" "CANCELLED(c@/!)")))

    ;; -----------------------------
    ;; Capture
    ;; -----------------------------
    ;; Thank you random guy from StackOverflow
    ;; http://stackoverflow.com/questions/23517372/hook-or-advice-when-aborting-org-capture-before-template-selection

    (defadvice org-capture
        (after make-full-window-frame activate)
      "Advise capture to be the only window when used as a popup"
      (if (equal "emacs-capture" (frame-parameter nil 'name))
          (delete-other-windows)))

    (defadvice org-capture-finalize
        (after delete-capture-frame activate)
      "Advise capture-finalize to close the frame"
      (if (equal "emacs-capture" (frame-parameter nil 'name))
          (delete-frame)))

    (setq org-directory "~/Org")
    (setq org-default-notes-file "~/Org/refile.org")
    (setq org-capture-templates
          '(("t" "Task" entry
             (file "~/Org/refile.org")
             "* TODO %?\n  %U")
            ("T" "Clock-in Task" entry
             (file "~/Org/refile.org")
             "* TODO %?\n"
             :clock-in t
             :clock-resume t)
            ("d" "Distraction in a pomodoro" entry
             (file "~/Org/refile.org")
             "* TODO %^{Task}\n  SCHEDULED: %t\n  %l"
             :immediate-finish t)
            ("n" "Note" entry
             (file "~/Org/refile.org")
             "* %?\n  %U")
            ("l" "Note with link to current file" entry
             (file "~/Org/refile.org")
             "* %a\n  %U")
            ("L" "(Clocked in) Note with link to current file" entry
             (file "~/Org/refile.org")
             "* %a"
             :clock-in t
             :clock-resume t)
            ("j" "Journal" entry
             (file+datetree "~/Org/diary.org")
             "* %^{Content}\n"
             :clock-in t
             :clock-resume t)
            ("J" "Journal from Phone" entry
             (file+datetree "~/Org/diary.org")
             "* %^{Content}\n  :LOGBOOK:\n  CLOCK: %^{Begin}U--%^{End}U\n  :END:"
             :immediate-finish t)
            ("p" "People (Contacts)" entry
             (file "~/Org/contacts.org")
             "* %(org-contacts-template-name)\n  :PROPERTIES:\n  :EMAIL: %(org-contacts-template-email)\n  :END:")
            ("k" "Push to Kindle" entry
             (file+headline "~/Org/refile.org" "Push to Kindle")
             "* %a\n  %U"
             :immediate-finish t)
            ("P" "Online Video" entry
             (file "~/Org/refile.org")
             "* %a %(org-capture-play-video)"
             :clock-in t
             :clock-resume t)
            ))

    (defun org-capture-play-video ()
      (let ((link (plist-get org-store-link-plist :link)))
        (you-get link)
        nil))

    (defun you-get (link)
      (start-process "you-get" "*you-get*" "/usr/bin/you-get" "-p mpv" "--no-caption" link))

    ;; -----------------------------
    ;; Refile
    ;; -----------------------------
    (setq org-refile-targets '((private/opened-buffer-files :maxlevel . 9)))
    (setq org-refile-allow-creating-parent-nodes 'confirm)
    (setq org-refile-use-cache t)

    ;; -----------------------------
    ;; Clock
    ;; -----------------------------
    ;; Show lot of clocking history so it's easy to pick items
    (setq org-clock-history-length 20)
    ;; Resume clocking task on clock-in if the clock is open
    (setq org-clock-in-resume t)
    ;; Change task state to STARTED when clocking in
    (setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
    ;; Separate drawers for clocking and logs
    (setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
    ;; Save clock data and notes in the LOGBOOK drawer
    (setq org-log-into-drawer "LOGBOOK")
    ;; Save clock data and state changes and notes in the LOGBOOK drawer
    (setq org-clock-into-drawer t)
    ;; Removes clocked tasks with 0:00 duration
    (setq org-clock-out-remove-zero-time-clocks t)
    ;; Save the running clock and all clock history when exiting Emacs, load it on startup
    (setq org-clock-persist t)
    ;; Do not prompt to resume an active clock
    (setq org-clock-persist-query-resume nil)
    ;; Resume clocking task when emacs is restarted
    (org-clock-persistence-insinuate)
    (setq org-clock-persist-file "~/.emacs.d/org-files/org-clock-save.el")
    ;; Clock out when moving task to a done state
    (setq org-clock-out-when-done t)
    ;; Enable auto clock resolution for finding open clocks
    (setq org-clock-auto-clock-resolution '(when-no-clock-is-running))
    ;; Include current clocking task in clock reports
    (setq org-clock-report-include-clocking-task t)

    ;; (add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

    ;; Make clockcheck more accurate
    (setq org-agenda-clock-consistency-checks
          (quote (:max-duration "4:00"
                                :min-duration 0
                                :max-gap 0
                                :gap-ok-around ("4:00"))))

    ;; -----------------------------
    ;; Tags
    ;; -----------------------------
    (setq org-tag-alist '((:startgroup)
                          ("@SCHOOL" . ?s)
                          ("@HOME" . ?h)
                          ("@WORK" . ?w)
                          (:endgroup)
                          ("TOWATCH" . ?W)
                          ("TOREAD" . ?R)))

    ;; -----------------------------
    ;; Archive
    ;; -----------------------------
    (setq org-archive-mark-done nil)
    (setq org-archive-location "%s_archive::* Archived Tasks")

    ;; -----------------------------
    ;; Export / Publish
    ;; -----------------------------
    (org-babel-do-load-languages
     'org-babel-load-languages
     '(
       (sh . t)
       (emacs-lisp . t)
       (latex . t)
       (python . t)
       (ruby . t)
       (org . t)
       (sql . t)
       (C . t)
       (dot . t)
       (scheme . t)
       ))

    (setq org-export-babel-evaluate nil
          org-confirm-babel-evaluate nil)

    (setq org-export-backends '(beamer html latex md gfm))

    (setq org-export-with-sub-superscripts '{}
          org-export-with-section-numbers 3
          org-export-with-todo-keywords nil
          org-export-with-timestamps nil)

    ;; {{ export org-mode in Chinese into PDF
    ;; @see http://freizl.github.io/posts/2012-04-06-export-orgmode-file-in-Chinese.html
    (setq org-latex-pdf-process
          '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

    ;; Use minted to export code blocks
    ;; (setq org-latex-listings 'minted)
    ;; (add-to-list 'org-latex-packages-alist '("" "minted"))
    ;; (setq org-latex-minted-options
    ;;       '(("frame" "lines") ("linenos" "true") ("breaklines" "true")))

    ;; Use listings to export code blocks
    (setq org-latex-listings t)
    (setq org-latex-listings-options
          '(("breaklines" "")
            ("keywordstyle" "\\color{black}\\bfseries")
            ("basicstyle" "\\ttfamily\\scriptsize")))
    (add-to-list 'org-latex-packages-alist '("" "listings"))
    (add-to-list 'org-latex-packages-alist '("" "color"))

    (setq org-html-mathjax-options
          (quote
           ((path "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_SVG")
            (scale "100")
            (align "center")
            (indent "2em")
            (mathml nil))))

    ;; -----------------------------
    ;; Reminder
    ;; -----------------------------
    ;; Rebuild the reminders everytime the agenda is displayed
    (add-hook 'org-finalize-agenda-hook 'bh/org-agenda-to-appt 'append)

    (setq appt-disp-window-function (function private/appt-display))
    (setq appt-delete-window-function nil)

    ;; If we leave Emacs running overnight - reset the appointments one minute after midnight
    (run-at-time "24:01" nil 'bh/org-agenda-to-appt)

    ;; -----------------------------
    ;; Others
    ;; -----------------------------

    (setq org-cycle-include-plain-lists 'integrate)

    (setq org-id-locations-file "~/.emacs.d/org-files/.org-id-locations")
    (setq org-enforce-todo-dependencies nil)

    (setq org-return-follows-link t)

    (setq org-special-ctrl-a/e t)
    (setq org-special-ctrl-k t)
    (setq org-yank-adjusted-subtrees t)

    ;; Enable habit tracking (and a bunch of other modules)
    (setq org-modules '(org-crypt
                        org-docview
                        org-habit
                        org-id
                        org-info
                        org-irc
                        org-mhe
                        org-protocol
                        org-rmail
                        org-w3m
                        org-bullets
                        org-eww
                        org-mac-link
                        org-contacts))
    (eval-after-load 'org
      '(org-load-modules-maybe t))

    ;; position the habit graph on the agenda to the right of the default
    (setq org-habit-graph-column 50)

    (run-at-time "06:00" 86400 '(lambda () (setq org-habit-show-habits t)))

    ;; Showing source block syntax highlighting
    (setq org-src-fontify-natively t)

    (setq org-export-coding-system 'utf-8)
    (set-charset-priority 'unicode)

    (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

    (setq org-cycle-separator-lines 0)
    (setq org-blank-before-new-entry '((heading . nil)
                                       (plain-list-item . nil)))
    ))
