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
        evil
        evil-leader
        smartparens
        spaceline
        evil-org
        ;; org is installed by `org-plus-contrib'
        (org :location built-in)
        (org-plus-contrib :step pre)
        org-page
        flycheck
        avy
        osx-dictionary
        org-tree-slide
        pdf-tools
        (mu4e :location "/usr/local/share/emacs/site-lisp/mu4e")
        (org-mu4e :location "/usr/local/share/emacs/site-lisp/mu4e")
        (mu4e-contrib :location "/usr/local/share/emacs/site-lisp/mu4e")
        langtool
        org-pdfview
        ))

;; List of packages to exclude.
(setq dsdshcym-excluded-packages '())

(defun dsdshcym/init-org-pdfview ()
  (use-package org-pdfview
    :config
    (eval-after-load 'arg
      (progn
        (add-to-list 'org-file-apps '("\\.pdf\\'" . org-pdfview-open))
        (add-to-list 'org-file-apps '("\\.pdf::\\([[:digit:]]+\\)\\'" . org-pdfview-open)))))
  )

(defun dsdshcym/init-langtool ()
  (use-package langtool
    :config
    (progn
      (setq langtool-language-tool-jar
            "/usr/local/Cellar/languagetool/3.0/libexec/languagetool-commandline.jar")
      (setq langtool-disabled-rules '("WHITESPACE_RULE"
                                      "EN_UNPAIRED_BRACKETS"
                                      "COMMA_PARENTHESIS_WHITESPACE"
                                      "EN_QUOTES"))
      ))
  )

(defun dsdshcym/init-mu4e ()
  (use-package mu4e
    :init
    (evil-leader/set-key "am" 'mu4e)
    :config
    (progn
      ;; default
      (setq mu4e-maildir (expand-file-name "~/Maildir"))

      (setq mu4e-drafts-folder "/Gmail/[Gmail].Drafts")
      (setq mu4e-sent-folder   "/Gmail/[Gmail].Sent Mail")
      (setq mu4e-trash-folder  "/Gmail/[Gmail].Trash")

      ;; don't save message to Sent Messages, GMail/IMAP will take care of this
      (setq mu4e-sent-messages-behavior
            (lambda ()
              (if (string= (message-sendmail-envelope-from) "dsdshcym@gmail.com")
                  'delete 'sent)))

      (setq mu4e-update-interval 1800)

      (setq mu4e-html2text-command "pandoc -f html -t plain")

      ;; setup some handy shortcuts
      (setq mu4e-maildir-shortcuts
            '(("/Gmail/INBOX"             . ?i)
              ("/Gmail/[Gmail].Sent Mail" . ?s)
              ("/Gmail/[Gmail].Trash"     . ?t)
              ("/FudanMail/INBOX"         . ?f)
              ("/FudanMail/Lab"           . ?l)
              ("/FudanMail/13CS"          . ?c)
              ("/FudanMail/Course"        . ?C)))

      ;; allow for updating mail using 'U' in the main view:
      (setq mu4e-get-mail-command "offlineimap")

      (setq mu4e-attachment-dir "~/Downloads")

      (setq mu4e-view-prefer-html t)

      (add-hook 'mu4e-headers-mode-hook
                (lambda ()
                  (setq show-trailing-whitespace nil)))

      (add-hook 'mu4e-view-mode-hook
                (lambda ()
                  (setq show-trailing-whitespace nil)))

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
      (setq org-mu4e-convert-to-html t)
      (add-hook 'mu4e-compose-mode-hook 'org~mu4e-mime-switch-headers-or-body)))
  )

(defun dsdshcym/init-mu4e-contrib ()
  (use-package mu4e-contrib
    :config
    (progn
      (setq mu4e-html2text-command 'mu4e-shr2text)))
  )

(defun dsdshcym/init-pdf-tools ()
  (use-package pdf-tools
    :config
    (progn
      (pdf-tools-install)
      (setq pdf-view-use-imagemagick t)
      (evil-make-overriding-map pdf-view-mode-map 'normal)
      (add-hook 'pdf-view-mode-hook #'evil-normalize-keymaps)))
  )

(defun dsdshcym/init-org-tree-slide ()
  (use-package org-tree-slide
    :defer t
    :config
    (progn
      (evil-leader/set-key-for-mode 'org-mode "." 'org-tree-slide-move-next-tree)
      (evil-leader/set-key-for-mode 'org-mode "," 'org-tree-slide-move-previous-tree)
      (evil-define-key 'normal org-tree-slide-mode-map "H" 'org-tree-slide-move-previous-tree)
      (evil-define-key 'normal org-tree-slide-mode-map "L" 'org-tree-slide-move-next-tree)
      ;; (define-key org-tree-slide-mode-map (kbd "H")
      ;;   'org-tree-slide-move-next-tree)
      ;;   (define-key org-tree-slide-mode-map (kbd "gj")
      ;;     'org-tree-slide-move-next-tree)
      ;;   (define-key org-tree-slide-mode-map (kbd "<f11>")
      ;;     'org-tree-slide-content)
      (org-tree-slide-narrowing-control-profile)
      (setq org-tree-slide-skip-outline-level 4)
      (setq org-tree-slide-skip-done nil)))
)

(defun dsdshcym/init-osx-dictionary ()
  (use-package osx-dictionary
    :defer t
    :init
    (progn
      (evil-leader/set-key
        "xdd" 'osx-dictionary-search-input
        "xdD" 'osx-dictionary-search-pointer))
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

(defun dsdshcym/post-init-avy ()
  (evil-leader/set-key "SPC" 'avy-goto-char-timer)
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
           outline-up-heading))
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

(defun dsdshcym/post-init-evil-leader ()
  (evil-leader/set-key
    "ww" 'ace-window
    "wW" 'other-window
    )
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
            org-next-visible-heading
            org-previous-visible-heading))

    (add-hook 'org-mode-hook (lambda () (setq evil-shift-width 2)))

    ;; -----------------------------
    ;; Agenda
    ;; -----------------------------
    (setq evil-leader/no-prefix-mode-rx '("Org-Agenda.*mode"))
    (setq org-agenda-diary-file "~/org/diary.org")
    (setq org-agenda-files '("~/Org"
                             "~/Org/IFTTT/sleep.org.txt"
                             "~/Org/notes"))

    ;; Overwrite the current window with the agenda
    (setq org-agenda-window-setup 'other-window)

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
    (setq org-directory "~/Org")
    (setq org-default-notes-file "~/Org/refile.org")
    (setq org-capture-templates
          '(("t" "Task" entry
             (file "~/Org/refile.org")
             "* TODO %?\n%U")
            ("T" "Clock-in Task" entry
             (file "~/Org/refile.org")
             "* TODO %?\n:LOGBOOK:\n:END:\n%U"
             :clock-in t
             :clock-resume t)
            ("n" "Note" entry
             (file "~/Org/refile.org")
             "* %?\n%U")
            ("j" "Journal" entry
             (file+datetree "~/Org/diary.org")
             "* %^{Content}\n%U"
             :clock-in t
             :clock-resume t)
            ("l" "Link from Safari" entry
             (file+headline "~/Org/refile.org" "Links")
             "* %(org-mac-safari-get-frontmost-url)")
            ("L" "Link Bookmarks" entry
             (file+headline "~/Org/refile.org" "Links")
             "* [[%^{Link}][%^{Description}]]"
             :immediate-finish t)
            ("w" "New English word" checkitem
             (file+headline "~/Org/word_notes.org" "New Words")
             "- [ ] %^{Word} :: %?")
            ("c" "Contacts" entry
             (file "~/Org/contacts.org")
             "* %(org-contacts-template-name)\n:PROPERTIES:\n:EMAIL: %(org-contacts-template-email)\n:END:"
             )))

    ;; -----------------------------
    ;; Refile
    ;; -----------------------------
    (setq org-refile-targets '((nil :maxlevel . 9)
                               (private/opened-buffer-files :maxlevel . 6)
                               (org-agenda-files :maxlevel . 6)))
    (setq org-refile-allow-creating-parent-nodes 'confirm)

    ;; Exclude DONE state tasks from refile targets
    (setq org-refile-target-verify-function 'bh/verify-refile-target)

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

    (add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

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
                          (:endgroup)
                          ("TOWATCH" . ?w)
                          ("TOREAD" . ?r)))

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
       ))

    (setq org-export-backends '(beamer html latex md gfm))

    (setq org-export-with-sub-superscripts '{}
          org-export-with-section-numbers 3
          org-export-with-todo-keywords nil
          org-export-with-timestamps nil)

    (setq org-confirm-babel-evaluate t)

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
                                       (plain-list-item . auto)))
    ))
