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
        gnus
        ))

;; List of packages to exclude.
(setq dsdshcym-excluded-packages '())

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
           spacemacs/describe-variable))
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

(defun dsdshcym/post-init-gnus ()
  (progn
    (setq gnus-use-cache t)

    (setq gnus-secondary-select-methods
          '(
            (nnimap "Fudan Mail"
                    (nnimap-server-port 993)
                    (nnimap-address "mail.fudan.edu.cn")
                    (nnimap-stream ssl)
                    (nnir-search-engine imap)
                    (nnimap-authinfo-file "~/.authinfo.gpg")
                    )
            (nnimap "Gmail"
                    (nnimap-address "imap.gmail.com")
                    (nnimap-server-port 993)
                    (nnimap-stream ssl)
                    (nnir-search-engine imap)
                    (nnimap-authinfo-file "~/.authinfo.gpg")
                    ;; @see http://www.gnu.org/software/emacs/manual/html_node/gnus/Expiring-Mail.html
                    ;; press 'E' to expire email
                    (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
                    (nnmail-expiry-wait 90)
                    )
            )
          )

    ;; This is needed to allow msmtp to do its magic:
    (setq message-sendmail-f-is-evil 't)

    ;;need to tell msmtp which account we're using
    (setq message-sendmail-extra-arguments '("--read-envelope-from"))

    (setq message-send-mail-function 'message-send-mail-with-sendmail)
    ;; we substitute sendmail with msmtp
    (setq sendmail-program "/usr/local/bin/msmtp")
    ;;need to tell msmtp which account we're using
    (setq message-sendmail-extra-arguments '("-a" "gmail"))
    ;; you might want to set the following too
    (setq mail-host-address "gmail.com")
    (setq user-full-name "Yiming Chen")
    (setq user-mail-address "dsdshcym@gmail.com")

    (setq gnus-use-correct-string-widths nil)

    (setq nnml-directory "~/.emacs.d/.cache/mails")
    (setq message-directory "~/.emacs.d/.cache/mails")

    ;; Do not archive sent messages
    (setq gnus-message-archive-group nil)
    )
  )

(defun dsdshcym/post-init-org ()
  (progn
    ;; --------------------------------------------------------------------
    ;; Functions which name starts with "bh" are from
    ;; http://doc.norang.ca/org-mode.html
    ;; --------------------------------------------------------------------
    (mapc #'evil-declare-ignore-repeat
          '(org-cycle))

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

    (setq org-agenda-ndays 2)

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
             "* %(org-mac-safari-get-frontmost-url)"
             :immediate-finish t)
            ("L" "Link Bookmarks" entry
             (file+headline "~/Org/refile.org" "Links")
             "* [[%^{Link}][%^{Description}]]"
             :immediate-finish t)
            ("w" "New English word" checkitem
             (file+headline "~/Org/word_notes.org" "New Words")
             "- [ ] %^{Word} :: %?")
            ("h" "Habit" entry (file "~/git/org/refile.org")
             "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:")
            ))

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
                        org-gnus
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
                        org-mac-link))
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
