;;; funcs.el --- dsdshcym Layer packages File for Spacemacs
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

;; -----------------------------------------------------------
;; Private Keyboard Macros
;; -----------------------------------------------------------
(fset 'leetcode-commit
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([105 83 111 108 118 101 100 32 escape 47 97 108 103 111 13 102 47 119 89 96 96 96 96 112 70 46 68 86 58 115 47 95 47 32 47 103 return 86 32 58 99 97 112 105 116 97 108 105 122 101 45 114 101 103 105 111 110 13] 0 "%d")) arg)))

;; -----------------------------------------------------------
;; Hard-mode
;; -----------------------------------------------------------
(defvar private/hardtime-mode nil)

(defun private/turn-on-hardtime-mode ()
  (interactive "P")
  (let ((keys '("h" "j" "k" "l")))
    (dolist (key keys)
      (define-key evil-normal-state-map (kbd key) 'private/nop)
      (define-key evil-visual-state-map (kbd key) 'private/nop)))
  (setq private/hardtime-mode t)
  )

(defun private/turn-off-hardtime-mode ()
  (interactive "P")
  (let ((keys '("h" "j" "k" "l")))
    (dolist (key keys)
      (define-key evil-normal-state-map (kbd key) nil)
      (define-key evil-visual-state-map (kbd key) nil)))
  (setq private/hardtime-mode nil)
  )

(defun private/nop ()
  (interactive)
  (message "Stop using hjkl!")
  )

;; -----------------------------------------------------------
;; Dired
;; -----------------------------------------------------------
(defun private/dired-kill-filename-for-visit ()
  (interactive)
  (kill-new (dired-get-file-for-visit)))

;; -----------------------------------------------------------
;; Ruby mode helper functions
;; -----------------------------------------------------------
(defun ruby-send-buffer ()
  "Send the current buffer to the inferior Ruby process."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (ruby-send-region (point-min) (point-max)))))

(defun ruby-send-buffer-and-go ()
  "Send the current buffer to the inferior Ruby process."
  (interactive)
  (ruby-send-buffer)
  (ruby-switch-to-inf t))

;; -----------------------------------------------------------
;; Solve the Chinese font issue
;; https://gist.github.com/Superbil/8554936
;; http://kkdevs.tumblr.com/post/38276076979/mac-os-x-org-mode
;; -----------------------------------------------------------
(defun private/set-cjk-font (chinese chinese-size)
  (dolist (charset '(han cjk-misc))
    (set-fontset-font t charset
                      (font-spec :family chinese :size chinese-size))))
(defun private/set-my-font ()
  (interactive)
  (private/set-cjk-font "Noto Sans Mono CJK SC" 34))

;; A hack to get english input mode in normal mode by simulating keystrokes
;; when exit insert mode
(defvar private/toggle-rimeime-mode nil)

(defun private/turn-off-rimeime-mode ()
  (progn
    (fcitx-evil-turn-off)
    (setq private/toggle-rimeime-mode nil)))

(defun private/turn-on-rimeime-mode ()
  (progn
    (fcitx-evil-turn-on)
    (setq private/toggle-rimeime-mode t)))

(defun private/notification (title msg &optional subtitle group-id sound)
  (interactive)
  (if (spacemacs/system-is-mac)
      (call-process-shell-command
       (concat "terminal-notifier"
               " -title \"" title
               "\" -message \"" msg
               (if subtitle (concat "\" -subtitle \"" subtitle))
               (if sound (concat "\" -sound \"" sound))
               (if group-id (concat "\" -group \"" group-id))
               "\" -activate " "org.gnu.Emacs"
               " -sender " "org.gnu.Emacs")
       )
    )
  (if (spacemacs/system-is-linux)
      (call-process-shell-command
       (concat "notify-send" " \"" title "\" \"" msg "\"")
       )
    )
  )

;; --------------------------------------------------------------------
;; org-mode helper functions
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; function to wrap blocks of text in org templates
;; e.g. latex or src etc
;; See http://pragmaticemacs.com/emacs/wrap-text-in-an-org-mode-block/
;; --------------------------------------------------------------------
(defun private/org-begin-template ()
  "Make a template at point."
  (interactive)
  (if (org-at-table-p)
      (call-interactively 'org-table-rotate-recalc-marks)
    (let* ((choices '(("s" . "SRC")
                      ("e" . "EXAMPLE")
                      ("q" . "QUOTE")
                      ("v" . "VERSE")
                      ("c" . "CENTER")
                      ("l" . "LaTeX")
                      ("h" . "HTML")
                      ("a" . "ASCII")))
           (key
            (key-description
             (vector
              (read-key
               (concat (propertize "Template type: " 'face 'minibuffer-prompt)
                       (mapconcat (lambda (choice)
                                    (concat (propertize (car choice) 'face 'font-lock-type-face)
                                            ": "
                                            (cdr choice)))
                                  choices
                                  ", ")))))))
      (let ((result (assoc key choices)))
        (when result
          (let ((choice (cdr result)))
            (cond
             ((region-active-p)
              (let ((start (region-beginning))
                    (end (region-end)))
                (goto-char end)
                (insert "\n#+END_" choice)
                (goto-char start)
                (insert "#+BEGIN_" choice "\n")))
             (t
              (insert "#+BEGIN_" choice "\n")
              (save-excursion (insert "\n#+END_" choice))))))))))

(defun private/opened-buffer-files ()
  "Return the list of files currently opened in emacs"
  (delq nil
        (mapcar (lambda (x)
                  (if (and (buffer-file-name x)
                           (string-match "\\.org$"
                                         (buffer-file-name x)))
                      (buffer-file-name x)))
                (buffer-list))))

(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in. Skips capture tasks,
    projects, and subprojects. Switch projects and subprojects from NEXT back to
    TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (bh/is-task-p))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (bh/is-project-p))
      "TODO"))))

(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          )))))

(defun bh/clock-out-maybe ()
  (when (and
         (or
          (not
           (boundp 'org-pomodoro-state))
           (equal org-pomodoro-state :none))
         ;; bh/keep-clock-running
         (not org-clock-clocking-in)
         ;; (marker-buffer org-clock-default-task)
         (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-parent-task)))

(defun bh/org-agenda-to-appt ()
  "Erase all reminders and rebuilt reminders for today from the agenda"
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

(defun private/appt-display (min-to-app new-time msg)
  (private/notification "Org Agenda Appointment" msg (format "Appointment in %s minute(s)" min-to-app) "1")
  (appt-disp-window min-to-app new-time msg)
  )

;; --------------------------------------------------------------------
;; org capture in elfeed
;; --------------------------------------------------------------------
(defun private/org-elfeed-entry-store-link ()
  (when elfeed-show-entry
    (let* ((link (elfeed-entry-link elfeed-show-entry))
           (title (elfeed-entry-title elfeed-show-entry)))
      (org-store-link-props
       :link link
       :description title)
      )))

(add-hook 'org-store-link-functions 'private/org-elfeed-entry-store-link)
