;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/Github/dotfiles/spacemacs/layers/")
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers '((colors :variables
                                               colors-enable-nyan-cat-progress-bar t)
                                       (auto-completion :variables
                                                        auto-completion-return-key-behavior nil
                                                        auto-completion-tab-key-behavior 'complete
                                                        auto-completion-complete-with-key-sequence nil
                                                        auto-completion-enable-sort-by-usage t)
                                       (ibuffer
                                        :variables ibuffer-group-buffers-by 'projects)
                                       (shell
                                        :variables
                                        shell-default-shell 'ansi-term
                                        shell-default-term-shell "/usr/local/bin/zsh")
                                       git
                                       github
                                       version-control
                                       emacs-lisp
                                       fasd
                                       osx
                                       org
                                       python
                                       ruby
                                       latex
                                       markdown
                                       pandoc
                                       syntax-checking
                                       html
                                       osx-dictionary
                                       org-tree-slide
                                       eyebrowse
                                       sql
                                       dash
                                       javascript)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(evil-escape neotree company-quickhelp)
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages nil))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; Either `vim' or `emacs'. Evil is always enabled but if the variable
   ;; is `emacs' then the `holy-mode' is enabled at startup.
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer.
   dotspacemacs-verbose-loading t
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed.
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(solarized-dark
                         zenburn
                         solarized-light
                         leuven
                         monokai)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Input"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it.
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f) is replaced.
   dotspacemacs-use-ido nil
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state t
   ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
   ;; the commands bound to the current keystrokes.
   dotspacemacs-guide-key-delay 0.4
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil ;; to boost the loading time.
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up.
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup t
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX."
   dotspacemacs-fullscreen-use-non-native t
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen.
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible value is `all',
   ;; `current' or `nil'. Default is `all'
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server t
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   dotspacemacs-search-tools '("ag" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   dotspacemacs-default-package-repository nil
   )
  ;; User initialization goes here
  (setq-default
   ruby-enable-ruby-on-rails-support t
   ruby-version-manager 'rvm)
  )

(defun dotspacemacs/config ()
  ;;   "This is were you can ultimately override default Spacemacs configuration.
  ;; This function is called at the very end of Spacemacs initialization."

  ;; ---------------------------------------------------------------------------
  ;; Basics
  ;; ---------------------------------------------------------------------------

  ;; -----------------------------------------------------------
  ;; Solve the Chinese font issue
  ;; https://gist.github.com/Superbil/8554936
  ;; http://kkdevs.tumblr.com/post/38276076979/mac-os-x-org-mode
  ;; -----------------------------------------------------------
  (defun private/set-font (chinese chinese-size)
    (dolist (charset '(han cjk-misc))
      (set-fontset-font (frame-parameter nil 'font) charset
                        (font-spec :family chinese :size chinese-size))))
  (if (eq window-system 'mac) (private/set-font "Origin Han Sans UI HW PRChinaGB" 16))

  (setq sentence-end-double-space nil)
  (define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
  (setq vc-follow-symlinks t)
  (setq global-hl-line-mode nil)

  (setq mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control))))
  (setq flyspell-issue-welcome-flag nil) ;; fix flyspell problem
  (setq flyspell-mode nil)

  ;; --------------------------
  ;; kill-buffer without prompt
  ;; --------------------------
  (setq kill-buffer-query-functions
        (remq 'process-kill-buffer-query-function
              kill-buffer-query-functions))


  ;; ---------------------------------------------------------------------------
  ;; osx-dictionary
  ;; ---------------------------------------------------------------------------
  (add-hook 'osx-dictionary-mode-hook
            (lambda ()
              (setq show-trailing-whitespace nil)))

  ;; ---------------------------------------------------------------------------
  ;; SQL
  ;; ---------------------------------------------------------------------------
  (add-hook 'sql-interactive-mode-hook
            (lambda ()
              (toggle-truncate-lines t)))

  ;; (evil-leader/set-key-for-mode 'sql-mode
  ;;   "msb" 'sql-send-buffer
  ;;   "msr" 'sql-send-region
  ;;   "mss" 'sql-send-string
  ;;   "msp" 'sql-send-paragraph
  ;;   "msi" 'sql-mysql)

  ;; ---------------------------------------------------------------------------
  ;; paradox
  ;; ---------------------------------------------------------------------------
  (setq paradox-github-token "dd8ec7f2000aa64f8a81ceabe95aa76406d0e34d")

  ;; ---------------------------------------------------------------------------
  ;; projectile
  ;; ---------------------------------------------------------------------------
  ;; (evil-leader/set-key "pp" 'projectile-persp-switch-project)

  ;; ---------------------------------------------------------------------------
  ;; magit
  ;; ---------------------------------------------------------------------------
  (setq magit-repository-directories '("~/GitHub/"))
  (setq magit-use-overlays nil)

  ;; ---------------------------------------------------------------------------
  ;; C/C++ indent style
  ;; ---------------------------------------------------------------------------
  (setq-default c-default-style "java")
  (setq gdb-many-windows t
        gdb-show-main t)

  ;; ---------------------------------------------------------------------------
  ;; Google Translate
  ;; ---------------------------------------------------------------------------
  (setq google-translate-default-source-language "auto"
        google-translate-default-target-language "zh")

  ;; ---------------------------------------------------------------------------
  ;; golden-ratio-mode
  ;; ---------------------------------------------------------------------------
  ;; (golden-ratio-mode 1)

  ;; ---------------------------------------------------------------------------
  ;; image-mode
  ;; ---------------------------------------------------------------------------
  (evil-define-key 'normal image-mode-map "h" 'image-backward-hscroll)
  (evil-define-key 'normal image-mode-map "l" 'image-forward-hscroll)
  (evil-define-key 'normal image-mode-map "j" 'image-next-line)
  (evil-define-key 'normal image-mode-map "k" 'image-previous-line)
  (evil-define-key 'normal image-mode-map (kbd "\C-d") 'image-scroll-up)
  (evil-define-key 'normal image-mode-map (kbd "\C-u") 'image-scroll-down)


  ;; ---------------------------------------------------------------------------
  ;; markdown
  ;; ---------------------------------------------------------------------------
  (defun markdown-preview-file ()
    "use Marked 2 to preview the current file"
    (save-buffer)
    (interactive)
    (shell-command
     (format "open -a 'Marked 2.app' %s"
             (shell-quote-argument (buffer-file-name)))))

  (evil-leader/set-key-for-mode 'markdown-mode "mp" 'markdown-preview-file)

  (setq-default markdown-enable-math t)

  ;; ---------------------------------------------------------------------------
  ;; evil
  ;; ---------------------------------------------------------------------------
  (setq evil-want-fine-undo 'No)
  (define-key evil-normal-state-map (kbd "Y") (kbd "y$"))
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

  ;; (define-key isearch-mode-map (kbd "<escape>") 'isearch-cancel)

  (define-key evil-insert-state-map (kbd "C-u")
    (lambda ()
      (interactive)
      (evil-delete (point-at-bol) (point))))

  ;; (global-evil-matchit-mode 1)

  (evil-leader/set-key
    ;; "wo" 'delete-other-windows
    ;; "wO" 'other-frame
    ;; "wS" (lambda () (interactive) (split-window-below-and-focus) (helm-mini))
    ;; "wV" (lambda () (interactive) (split-window-right-and-focus) (helm-mini))
    "ww" 'ace-window
    "wW" 'other-window
    ;; "/" 'helm-ag
    )

  ;; (evil-leader/set-key "SPC" 'evil-ace-jump-char-mode)

  ;; Define my own text object // use spacemacs|define-and-bind-text-object

  ;; ---------------------------------------------------------------------------
  ;; Ace Jump
  ;; ---------------------------------------------------------------------------
  (setq ace-jump-mode-scope 'window)

  (setq aw-background nil)

  ;; ---------------------------------------------------------------------------
  ;; Own Customizations
  ;; ---------------------------------------------------------------------------
  ;; A hack to get english input mode in normal mode by simulating keystrokes
  ;; when exit insert mode
  (defvar private/toggle-rimeime-mode nil)

  (defun private/simulate-shift ()
    (call-process-shell-command "echo \"tell application \\\"System Events\\\"
                                        key code 56
                                        end tell\" | osascript")
    )

  (defun private/toggle-rime ()
    (if (symbol-value private/toggle-rimeime-mode)
        (progn
          (remove-hook 'evil-insert-state-entry-hook 'private/simulate-shift)
          (setq private/toggle-rimeime-mode nil))
      (progn
        (add-hook 'evil-insert-state-entry-hook 'private/simulate-shift)
        (setq private/toggle-rimeime-mode t)))
    )

  (spacemacs|add-toggle rimeime
                        :if (eq window-system 'mac)
                        :status private/toggle-rimeime-mode
                        :on (private/toggle-rime)
                        :off (private/toggle-rime)
                        :documentation "Use Chinese Input when exiting the insert mode"
                        :evil-leader "tc")

  ;; override spacemacs/alternate-buffer to switch between
  ;; current and last unvisible buffer
  (defun spacemacs/alternate-buffer ()
    "Switch back and forth between current and last buffer."
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) nil)))

  (setq switch-to-visible-buffer nil)

  (defun private/osx-notif (title msg &optional subtitle sound)
    "Show a OS X notification. Sound can be found in ~/Library/Sounds and
/System/Library/Sounds"
    (interactive)
    (if (eq window-system 'mac)
        (call-process-shell-command
         (concat
          "osascript -e 'display notification \"" msg
          "\" with title \"" title
          (if subtitle (concat "\" subtitle \"" subtitle))
          "\" sound name \"" (if sound sound "Basso")"\"'"))))

  ;; ---------------------------------------------------------------------------
  ;; helm
  ;; ---------------------------------------------------------------------------
  (define-key evil-insert-state-map (kbd "M-y") 'helm-show-kill-ring)

  (global-set-key "\M-x" 'helm-M-x)

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  ;; Fix issues with org-refile or org-jump and other helm keybindings
  (eval-after-load 'helm
    (lambda ()
      (define-key helm-map (kbd "<escape>") 'helm-keyboard-quit)
      ;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
      ;; (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
      ;; (define-key helm-map (kbd "C-z") 'helm-select-action)
      (define-key helm-map (kbd "C-u") 'backward-kill-sentence)
      ))

  ;; (setq helm-split-window-in-side-p t)
  (setq helm-ff-search-library-in-sexp t)
  ;; (setq helm-split-window-default-side 'other)

  ;; ---------------------------------------------------------------------------
  ;; OS X
  ;; ---------------------------------------------------------------------------
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'meta)
  (setq mac-pass-command-to-system nil)

  ;; ---------------------------------------------------------------------------
  ;; shell
  ;; ---------------------------------------------------------------------------
  (setq eshell-cmpl-ignore-case t)
  (ansi-color-for-comint-mode-on)
  (setq pcomplete-ignore-case t)

  ;; ---------------------------------------------------------------------------
  ;; Term Mode
  ;; ---------------------------------------------------------------------------
  (defun private/open-ansi-term ()
    (interactive)
    ;; (split-window-right-and-focus)
    (if (string-match "\/ssh:.*" (buffer-file-name)) (eshell)
      (let* (
             (term-buffer-name-var (concat (projectile-project-name) "-ansi-term"))
             (term-buffer-name (concat "*" term-buffer-name-var "*"))
             )
        (if (get-buffer term-buffer-name)
            (switch-to-buffer term-buffer-name)
          (ansi-term "/usr/local/bin/zsh" term-buffer-name-var))
        (evil-append nil))))

  (evil-leader/set-key
    "ot" 'private/open-ansi-term)

  ;; (setq multi-term-program "/usr/local/bin/zsh")
  (evil-declare-key 'insert term-raw-map (kbd "C-p") 'term-send-raw)
  (evil-declare-key 'insert term-raw-map (kbd "C-n") 'term-send-raw)
  (evil-declare-key 'normal term-raw-map "p" 'term-paste)
  (evil-declare-key 'normal term-raw-map (kbd "RET") 'term-send-return)
  (add-hook 'term-mode-hook
            (lambda ()
              (setq term-buffer-maximum-size 10000)
              (setq show-trailing-whitespace nil)))

  ;; ---------------------------------------------------------------------------
  ;; trailing-whitespace
  ;; ---------------------------------------------------------------------------
  (setq-default show-trailing-whitespace t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; ---------------------------------------------------------------------------
  ;; smartparens
  ;; ---------------------------------------------------------------------------
  ;; (setq sp-autoescape-string-quote nil)
  ;; (setq sp-autoinsert-pair nil)

  (define-key evil-insert-state-map (kbd "C-s") 'sp-forward-slurp-sexp)
  (define-key evil-insert-state-map (kbd "S-C-S") 'sp-backward-slurp-sexp)

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
    (sp-local-pair "$" "$")
    (sp-local-pair "\\[" "\\]")
    (sp-local-pair "\\(" "\\)")
    (sp-local-pair "\\{" "\\}")
    (sp-local-pair "\\left(" "\\right)")
    (sp-local-pair "\\left\\{" "\\right\\}"))

  ;; (add-to-hooks #'smartparens-mode '(text-mode-hook prog-mode-hook org-mode-hook))


  ;; ---------------------------------------------------------------------------
  ;; web-mode
  ;; ---------------------------------------------------------------------------

  ;; ---------------------------------------------------------------------------
  ;; yaml-mode
  ;; ---------------------------------------------------------------------------
  (use-package yaml-mode :defer t)

  ;; ---------------------------------------------------------------------------
  ;; company-mode
  ;; ---------------------------------------------------------------------------
  ;; (global-company-mode)

  (defun private/company-complete-common-or-complete (&optional arg)
  (interactive "p")
  (when (company-manual-begin)
    (let ((tick (buffer-chars-modified-tick)))
      (call-interactively 'company-complete-common)
      (when (eq tick (buffer-chars-modified-tick))
        (call-interactively 'company-complete-selection)))))

  (eval-after-load 'company
    (lambda ()
      (define-key company-active-map (kbd "\C-n") 'company-select-next)
      (define-key company-active-map (kbd "\C-p") 'company-select-previous)
      (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
      (define-key company-active-map (kbd "<tab>") 'private/company-complete-common-or-complete)
      ;; (define-key company-active-map (kbd "<RET>") (lambda () (interactive) (progn (company-complete-selection) (newline-and-indent))))
      ;; (setq company-auto-complete nil)
      (setq company-global-modes '(not shell-mode))
      ))

  ;; --------------------------------------------------------------------
  ;; org-mode
  ;;
  ;; Functions which name starts with "bh" are from
  ;; http://doc.norang.ca/org-mode.html
  ;; --------------------------------------------------------------------
  (setq spacemacs-mode-line-org-clock-current-taskp t)

  (evil-declare-key 'normal evil-org-mode-map "o" 'evil-open-below)
  (evil-declare-key 'normal evil-org-mode-map "O" 'evil-open-above)
  (evil-declare-key 'normal evil-org-mode-map (kbd "C-o") (lambda () (interactive) (evil-end-of-line) (org-insert-heading-respect-content) (evil-append nil)))
  (evil-declare-key 'normal evil-org-mode-map (kbd "M-o") (lambda () (interactive) ((lambda () (evil-end-of-line) (evil-append nil) (org-meta-return) ))))
  (evil-declare-key 'insert evil-org-mode-map (kbd "C-o") 'org-insert-heading-respect-content)
  (evil-declare-key 'insert evil-org-mode-map (kbd "M-o") (lambda () (interactive) ((lambda () (evil-end-of-line) (evil-append nil) (org-meta-return) ))))
  (evil-declare-key 'normal evil-org-mode-map (kbd "C-S-o") (lambda () (interactive) (org-insert-todo-heading-respect-content) (evil-append nil)))
  (evil-declare-key 'insert evil-org-mode-map (kbd "C-S-o") 'org-insert-todo-heading-respect-content)
  (evil-declare-key 'normal evil-org-mode-map (kbd "<RET>") 'org-open-at-point)

  (evil-declare-key 'normal evil-org-mode-map "gj" 'outline-next-visible-heading)
  (evil-declare-key 'normal evil-org-mode-map "gk" 'outline-previous-visible-heading)
  (evil-declare-key 'normal evil-org-mode-map "gh" 'org-backward-heading-same-level)
  (evil-declare-key 'normal evil-org-mode-map "gl" 'org-forward-heading-same-level)

  (evil-leader/set-key
    "aa" 'org-agenda-list
    "oa" 'org-agenda
    "ol" 'org-store-link
    "os" 'org-iswitchb
    "og" 'org-clock-goto
    "oo" 'org-clock-out
    "op" 'org-pomodoro
    "oc" 'org-capture
    "oj" '(lambda () (interactive) (org-refile (universal-argument))))

  (evil-leader/set-key-for-mode 'org-mode
    "ns" 'org-narrow-to-subtree
    "nb" 'org-narrow-to-block
    "ne" 'org-narrow-to-element
    "m/" 'org-sparse-tree)

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
          ("l" "Link Bookmarks" entry
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
  ;; (defun opened-buffer-files ()
  ;;   "Return the list of files currently opened in emacs"
  ;;   (delq nil
  ;;         (mapcar (lambda (x)
  ;;                   (if (and (buffer-file-name x)
  ;;                            (string-match "\\.org$"
  ;;                                          (buffer-file-name x)))
  ;;                       (buffer-file-name x)))
  ;;                 (buffer-list))))
  (setq org-refile-targets '((nil :maxlevel . 9)
                             ;; (opened-buffer-files :maxlevel . 6)
                             (org-agenda-files :maxlevel . 6)))
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  ;; Exclude DONE state tasks from refile targets
  (defun bh/verify-refile-target ()
    "Exclude todo keywords with a done state from refile targets"
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))

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
           (and
            (boundp 'org-pomodoro-state)
            (equal org-pomodoro-state :none))
           ;; bh/keep-clock-running
           (not org-clock-clocking-in)
           ;; (marker-buffer org-clock-default-task)
           (not org-clock-resolving-clocks-due-to-idleness))
      (bh/clock-in-parent-task)))

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

  ;; Do not prompt to confirm evaluation
  ;; This may be dangerous - make sure you understand the consequences
  ;; of setting this -- see the docstring for details
  (setq org-confirm-babel-evaluate nil)

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
          ("keywordstyle" "\\color{black}\\bfseries")))
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
  ;; Erase all reminders and rebuilt reminders for today from the agenda
  (defun bh/org-agenda-to-appt ()
    (interactive)
    (setq appt-time-msg-list nil)
    (org-agenda-to-appt))

  ;; Rebuild the reminders everytime the agenda is displayed
  (add-hook 'org-finalize-agenda-hook 'bh/org-agenda-to-appt 'append)

  (defun private/appt-display (min-to-app new-time msg)
    (private/osx-notif "Org Agenda Appointment" msg (format "Appointment in %s minute(s)" min-to-app))
    ;; (appt-disp-window min-to-app new-time msg)
    )

  (setq appt-disp-window-function (function private/appt-display))
  (setq appt-delete-window-function nil)

  ;; This is at the end of my .emacs - so appointments are set up when Emacs starts
  (bh/org-agenda-to-appt)

  ;; Activate appointments so we get notifications
  (appt-activate t)

  ;; If we leave Emacs running overnight - reset the appointments one minute after midnight
  (run-at-time "24:01" nil 'bh/org-agenda-to-appt)

  ;; -----------------------------
  ;; MobileOrg
  ;; -----------------------------
  ;; ;; Set to the name of the file where new notes will be stored
  ;; (setq org-mobile-inbox-for-pull "~/Org/refile.org")
  ;; ;; Set to <your Dropbox root directory>/MobileOrg.
  ;; (setq org-mobile-directory "~/Dropbox/应用/MobileOrg")
  ;; (run-at-time "01:00" 86400 '(lambda ()
  ;;                               (org-save-all-org-buffers)
  ;;                               (org-mobile-push)))


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
  (setq org-modules '(org-bbdb
                      org-bibtex
                      org-docview
                      org-crypt
                      org-gnus
                      org-id
                      org-info
                      org-habit
                      org-irc
                      org-mhe
                      org-rmail
                      org-w3m))

  ;; position the habit graph on the agenda to the right of the default
  (setq org-habit-graph-column 50)

  (run-at-time "06:00" 86400 '(lambda () (setq org-habit-show-habits t)))

  ;; ;; Insert inactive timestamps and exclude from export
  ;; (defvar bh/insert-inactive-timestamp t)

  ;; (defun bh/toggle-insert-inactive-timestamp ()
  ;;   (interactive)
  ;;   (setq bh/insert-inactive-timestamp (not bh/insert-inactive-timestamp))
  ;;   (message "Heading timestamps are %s" (if bh/insert-inactive-timestamp "ON" "OFF")))

  ;; (defun bh/insert-inactive-timestamp ()
  ;;   (interactive)
  ;;   (org-insert-time-stamp nil t t nil nil nil))

  ;; (defun bh/insert-heading-inactive-timestamp ()
  ;;   (save-excursion
  ;;     (when bh/insert-inactive-timestamp
  ;;       (org-return)
  ;;       (org-cycle)
  ;;       (bh/insert-inactive-timestamp))))

  ;; (add-hook 'org-insert-heading-hook 'bh/insert-heading-inactive-timestamp 'append)
  ;; (evil-leader/set-key "ot" 'bh/insert-inactive-timestamp)

  ;; Showing source block syntax highlighting
  (setq org-src-fontify-natively t)

  (setq org-export-coding-system 'utf-8)
  ;; (prefer-coding-system 'utf-8) ;; commented since it's default in spacemacs
  (set-charset-priority 'unicode)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))

  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  (setq org-cycle-separator-lines 0)
  (setq org-blank-before-new-entry '((heading . nil)
                                     (plain-list-item . auto)))

  (setq calendar-latitude 31.23)
  (setq calendar-longitude 121.47)
  (setq calendar-location-name "Shanghai")

  )

;; Custom variables
;; ----------------

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-case-fold-search nil)
 '(ahs-default-range (quote ahs-range-whole-buffer))
 '(ahs-idle-interval 0.25)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil)
 '(magit-use-overlays nil)
 '(paradox-automatically-star t)
 '(paradox-github-token t)
 '(ring-bell-function (quote ignore) t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
