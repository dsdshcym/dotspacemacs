;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `!distribution'. For now available distributions are `spacemacs-core'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers '((colors :variables
                                               colors-enable-nyan-cat-progress-bar t)
                                       (auto-completion :variables
                                                        auto-completion-return-key-behavior nil
                                                        auto-completion-tab-key-behavior 'complete
                                                        auto-completion-complete-with-key-sequence nil
                                                        auto-completion-enable-sort-by-usage t
                                                        auto-completion-show-snippets-in-popup t)
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
                                       common-lisp
                                       fasd
                                       osx
                                       org
                                       (c-c++
                                        :variables
                                        c-c++-enable-clang-support t)
                                       python
                                       ruby
                                       ruby-on-rails
                                       yaml
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
                                       javascript
                                       erc
                                       gnus)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(evil-escape neotree)
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
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
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key "\\"
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to miminimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server t
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   dotspacemacs-search-tools '("ag" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   )
  )

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put any
user code."
  ;; User initialization goes here
  (setq-default
   ruby-enable-ruby-on-rails-support t
   ruby-version-manager 'rvm)
  )

(defun dotspacemacs/user-config ()
  ;;   "This is were you can ultimately override default Spacemacs configuration.
  ;; This function is called at the very end of Spacemacs initialization."

  ;; ---------------------------------------------------------------------------
  ;; Basics
  ;; ---------------------------------------------------------------------------
  (set-frame-parameter nil 'fullscreen 'fullboth)
  (setq user-mail-address "dsdshcym@gmail.com")
  (setq user-full-name "Yiming Chen")

  ;; -----------------------------------------------------------
  ;; Solve the Chinese font issue
  ;; https://gist.github.com/Superbil/8554936
  ;; http://kkdevs.tumblr.com/post/38276076979/mac-os-x-org-mode
  ;; -----------------------------------------------------------
  (defun private/set-cjk-font (chinese chinese-size)
    (dolist (charset '(han cjk-misc))
      (set-fontset-font (frame-parameter nil 'font) charset
                        (font-spec :family chinese :size chinese-size))))
  (defun private/set-my-font ()
    (interactive)
    (if (eq window-system 'mac) (private/set-cjk-font "PingFang SC" 16)))
  (private/set-my-font)
  (add-to-list 'after-make-frame-functions 'private/set-my-font)

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
  ;; Gnus
  ;; ---------------------------------------------------------------------------
  (setq gnus-select-method '(nnimap "Fudan Mail"
                                    (nnimap-server-port 993)
                                    (nnimap-address "mail.fudan.edu.cn")
                                    (nnimap-stream ssl)
                                    (nnir-search-engine imap)
                                    (nnimap-authinfo-file "~/.authinfo.gpg")
                                    ))

  (setq gnus-use-cache t)

  (setq gnus-secondary-select-methods
        '(
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

  (setq send-mail-function 'smtpmail-send-it
        smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
        smtpmail-auth-credentials '(("smtp.gmail.com" 587 "dsdshcym@gmail.com" nil))
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        starttls-use-gnutls t)

  (setq gnus-use-correct-string-widths nil)

  (setq nnml-directory "~/.emacs.d/.cache/mails")
  (setq message-directory "~/.emacs.d/.cache/mails")

  ;; ---------------------------------------------------------------------------
  ;; Python / Anaconda Mode
  ;; ---------------------------------------------------------------------------
  (setq anaconda-mode-server-script "/usr/local/lib/python2.7/site-packages/anaconda_mode.py")

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

  ;; ---------------------------------------------------------------------------
  ;; paradox
  ;; ---------------------------------------------------------------------------
  (setq paradox-github-token "dd8ec7f2000aa64f8a81ceabe95aa76406d0e34d")

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

  (evil-leader/set-key
    "ww" 'ace-window
    "wW" 'other-window
    )

  ;; Define my own text object // use spacemacs|define-and-bind-text-object

  ;; ---------------------------------------------------------------------------
  ;; Ace Jump / Avy
  ;; ---------------------------------------------------------------------------
  (setq avy-all-windows t)
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

  (defun private/osx-notif (title msg &optional subtitle group-id sound)
    "Show a OS X notification. Sound can be found in ~/Library/Sounds and
    /System/Library/Sounds"
    (interactive)
    (if (eq window-system 'mac)
        (call-process-shell-command
         (concat "terminal-notifier"
                 " -title \"" title
                 "\" -message \"" msg
                 (if subtitle (concat "\" -subtitle \"" subtitle))
                 (if sound (concat "\" -sound \"" sound))
                 (if group-id (concat "\" -group \"" group-id))
                 "\" -activate " "org.gnu.Emacs"
                 " -sender " "org.gnu.Emacs")
         ;; (concat
         ;;  "osascript -e 'display notification \"" msg
         ;;  "\" with title \"" title
         ;;  (if subtitle (concat "\" subtitle \"" subtitle))
         ;;  "\" sound name \"" (if sound sound "Basso")"\"'")
         )))

  ;; ---------------------------------------------------------------------------
  ;; helm
  ;; ---------------------------------------------------------------------------
  (define-key evil-insert-state-map (kbd "M-y") 'helm-show-kill-ring)

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  ;; Fix issues with org-refile or org-jump and other helm keybindings
  (eval-after-load 'helm
    (lambda ()
      (define-key helm-map (kbd "<escape>") 'helm-keyboard-quit)
      (define-key helm-map (kbd "C-u") 'backward-kill-sentence)
      ))

  (setq helm-ff-search-library-in-sexp t)

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
    (sp-local-pair "（" "）")
    (sp-local-pair "「" "」")
    (sp-local-pair "『" "』")
    (sp-local-pair "$" "$")
    (sp-local-pair "\\[" "\\]")
    (sp-local-pair "\\(" "\\)")
    (sp-local-pair "\\{" "\\}")
    (sp-local-pair "\\left(" "\\right)")
    (sp-local-pair "\\left\\{" "\\right\\}"))

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
  (eval-after-load 'company
    (lambda ()
      (define-key company-active-map (kbd "\C-n") 'company-select-next)
      (define-key company-active-map (kbd "\C-p") 'company-select-previous)
      (define-key company-active-map (kbd "\C-w") 'evil-delete-backward-word)
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
  (evil-declare-key 'normal evil-org-mode-map "<" 'evil-shift-left)
  (evil-declare-key 'normal evil-org-mode-map ">" 'evil-shift-right)
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
  (defun opened-buffer-files ()
    "Return the list of files currently opened in emacs"
    (delq nil
          (mapcar (lambda (x)
                    (if (and (buffer-file-name x)
                             (string-match "\\.org$"
                                           (buffer-file-name x)))
                        (buffer-file-name x)))
                  (buffer-list))))
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (opened-buffer-files :maxlevel . 6)
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
  ;; Erase all reminders and rebuilt reminders for today from the agenda
  (defun bh/org-agenda-to-appt ()
    (interactive)
    (setq appt-time-msg-list nil)
    (org-agenda-to-appt))

  ;; Rebuild the reminders everytime the agenda is displayed
  (add-hook 'org-finalize-agenda-hook 'bh/org-agenda-to-appt 'append)

  (defun private/appt-display (min-to-app new-time msg)
    (private/osx-notif "Org Agenda Appointment" msg (format "Appointment in %s minute(s)" min-to-app) "1")
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

  ;; Showing source block syntax highlighting
  (setq org-src-fontify-natively t)

  (setq org-export-coding-system 'utf-8)
  (set-charset-priority 'unicode)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))

  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  ;; backwards compatibility as default-buffer-file-coding-system
  ;; is deprecated in 23.2.
  (if (boundp 'buffer-file-coding-system)
      (setq-default buffer-file-coding-system 'utf-8)
    (setq default-buffer-file-coding-system 'utf-8))

  ;; Treat clipboard input as UTF-8 string first; compound text next, etc.
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

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
