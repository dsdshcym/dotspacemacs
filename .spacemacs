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
                                       gnus
                                       dsdshcym)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(evil-escape neotree evil-mc)
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
   dotspacemacs-enable-paste-micro-state t
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

  (private/set-my-font)
  (add-to-list 'after-make-frame-functions 'private/set-my-font)

  (setq sentence-end-double-space nil)
  (setq vc-follow-symlinks t)

  (add-hook 'view-mode-hook (lambda ()
                              (setq show-trailing-whitespace nil)))

  ;; --------------------------
  ;; kill-buffer without prompt
  ;; --------------------------
  (setq kill-buffer-query-functions
        (remq 'process-kill-buffer-query-function
              kill-buffer-query-functions))

  ;; ---------------------------------------------------------------------------
  ;; Python / Anaconda Mode
  ;; ---------------------------------------------------------------------------
  (setq anaconda-mode-server-script "/usr/local/lib/python2.7/site-packages/anaconda_mode.py")
  (add-to-list 'dash-at-point-mode-alist '(python-mode . "python2,python3,django,twisted,sphinx,flask,tornado,sqlalchemy,numpy,scipy,saltcvp"))

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
  (spacemacs/set-google-translate-languages "auto" "zh")

  ;; ---------------------------------------------------------------------------
  ;; markdown
  ;; ---------------------------------------------------------------------------
  (setq-default markdown-enable-math t)

  ;; Define my own text object // use spacemacs|define-and-bind-text-object

  ;; ---------------------------------------------------------------------------
  ;; Ace Jump / Avy
  ;; ---------------------------------------------------------------------------
  (setq avy-all-windows t)
  (setq aw-background nil)

  ;; ---------------------------------------------------------------------------
  ;; Own Customizations
  ;; ---------------------------------------------------------------------------
  ;; override spacemacs/alternate-buffer to switch between
  ;; current and last unvisible buffer
  (defun spacemacs/alternate-buffer ()
    "Switch back and forth between current and last buffer."
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) nil)))

  (setq switch-to-visible-buffer nil)

  ;; ---------------------------------------------------------------------------
  ;; helm
  ;; ---------------------------------------------------------------------------
  (define-key evil-insert-state-map (kbd "M-y") 'helm-show-kill-ring)

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

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
  ;; org-mode
  ;; ---------------------------------------------------------------------------
  ;; This is at the end of my .emacs - so appointments are set up when Emacs starts
  (bh/org-agenda-to-appt)

  ;; Activate appointments so we get notifications
  (appt-activate t)
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
