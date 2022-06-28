;;; init.el --- Emacs Rice

;;; Commentary:
;; My Emacs configuration in org mode

;;; Code:

(setq gc-cons-threshold (* 50 1000 1000))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time))) gcs-done)))

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

    ;;; Install quelpa for external repo fetching
(use-package quelpa-use-package
  :init (setq quelpa-update-melpa-p nil)
  :config (quelpa-use-package-activate-advice))

;; Change the user-emacs-directory to keep unwanted things out of ~/.config/emacs
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering)

(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))

(load custom-file 'noerror 'nomessage)

(setq inhibit-startup-message t)
(setq visible-bell t)

(use-package beacon)
(beacon-mode 1)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(global-hl-line-mode +1)
(global-visual-line-mode 1)
(blink-cursor-mode -1)

(setq scroll-conservatively 101) ;; value greater than 100 gets rid of half page jumping
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq use-dialog-box nil) ;; Don't pop up UI dialogs when prompting
(setq isearch-allow-scroll t)
(setq undo-outer-limit 104857600) ;; set the size of output in bytes

(global-display-line-numbers-mode t)
(column-number-mode)
(setq display-line-numbers-type 'relative)
(use-package command-log-mode)

(dolist (mode '(term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq large-file-warning-threshold nil)

(setq vc-follow-symlinks t)

(setq ad-redefinition-action 'accept)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'foo-mode-hook #'rainbow-delimiters-mode))

(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-light-medium t))

;; Set the font face based on platform
(pcase system-type
  ((or 'gnu/linux 'windows-nt 'cygwin)
   (set-face-attribute 'default nil
                       :font "Fantasque Sans Mono"
                       :weight 'light
                       :height 110))
  ('darwin (set-face-attribute 'default nil :font "Fira Mono" :height 110)))

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil
                    :font "Fantasque Sans Mono"
                    :weight 'light
                    :height 110)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil
                    ;; :font "Cantarell"
                    :font "Linux Biolinum"
                    :height 110
                    :weight 'light)

; (set-fontset-font "fontset-default" 'arabic (font-spec :family "Janna LT" :height 110))
(set-fontset-font "fontset-default" 'arabic (font-spec :family "Amiri Quran" :height 110))

(use-package emojify
  :ensure t
  :hook
  (after-init . global-emojify-mode))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-completion)
(all-the-icons-completion-mode)

(setq display-time-format "%l:%M %p %b %y"
      display-time-default-load-average nil)

(use-package diminish
  :ensure t)

;; You must run (all-the-icons-install-fonts) one time after
;; installing this package!

(use-package minions
  :hook (doom-modeline-mode . minions-mode))

(use-package doom-modeline
  :hook (after-init . doom-modeline-init)
  :custom-face
  (mode-line ((t (:height 0.90))))
  (mode-line-inactive ((t (:height 0.90))))
  :custom
  (doom-modeline-height 26)
  (doom-modeline-bar-width 6)
  (doom-modeline-lsp t)
  (doom-modeline-github nil)

  ;; Whether display the mu4e notifications. It requires `mu4e-alert' package.
  (doom-modeline-mu4e t)
  ;; also enable the start of mu4e-alert
  (mu4e-alert-enable-mode-line-display)

  (doom-modeline-persp-name nil)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)

  ;; Whether to use hud instead of default bar. It's only respected in GUI.
  (doom-modeline-hud nil)

  ;; Whether display icons in the mode-line.
  ;; While using the server mode in GUI, should set the value explicitly.
  (doom-modeline-icon (display-graphic-p))

  ;; Whether display the indentation information.
  (doom-modeline-indent-info t)

  ;; The maximum displayed length of the branch name of version control.
  (setq doom-modeline-vcs-max-length 6)

  ;; Whether display the environment version.
  (setq doom-modeline-env-version t)

  ;; The limit of the window width.
  ;; If `window-width' is smaller than the limit, some information won't be displayed.
  (doom-modeline-window-width-limit fill-column)

  ;; If non-nil, a word count will be added to the selection-info modeline segment.
  (setq doom-modeline-enable-word-count t)

  ;; Whether display the modification icon for the buffer.
  ;; It respects `doom-modeline-icon' and `doom-modeline-buffer-state-icon'.
  (setq doom-modeline-buffer-modification-icon t)

  ;; Whether display the environment version.
  (doom-modeline-env-version t)
  (doom-modeline-major-mode-icon t)

  ;; Whether display the colorful icon for `major-mode'.
  ;; It respects `all-the-icons-color-icons'.
  (doom-modeline-major-mode-color-icon t)

  (doom-modeline-minor-modes t))

(doom-modeline-mode 1)

(use-package alert
  :commands alert
  :config
  (setq alert-default-style 'notifications))

(use-package perspective
  :demand t
  :bind (("C-M-k" . persp-switch)
         ("C-M-n" . persp-next)
         ("C-x k" . persp-kill-buffer*))
  :custom
  (persp-initial-frame-name "Main")
  :config
  ;; Running `persp-mode' multiple times resets the perspective list...
  (unless (equal persp-mode t)
    (persp-mode)))

(use-package paren
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))
(electric-pair-mode 1) ;; إغلاق تلقائي للأقواس

(setq display-time-world-list
      '(("Etc/UTC" "UTC")
        ("America/Los_Angeles" "Seattle")
        ("Africa/Cairo" "Cairo")
        ("Europe/Athens" "Athens")
        ("Pacific/Auckland" "Auckland")
        ("Asia/Shanghai" "Shanghai")
        ("Asia/Kolkata" "Hyderabad")))
(setq display-time-world-time-format "%a, %d %b %I:%M %p %Z")

(use-package pinentry
  :ensure t)

(setq epa-pinentry-mode 'loopback)
(pinentry-start)

(setq tramp-default-method "ssh")

(server-mode 1)

(use-package hydra)

(use-package general
  :config
  (general-override-mode)
  (general-create-definer leader-spc
    :keymaps 'override
    :prefix "SPC"))

;; ESC Cancels All
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(general-define-key
 :keymaps '(normal insert emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "g" 'counsel-projectile-rg
 "t t" 'load-theme)

(setq-default tab-width 2)

(setq-default indent-tabs-mode nil)

(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

(delete-selection-mode +1)

(recentf-mode 1)

;; Save what you enter into minibuffer prompts
(setq history-length 100)
(savehist-mode 1)

;; Remember and restore the last cursor location of opened files
(save-place-mode 1)

(use-package super-save
  :defer 1
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

(use-package multiple-cursors)
(global-set-key (kbd "C-M-x") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(use-package aggressive-indent)
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'css-mode-hook #'aggressive-indent-mode)

(global-aggressive-indent-mode 1)
(add-to-list 'aggressive-indent-excluded-modes 'html-mode)

(add-to-list
 'aggressive-indent-dont-indent-if
 '(and (derived-mode-p 'c++-mode)
       (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                           (thing-at-point 'line)))))

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode)
  :config
  ;; (setq undo-tree-auto-save-history 1) ;; you can turn this on
  ;; Each node in the undo tree should have a timestamp.
  (setq undo-tree-visualizer-timestamps t)
  ;; Show a diff window displaying changes between undo nodes.
  (setq undo-tree-visualizer-diff t))

(define-key global-map (kbd "C-/") 'undo)
(define-key global-map (kbd "C-x C-/") 'redo)

(use-package corfu
  :bind (:map corfu-map
              ("C-j" . corfu-next)
              ("C-k" . corfu-previous)
              ("C-f" . corfu-insert))
  :custom
  (corfu-cycle t)
  :config
  (corfu-global-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package avy
  :commands (avy-goto-char avy-goto-word-0 avy-goto-line))

(general-define-key
 :keymap '(normal emacs)
 :prefix "C-c"
 :properties '(:repeat t :jump t)
 :non-normal-prefix "M-SPC"
 "c" 'avy-goto-char
 "l" 'avy-goto-line
 "w" 'avy-goto-word-0)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :ensure t
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(setq ivy-initial-inputs-alist nil)

(use-package smex)
(smex-initialize)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;;If you want to replace the default Emacs help keybindings, you can do so:
;; Note that the built-in `describe-function' includes both functions
;; and macros. `helpful-function' is functions only, so we provide
;; `helpful-callable' as a drop-in replacement.
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)

(use-package bufler
  :bind (("C-M-j" . bufler-switch-buffer)
         ("C-M-k" . bufler-workspace-frame-set))
  :config
  (setf bufler-groups
        (bufler-defgroups
          ;; Subgroup collecting all named workspaces.
          (group (auto-workspace))
          ;; Subgroup collecting buffers in a projectile project.
          (group (auto-projectile))
          ;; Grouping browser windows
          (group
           (group-or "Browsers"
                     (name-match "Vimb" (rx bos "vimb"))
                     (name-match "Qutebrowser" (rx bos "Qutebrowser"))
                     (name-match "Chromium" (rx bos "Chromium"))))
          (group
           (group-or "Chat"
                     (mode-match "Telega" (rx bos "telega-"))))
          (group
           ;; Subgroup collecting all `help-mode' and `info-mode' buffers.
           (group-or "Help/Info"
                     (mode-match "*Help*" (rx bos (or "help-" "helpful-")))
                     ;; (mode-match "*Helpful*" (rx bos "helpful-"))
                     (mode-match "*Info*" (rx bos "info-"))))
          (group
           ;; Subgroup collecting all special buffers (i.e. ones that are not
           ;; file-backed), except `magit-status-mode' buffers (which are allowed to fall
           ;; through to other groups, so they end up grouped with their project buffers).
           (group-and "*Special*"
                      (name-match "**Special**"
                                  (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace" "Pinentry") "*"))
                      (lambda (buffer)
                        (unless (or (funcall (mode-match "Magit" (rx bos "magit-status"))
                                             buffer)
                                    (funcall (mode-match "Dired" (rx bos "dired"))
                                             buffer)
                                    (funcall (auto-file) buffer))
                          "*Special*"))))
          ;; Group remaining buffers by major mode.
          (auto-mode))))

(use-package default-text-scale
  :defer 1
  :config
  (default-text-scale-mode))

(use-package ace-window
  :bind (("M-o" . ace-window))
  :custom
  (aw-scope 'frame)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-minibuffer-flag t)
  :config
  (ace-window-display-mode 1))

(use-package winner
  :config
  (winner-mode))

;; (setq display-buffer-base-action
;;       '(display-buffer-reuse-mode-window
;;         display-buffer-reuse-window
;;         display-buffer-same-window))

;; If a popup does happen, don't resize windows to be equal-sized
(setq even-window-sizes nil)

(use-package password-store
  :config
  (setq password-store-password-length 12))

(use-package auth-source-pass
  :config
  (auth-source-pass-enable))

(use-package password-store-otp)

(use-package oauth2)

(setq auth-sources '("~/.authinfo.gpg"))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :demand t
  :custom ((projectile-completion-system 'ivy))
  :bind ("C-M-p" . projectile-find-file)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/.local/src")
    (setq projectile-project-search-path '("~/.local/src")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(global-set-key (kbd "C-x g") 'magit)

(use-package forge
  :ensure t)

(use-package magit-todos
  :defer t)

(use-package pdf-tools
  :config
  ;;(pdf-tools-install)
  (pdf-loader-install)
  (setq-default pdf-view-display-size 'fit-width)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (define-key pdf-view-mode-map (kbd "C-r") 'isearch-backward)
  (setq pdf-misc-print-programm "/usr/bin/lp")
  (setq-default pdf-view-display-size (quote fit-page))
  (setq pdf-view-incompatible-modes
        (quote
         (linum-relative-mode helm-linum-relative-mode nlinum-mode nlinum-hl-mode nlinum-relative-mode yalinum-mode)))
  :custom
  (pdf-annot-activate-created-annotations t "automatically annotate highlights"))

(setq  lpr-command "lp"
       lpr-printer-switch "-d"
       lpr-switches (quote ("-o fit-to-page=true" "-o sides=two-sided-long-edge" "-o Resolution=600" "-n 1 -o page-ranges=1-"))
       )

(setq thumbs-conversion-program "/usr/bin/convert")
(setq ps-print-header nil)

(add-hook 'image-mode-hook
          (lambda ()
            (auto-revert-mode)
            (auto-image-file-mode)))

(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
      TeX-source-correlate-start-server t)

(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)

(use-package org-noter)

(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-noter-pdftools
  :after org-noter)

;; (use-package org-pdfview)

(use-package pdf-view-restore
  :after pdf-toos
  :config
  (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode))
(setq pdf-view-restore-filename "~/.cache/emacs/.pdf-view-restore")

(use-package git-link
  :commands git-link
  :config
  (setq git-link-open-in-browser t))

;;Functions can be called interactively (M-x git-link) or via a key binding of your choice. For example:
(global-set-key (kbd "C-c g l") 'git-link)

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
(use-package docker-compose-mode)

(use-package lsp-docker)

(defvar lsp-docker-client-packages
  '(lsp-css lsp-clients lsp-bash lsp-go lsp-pyls lsp-html lsp-typescript
            lsp-terraform lsp-clangd))

(setq lsp-docker-client-configs
      '((:server-id bash-ls :docker-server-id bashls-docker :server-command "bash-language-server start")
        (:server-id clangd :docker-server-id clangd-docker :server-command "clangd")
        (:server-id css-ls :docker-server-id cssls-docker :server-command "css-languageserver --stdio")
        (:server-id dockerfile-ls :docker-server-id dockerfilels-docker :server-command "docker-langserver --stdio")
        (:server-id gopls :docker-server-id gopls-docker :server-command "gopls")
        (:server-id html-ls :docker-server-id htmls-docker :server-command "html-languageserver --stdio")
        (:server-id pyls :docker-server-id pyls-docker :server-command "pyls")
        (:server-id ts-ls :docker-server-id tsls-docker :server-command "typescript-language-server --stdio")))

(use-package lsp-mode
  :commands lsp
  :hook ((typescript-mode js2-mode web-mode) . lsp)
  :bind (:map lsp-mode-map
              ("TAB" . completion-at-point))
  :custom (lsp-headerline-breadcrumb-enable nil))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-treemacs)

(use-package dap-mode
  :custom
  (lsp-enable-dap-auto-configure nil)
  :config
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (require 'dap-node)
  (dap-node-setup))

(use-package lispy
  :hook ((emacs-lisp-mode . lispy-mode)
         (scheme-mode . lispy-mode)))

(use-package lispyville
  :hook ((lispy-mode . lispyville-mode))
  :config
  (lispyville-set-key-theme '(operators c-w additional
                                        additional-movement slurp/barf-cp
                                        prettify)))

(use-package ggtags
  :ensure t
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (ggtags-mode 1)))))

(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))

(use-package disaster)
(define-key c-mode-base-map (kbd "C-c a") 'disaster)

(use-package go-mode
  :hook (go-mode . lsp-deferred))

(use-package lsp-java)
(add-hook 'java-mode-hook #'lsp)

(use-package autodisass-java-bytecode)

(use-package rust-mode
  :mode "\\.rs\\'"
  :init (setq rust-format-on-save t))

(use-package cargo
  :defer t)

(use-package elpy
  :ensure t
  :init
  (elpy-enable))

(add-hook 'emacs-lisp-mode-hook #'flycheck-mode)

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (setq markdown-command "marked")
  (defun amf/set-markdown-header-font-sizes ()
    (dolist (face '((markdown-header-face-1 . 1.8)
                    (markdown-header-face-2 . 1.6)
                    (markdown-header-face-3 . 1.4)
                    (markdown-header-face-4 . 1.2)
                    (markdown-header-face-5 . 1.0)))
      (set-face-attribute (car face) nil :weight 'normal :height (cdr face)))))

(use-package web-mode
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2)
  (setq-default web-mode-enable-current-element-highlight t)
  (setq-default web-mode-enable-current-column-highlight t))

(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))

(setq web-mode-content-types-alist
      '(("jsx" . "\\.jsx?\\'")
        ("tsx" . "\\.tsx?\\'")
        ("json" . "\\.json\\'")))

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      create-lockfiles nil) ;; lock files will kill `npm start'

;; 1. Start the server with `httpd-start'
;; 2. Use `impatient-mode' on any buffer
(use-package impatient-mode)

(use-package skewer-mode)

(use-package emmet-mode
  :init
  (emmet-mode t))

(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

(add-hook 'emmet-mode-hook (lambda () (setq emmet-indent-after-insert nil)))

;; If you disable indent-region, you can set the default indent level thusly:
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ;; indent 2 spaces.

(setq emmet-move-cursor-between-quotes t) ;; default nil

;; To enable the JSX support, add your major-mode to emmet-jsx-major-modes:
(add-to-list 'emmet-jsx-major-modes 'your-jsx-major-mode)

;; Enable emmet-mode with web-mode
(add-hook 'web-mode-hook  'emmet-mode)

(general-define-key
 :keymaps '(normal emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 "w" 'emmet-wrap-wait-markup)

(use-package auctex-latexmk
  :config
  (auctex-latexmk-setup)
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))

;; Customary Customization, p. 1 and 16 in the manual, and http://www.emacswiki.org/emacs/AUCTeX#toc2
(setq TeX-parse-self t); Enable parse on load.
(setq TeX-auto-save t); Enable parse on save.
(setq-default TeX-master nil)

(setq TeX-PDF-mode t); PDF mode (rather than DVI-mode)

(add-hook 'TeX-mode-hook 'flyspell-mode); Enable Flyspell mode for TeX modes such as AUCTeX. Highlights all misspelled words.
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode); Enable Flyspell program mode for emacs lisp mode, which highlights all misspelled words in comments and strings.
(setq ispell-dictionary "english"); Default dictionary. To change do M-x ispell-change-dictionary RET.
(add-hook 'TeX-mode-hook
          (lambda () (TeX-fold-mode 1))); Automatically activate TeX-fold-mode.
(setq LaTeX-babel-hyphen nil); Disable language-specific hyphen insertion.

;; " expands into csquotes macros (for this to work babel must be loaded after csquotes).
(setq LaTeX-csquotes-close-quote "}"
      LaTeX-csquotes-open-quote "\\enquote{")

;; LaTeX-math-mode http://www.gnu.org/s/auctex/manual/auctex/Mathematics.html
(add-hook 'TeX-mode-hook 'LaTeX-math-mode)

(use-package reftex
  :defer t
  :config
  (setq reftex-cite-prompt-optional-args t)) ;; Prompt for empty optional arguments in cite
;; Turn on RefTeX for AUCTeX http://www.gnu.org/s/auctex/manual/reftex/reftex_5.html
(add-hook 'TeX-mode-hook 'turn-on-reftex)

(eval-after-load 'reftex-vars; Is this construct really needed?
  '(progn
     (setq reftex-cite-prompt-optional-args t); Prompt for empty optional arguments in cite macros.
     ;; Make RefTeX interact with AUCTeX, http://www.gnu.org/s/auctex/manual/reftex/AUCTeX_002dRefTeX-Interface.html
     (setq reftex-plug-into-AUCTeX t)
     ;; So that RefTeX also recognizes \addbibresource. Note that you
     ;; can't use $HOME in path for \addbibresource but that "~"
     ;; works.
     (setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))
                                        ;     (setq reftex-default-bibliography '("UNCOMMENT LINE AND INSERT PATH TO YOUR BIBLIOGRAPHY HERE")); So that RefTeX in Org-mode knows bibliography
     (setcdr (assoc 'caption reftex-default-context-regexps) "\\\\\\(rot\\|sub\\)?caption\\*?[[{]"); Recognize \subcaptions, e.g. reftex-citation
     (setq reftex-cite-format; Get ReTeX with biblatex, see https://tex.stackexchange.com/questions/31966/setting-up-reftex-with-biblatex-citation-commands/31992#31992
           '((?t . "\\textcite[]{%l}")
             (?a . "\\autocite[]{%l}")
             (?c . "\\cite[]{%l}")
             (?s . "\\smartcite[]{%l}")
             (?f . "\\footcite[]{%l}")
             (?n . "\\nocite{%l}")
             (?b . "\\blockcquote[]{%l}{}")))))

;; Fontification (remove unnecessary entries as you notice them) http://lists.gnu.org/archive/html/emacs-orgmode/2009-05/msg00236.html http://www.gnu.org/software/auctex/manual/auctex/Fontification-of-macros.html
(setq font-latex-match-reference-keywords
      '(
        ;; biblatex
        ("printbibliography" "[{")
        ("addbibresource" "[{")
        ;; Standard commands
        ;; ("cite" "[{")
        ("Cite" "[{")
        ("parencite" "[{")
        ("Parencite" "[{")
        ("footcite" "[{")
        ("footcitetext" "[{")
        ;; ;; Style-specific commands
        ("textcite" "[{")
        ("Textcite" "[{")
        ("smartcite" "[{")
        ("Smartcite" "[{")
        ("cite*" "[{")
        ("parencite*" "[{")
        ("supercite" "[{")
                                        ; Qualified citation lists
        ("cites" "[{")
        ("Cites" "[{")
        ("parencites" "[{")
        ("Parencites" "[{")
        ("footcites" "[{")
        ("footcitetexts" "[{")
        ("smartcites" "[{")
        ("Smartcites" "[{")
        ("textcites" "[{")
        ("Textcites" "[{")
        ("supercites" "[{")
        ;; Style-independent commands
        ("autocite" "[{")
        ("Autocite" "[{")
        ("autocite*" "[{")
        ("Autocite*" "[{")
        ("autocites" "[{")
        ("Autocites" "[{")
        ;; Text commands
        ("citeauthor" "[{")
        ("Citeauthor" "[{")
        ("citetitle" "[{")
        ("citetitle*" "[{")
        ("citeyear" "[{")
        ("citedate" "[{")
        ("citeurl" "[{")
        ;; Special commands
        ("fullcite" "[{")))

(setq font-latex-match-textual-keywords
      '(
        ;; biblatex brackets
        ("parentext" "{")
        ("brackettext" "{")
        ("hybridblockquote" "[{")
        ;; Auxiliary Commands
        ("textelp" "{")
        ("textelp*" "{")
        ("textins" "{")
        ("textins*" "{")
        ;; supcaption
        ("subcaption" "[{")))

(setq font-latex-match-variable-keywords
      '(
        ;; amsmath
        ("numberwithin" "{")
        ;; enumitem
        ("setlist" "[{")
        ("setlist*" "[{")
        ("newlist" "{")
        ("renewlist" "{")
        ("setlistdepth" "{")
        ("restartlist" "{")))

(use-package auto-dictionary
  :init(add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode 1))))

(use-package company-math)
;; global activation of the unicode symbol completion
(add-to-list 'company-backends 'company-math-symbols-unicode)

(use-package company-auctex
  :init (company-auctex-init))

(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :config (progn
            (setq TeX-source-correlate-mode t)
            (setq TeX-source-correlate-method 'synctex)
            (setq TeX-auto-save t)
            (setq TeX-parse-self t)
                                        ; (setq-default TeX-master "paper.tex")
            (setq reftex-plug-into-AUCTeX t)
            (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
                  TeX-source-correlate-start-server t)
            ;; Update PDF buffers after successful LaTeX runs
            (add-hook 'TeX-after-compilation-finished-functions
                      #'TeX-revert-document-buffer)
            (add-hook 'LaTeX-mode-hook
                      (lambda ()
                        (reftex-mode t)
                        (flyspell-mode t)))
            ))

(use-package latex-preview-pane)

;; Refresh Preview (bound to M-p)
;; Open in External Program (Bound to M-P)
(latex-preview-pane-enable)

(use-package indium)

(use-package js2-mode)
(use-package js2-refactor)

(use-package prettier-js)

(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)

(use-package json-mode)

(use-package scss-mode
  :mode ("\\.scss\\'" . scss-mode)
  :config (setq css-indent-offset 2))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package solidity-mode)

(use-package color-identifiers-mode)
(add-hook 'after-init-hook 'global-color-identifiers-mode)

;; To make the variables stand out, you can turn off highlighting for all other keywords in supported modes using a code like:
(defun myfunc-color-identifiers-mode-hook ()
  (let ((faces '(font-lock-comment-face font-lock-comment-delimiter-face font-lock-constant-face font-lock-type-face font-lock-function-name-face font-lock-variable-name-face font-lock-keyword-face font-lock-string-face font-lock-builtin-face font-lock-preprocessor-face font-lock-warning-face font-lock-doc-face font-lock-negation-char-face font-lock-regexp-grouping-construct font-lock-regexp-grouping-backslash)))
    (dolist (face faces)
      (face-remap-add-relative face '((:foreground "" :weight normal :slant normal)))))
  (face-remap-add-relative 'font-lock-keyword-face '((:weight bold)))
  (face-remap-add-relative 'font-lock-comment-face '((:slant italic)))
  (face-remap-add-relative 'font-lock-builtin-face '((:weight bold)))
  (face-remap-add-relative 'font-lock-preprocessor-face '((:weight bold)))
  (face-remap-add-relative 'font-lock-function-name-face '((:slant italic)))
  (face-remap-add-relative 'font-lock-string-face '((:slant italic)))
  (face-remap-add-relative 'font-lock-constant-face '((:weight bold))))
(add-hook 'color-identifiers-mode-hook 'myfunc-color-identifiers-mode-hook)

(use-package symbol-overlay)
(global-set-key (kbd "M-i") 'symbol-overlay-put)
(global-set-key (kbd "M-n") 'symbol-overlay-switch-forward)
(global-set-key (kbd "M-p") 'symbol-overlay-switch-backward)
(global-set-key (kbd "<f7>") 'symbol-overlay-mode)
(global-set-key (kbd "<f8>") 'symbol-overlay-remove-all)

(use-package visual-regexp)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
;; if you use multiple-cursors, this is for you:
(define-key global-map (kbd "C-c m") 'vr/mc-mark)

(use-package format-all)
(add-hook 'prog-mode-hook 'format-all-mode)
(setq format-all-show-errors 'errors)

(use-package compile
  :custom
  (compilation-scroll-output t))

(defun auto-recompile-buffer ()
  (interactive)
  (if (member #'recompile after-save-hook)
      (remove-hook 'after-save-hook #'recompile t)
    (add-hook 'after-save-hook #'recompile nil t)))

(use-package flycheck
  :init (global-flycheck-mode)
  :hook (lsp-mode . flycheck-mode))

(add-hook 'after-init-hook #'global-flycheck-mode)

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package rainbow-mode
  :defer t
  :hook (org-mode
         emacs-lisp-mode
         web-mode
         typescript-mode
         js2-mode))

(use-package darkroom
  :commands darkroom-mode
  :config
  (setq darkroom-text-scale-increase 0))

(defun amf/enter-focus-mode ()
  (interactive)
  (darkroom-mode 1)
  (display-line-numbers-mode 0))

(defun amf/leave-focus-mode ()
  (interactive)
  (darkroom-mode 0)
  (display-line-numbers-mode 1))

(defun amf/toggle-focus-mode ()
  (interactive)
  (if (symbol-value darkroom-mode)
      (amf/leave-focus-mode)
    (amf/enter-focus-mode)))

(use-package flyspell-correct
  :ensure t
  :config
  ;; set ivy as correcting interface
  (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-wrapper))

(use-package flyspell-correct-ivy
  :ensure t)

(use-package flymake)
(setq ispell-program-name "aspell") ; could be ispell as well, depending on your preferences
(setq ispell-dictionary "american") ; this can obviously be set to any language your spell-checking program supports

(add-hook 'text-mode-hook #'flyspell-mode)

(use-package company
  :init
  (company-mode t)
  :config
  (setq company-idle-delay 0.0)
  (setq company-minimum-prefix-length 1))

(add-hook 'after-init-hook 'global-company-mode)

(use-package redacted)

;; Enable `read-only-mode' to ensure that we don't change what we can't read.
(add-hook 'redacted-mode-hook (lambda () (read-only-mode (if redacted-mode 1 -1))))

(use-package keycast)

(use-package dashboard
  :init      ;; tweak dashboard config before loading it
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "While any text editor can save your files, only Emacs can save your soul")
  (setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  ;; (setq dashboard-startup-banner "~/.config/emacs/images/RMS.png")  ;; use custom image as banner
  (setq dashboard-center-content nil) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 4)
                          (agenda . 3 )
                          (bookmarks . 3)
                          (projects . 3)
                          (registers . 3)))
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book"))))
(dashboard-return)

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(use-package page-break-lines)

(use-package all-the-icons-dired)
(use-package dired-open)
(use-package peep-dired)

(with-eval-after-load 'dired
  ;; Get file icons in dired
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  ;; With dired-open plugin, you can launch external programs for certain extensions
  ;; For example, I set all .png files to open in 'sxiv' and all .mp4 files to open in 'mpv'
  (setq dired-open-extensions '(("gif" . "sxiv")
                                ("jpg" . "sxiv")
                                ("png" . "sxiv")
                                ("mkv" . "mpv")
                                ("mp4" . "mpv"))))

(use-package calfw
  :commands cfw:open-org-calendar
  :config
  (setq cfw:fchar-junction ?╋
        cfw:fchar-vertical-line ?┃
        cfw:fchar-horizontal-line ?━
        cfw:fchar-left-junction ?┣
        cfw:fchar-right-junction ?┫
        cfw:fchar-top-junction ?┯
        cfw:fchar-top-left-corner ?┏
        cfw:fchar-top-right-corner ?┓)
  (use-package calfw-org
    :config
    (setq cfw:org-agenda-schedule-args '(:timestamp))))

(use-package vterm
  :load-path  "~/.config/emacs/site-lisp/emacs-libvterm"
  :commands vterm
  :config
  (setq vterm-max-scrollback 1000000))

(use-package tracking
  :defer t
  :config
  (setq tracking-faces-priorities '(all-the-icons-pink
                                    all-the-icons-lgreen
                                    all-the-icons-lblue))
  (setq tracking-frame-behavior nil))

(use-package mpv)

(use-package emms
  :commands emms
  :config
  (require 'emms-setup)
  (emms-standard)
  (emms-default-players)
  (emms-mode-line-disable)
  (setq emms-source-file-default-directory "/mnt/entertainment/music"))

(use-package mu4e
  :ensure nil
  :defer 20 ;; Wait until 10 seconds after startup
  :config

  ;; Refresh mail using isync every 10 minutes
  (setq mu4e-update-interval (* 2 60))
  (setq mu4e-get-mail-command "mbsync -a -c ~/.config/isync/mbsyncrc")
  (setq mu4e-maildir "~/.local/share/Mail")

  ;; Use Ivy for mu4e completions (maildir folders, etc)
  (setq mu4e-completing-read-function #'ivy-completing-read)

  ;; load mu4e-context configuration
  (setq mu4e-contexts
        (list
         ;; work
         (make-mu4e-context
          :name "work"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/work" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "abdeltwab.m.fakhry@gmail.com")
                  (user-full-name    . "Abd El-Twab M. Fakhry")

                  (smtpmail-smtp-server  . "smtp.gmail.com")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-stream-type  . ssl)

                  (mu4e-sent-folder       . "/work/[Gmail]/Sent Mail")
                  (mu4e-spam-folder       . "/work/[Gmail]/Spam")
                  (mu4e-trash-folder      . "/work/[Gmail]/Trash")
                  (mu4e-starred-folder    . "/work/[Gmail]/Starred")
                  (mu4e-scheduled-folder  . "/work/[Gmail]/Scheduled")
                  (mu4e-drafts-folder     . "/work/[Gmail]/Drafts")))

         ;; University account
         (make-mu4e-context
          :name "azhar"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/azhar" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "AbdEl-TwabFakhry.2020@azhar.edu.eg")
                  (user-full-name    . "Abd El-Twab M. Fakhry")

                  (smtpmail-smtp-server  . "smtp.office365.com")
                  (smtpmail-smtp-service . 587)
                  (smtpmail-stream-type  . ssl)

                  (mu4e-inbox-folder     . "/azhar/Inbox")
                  (mu4e-sent-folder      . "/azhar/Sent Items")
                  (mu4e-spam-folder      . "/azhar/Spambox")
                  (mu4e-trash-folder     . "/azhar/Trash")
                  (mu4e-drafts-folder    . "/azhar/Drafts")))))

  ;; Mail dir
  (setq mu4e-maildir-shortcuts
        '(("/work/Inbox" 				     . ?i)
          ("/work/[Gmail]/Sent Mail" . ?s)
          ("/work/[Gmail]/Spam"      . ?p)
          ("/work/[Gmail]/Trash"     . ?t)
          ("/work/[Gmail]/Starred"   . ?r)
          ("/work/[Gmail]/Scheduled" . ?c)

          ("/azhar/Inbox"        . ?u)
          ("/azhar/Sent Items"   . ?n)
          ("/azhar/Spambox"      . ?m)
          ("/azhar/Trash"        . ?h)))

  ;; You can create bookmarks to show merged views of folders across accounts:
  (add-to-list 'mu4e-bookmarks '("m:/azhar/Inbox or m:/work/Inbox" "All Inboxes" ?i))

  ;; Display options
  (setq mu4e-view-show-images t)
  (setq mu4e-view-show-addresses 't)

  (setq message-confirm-send t)

  ;; Picking a context for sending mail
  ;; When using multiple contexts, you might want to define which context gets picked automatically for sending email (similar to mu4e-context-policy):
  ;; Only ask if a context hasn't been previously picked
  (setq mu4e-compose-context-policy 'ask-if-none)

  ;; start with the first (default) context;
  ;; default is to ask-if-none (ask when there's no context yet, and none match)
  (setq mu4e-context-policy 'pick-first)

  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)

  ;; Configure the function to use for sending mail
  (setq message-send-mail-function 'smtpmail-send-it)

  ;; Improving the look of plain text emails
  ;; By default all e-mails are sent as plain text. This can lead to strange wrapping in other email clients when reading your messages. You can improve this by setting the following variable:
  ;; Make sure plain text mails flow correctly for recipients
  (setq mu4e-compose-format-flowed t)

  ;; Adding a signature to your emails
  ;; You can set the mu4e-compose-signature variable to a string for the signature to include in your e-mails!
  (setq mu4e-compose-signature
        "Abd El-Twab M. Fakhry\nSoftware Engineer | Competitive Programmer\nws: http://abdeltwabmf.me/\nm: 01127030951")

  ;; Automatically Sign Every Email
  ;; You can automatically sign every e-mail using the message-send-hook:
  (add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime)

  ;; Run mu4e in the background to sync mail periodically
  (mu4e t))

(use-package mu4e-marker-icons
  :init (mu4e-marker-icons-mode 1))

(use-package mu4e-alert
  :hook ((after-init . mu4e-alert-enable-notifications)
         (after-init . mu4e-alert-enable-mode-line-display)))

(mu4e-alert-set-default-style 'libnotify)
(setq mu4e-alert-email-notification-types '(count))

(defun amf/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1))

(defun amf/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

(with-eval-after-load 'org-faces
  ;; Increase the size of various headings
  (set-face-attribute 'org-document-title nil :font "Fantasque Sans Mono" :weight 'bold :height 1.5)
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.18)
                  (org-level-3 . 1.16)
                  (org-level-4 . 1.14)
                  (org-level-5 . 1.12)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :font "Fantasque Sans Mono" :weight 'regular :height (cdr face))))

(use-package org
  :hook (org-mode . amf/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (amf/org-font-setup))

(use-package org-bullets
  :after org
  :ensure t
  :config
  (add-hook 'org-mode-hook #'org-bullets-mode)
  (setq inhibit-compacting-font-caches t))

(setq org-clock-sound "~/.local/share/sounds/notification.wav")

(use-package org-present)

;; Install visual-fill-column
(unless (package-installed-p 'visual-fill-column)
  (package-install 'visual-fill-column))

;; Configure fill width
(setq visual-fill-column-width 130
      visual-fill-column-center-text t)

(defun amf/org-present-start ()
  ;; Tweak font sizes
  (setq-local face-remapping-alist '((default (:height 1.2) default)
                                     (header-line (:height 4.4) variable-pitch)
                                     (org-document-title (:height 4.5) variable-pitch)
                                     (org-code (:height 1.1)  default)
                                     (org-verbatim (:height 1.1) default)
                                     (org-block (:height 1.1) default)
                                     (org-block-begin-line (:height 0.7) default)))

  ;; Set a blank header line string to create blank space at the top
  (setq header-line-format " ")
  (display-line-numbers-mode 0)

  ;; Let the desktop background show through
  (set-frame-parameter (selected-frame) 'alpha '(90 . 100))
  (add-to-list 'default-frame-alist '(alpha . (90 . 90)))

  ;; Center the presentation and wrap lines
  (visual-fill-column-mode 1)
  (visual-line-mode 1))

(defun amf/org-present-end ()
  ;; Reset font customizations
  (setq-local face-remapping-alist '((default default default)))

  ;; Set a blank header line string to create blank space at the top
  (setq header-line-format nil)
  (display-line-numbers-mode 1)

  ;; Let the desktop background show through
  (set-frame-parameter (selected-frame) 'alpha '(100 . 100))
  (add-to-list 'default-frame-alist '(alpha . (100 . 100)))

  ;; Stop centering the document
  (visual-fill-column-mode 0)
  (visual-line-mode 0))

;; Register hooks with org-present
(add-hook 'org-present-mode-hook 'amf/org-present-start)
(add-hook 'org-present-mode-quit-hook 'amf/org-present-end)


;; Load org-faces to make sure we can set appropriate faces
(require 'org-faces)

;; Hide emphasis markers on formatted text
(setq org-hide-emphasis-markers t)

;; Resize Org headings
(dolist (face '((org-level-1 . 1.7)
                (org-level-2 . 1.5)
                (org-level-3 . 1.3)
                (org-level-4 . 1.1)
                (org-level-5 . 1.0)
                (org-level-6 . 1.0)
                (org-level-7 . 1.0)
                (org-level-8 . 1.0)))
  (set-face-attribute (car face) nil :font "Fantasque Sans Mono" :weight 'medium :height (cdr face)))

;; Make the document title a bit bigger
(set-face-attribute 'org-document-title nil :font "Fantasque Sans Mono" :weight 'bold :height 1.5)

;; Make sure certain org faces use the fixed-pitch face when variable-pitch-mode is on
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

(defun amf/org-present-prepare-slide (buffer-name heading)
  ;; Show only top-level headlines
  (org-overview)

  ;; Unfold the current entry
  (org-show-entry)

  ;; Show only direct subheadings of the slide but don't expand them
  (org-show-children))
(add-hook 'org-present-after-navigate-functions 'amf/org-present-prepare-slide)

(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(setq org-todo-keywords        ; This overwrites the default Doom org-todo-keywords
      '((sequence
         "TODO(t)"           ; A task that is considered
         "DOING(t)"          ; A task that is accomplish now
         "DONE(d)"           ; Task has been completed
         "IMPORTANT(I)"      ; A task with very high priority
         "READY(g)"          ; A task that is ready to be tackled
         "DRAFT(p)"          ; A task that is drafted
         "ARCHIVE(c)"        ; Archive for later on
         "WAIT(w)"           ; Something is holding up this task
         "|"                 ; The pipe necessary to separate "active" states and "inactive" states
         "CANCELLED(c)" )))  ; Task has been cancelled

(setq inhibit-compacting-font-caches t)

;; (server-mode t)
(provide 'init)
 ;;; init.el ends here
