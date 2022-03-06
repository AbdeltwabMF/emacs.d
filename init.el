;;; package --- Summary

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

;; Change the user-emacs-directory to keep unwanted things out of ~/.config/emacs
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering)

;; Keep customization settings in a temporary file
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

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

(setq inhibit-startup-message t)
(setq visible-bell t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq use-dialog-box nil) ;; Disable dialog boxes since they weren't working in Mac OSX

(set-frame-parameter (selected-frame) 'alpha '(95 . 90))
(add-to-list 'default-frame-alist '(alpha . (95 . 90)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

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
  (load-theme 'gruvbox-light-soft t))

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
                    :height 100
                    :weight 'light)

(set-fontset-font "fontset-default" 'arabic (font-spec
                                             :family "Amiri Quran"
                                             :height 110))

(defun amf/replace-unicode-font-mapping (block-name old-font new-font)
  (let* ((block-idx (cl-position-if
                     (lambda (i) (string-equal (car i) block-name))
                     unicode-fonts-block-font-mapping))
         (block-fonts (cadr (nth block-idx unicode-fonts-block-font-mapping)))
         (updated-block (cl-substitute new-font old-font block-fonts :test 'string-equal)))
    (setf (cdr (nth block-idx unicode-fonts-block-font-mapping))
          `(,updated-block))))

(use-package unicode-fonts
  :disabled
  :if (not amf/is-termux)
  :custom
  (unicode-fonts-skip-font-groups '(low-quality-glyphs))
  :config
  ;; Fix the font mappings to use the right emoji font
  (mapcar
   (lambda (block-name)
     (amf/replace-unicode-font-mapping block-name "Noto Color Emoji"))
   '("Dingbats"
     "Emoticons"
     "Miscellaneous Symbols and Pictographs"
     "Transport and Map Symbols"))
  (unicode-fonts-setup))

(use-package emojify
  :ensure t
  :hook
  (after-init . global-emojify-mode))

(use-package all-the-icons
  :if (display-graphic-p))

(setq display-time-format "%l:%M %p %b %y"
      display-time-default-load-average nil)

(use-package diminish
  :ensure t)

;; You must run (all-the-icons-install-fonts) one time after
;; installing this package!

(use-package minions
  :hook (doom-modeline-mode . minions-mode))

(use-package doom-modeline
  :ensure t
  :after eshell     ;; Make sure it gets hooked after eshell
  :hook (after-init . doom-modeline-init)
  :custom-face
  (mode-line ((t (:height 0.85))))
  (mode-line-inactive ((t (:height 0.85))))
  :custom
  (doom-modeline-height 15)
  (doom-modeline-bar-width 6)
  (doom-modeline-lsp t)
  (doom-modeline-github nil)

  ;; Whether display the mu4e notifications. It requires `mu4e-alert' package.
  (doom-modeline-mu4e nil)
  ;; also enable the start of mu4e-alert
  (mu4e-alert-enable-mode-line-display)

  (doom-modeline-irc t)
  (doom-modeline-minor-modes t)
  (doom-modeline-persp-name nil)
  (doom-modeline-buffer-file-name-style 'truncate-all)

  ;; Whether display icons in the mode-line.
  ;; While using the server mode in GUI, should set the value explicitly.
  (doom-modeline-icon (display-graphic-p))

  ;; Whether display the environment version.
  (doom-modeline-env-version t)

  (doom-modeline-major-mode-icon nil))

(doom-modeline-mode 1)

(use-package alert
  :commands alert
  :config
  (setq alert-default-style 'notifications))

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

(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)

(setq-default indent-tabs-mode nil)

(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

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

(global-set-key (kbd "C-:") 'avy-goto-char) ;; القفز إلى حرف ما
(global-set-key (kbd "C-'") 'avy-goto-char-2) ;; القفز إلى حرف باستخدام حرفين
(global-set-key (kbd "M-g f") 'avy-goto-line) ;; القفز إلى سطر
(global-set-key (kbd "M-g w") 'avy-goto-word-1) ;; القفز إلى كلمة باستخدام حرفها الأول
(global-set-key (kbd "M-g e") 'avy-goto-word-0) ;; القفز إلى أي كلمة من كلمات البفر

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

(use-package general
  :config
  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (rune/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(rune/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
  :ensure t)

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
  (set-face-attribute 'org-document-title nil :font "Cantarell" :weight 'bold :height 2.0)
  (dolist (face '((org-level-1 . 2.0)
                  (org-level-2 . 1.8)
                  (org-level-3 . 1.6)
                  (org-level-4 . 1.4)
                  (org-level-5 . 1.2)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face))))

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

(defun amf/org-mode-visual-fill ()
  (setq visual-fill-column-width 0
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . amf/org-mode-visual-fill))

(setq inhibit-compacting-font-caches t)

(provide 'init)
;;; init.el ends here
