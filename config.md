
# Table of Contents

1.  [About](#org5868951)
2.  [Package Management System](#org9a5ee0c)
3.  [Keybindings](#org658bae9)
4.  [Custom Functions](#org02c64fd)
5.  [Visuals](#org424003e)
6.  [Assist Tools](#orgb5505d6)
7.  [Git](#org8c403dd)
8.  [Markdown](#org7410e63)
9.  [Org Mode](#org746393d)
    1.  [Test out](#org3d3b3f6)
10. [Latex](#orgbdb1bd3)
11. [Programming Mode](#org30b54e8)
    1.  [LSP Mode](#org05c337b)
    2.  [LSP Servers](#org7d98b3e)
12. [Modernize Text Editing](#org6aa91a2)



<a id="org5868951"></a>

# About

My attempt at an organized config


<a id="org9a5ee0c"></a>

# Package Management System

Set package sources

    ;; Initialize package sources
    (require 'package)
    
    (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                             ("org" . "https://orgmode.org/elpa/")
                             ("elpa" . "https://elpa.gnu.org/packages/")))
    
    (package-initialize)
    (unless package-archive-contents
      (package-refresh-contents))
    
    ;; Initialize use-package on non-Linux platforms
    (unless (package-installed-p 'use-package)
      (package-install 'use-package))
    
    (require 'use-package)
    (setq use-package-always-ensure t)

Update packages every 60 days

    (use-package auto-package-update
      :custom
      (auto-package-update-interval 60)
      (auto-package-update-prompt-before-update t)
      (auto-package-update-hide-results t)
      :config
      (auto-package-update-maybe)
      (auto-package-update-at-time "09:00"))


<a id="org658bae9"></a>

# Keybindings

Apple-like

    (setq mac-command-modifier 'meta) ;; Cmd as M
    (global-set-key (kbd "M-c") 'kill-ring-save) ;; copy
    (global-set-key (kbd "M-v") 'yank) ;; paste
    (global-set-key (kbd "M-z") 'undo) ;; undo
    (global-set-key (kbd "M-a") 'mark-whole-buffer) ;; select all

Unbind some default keys

    (global-unset-key (kbd "C-_")) ;; remove C-S-_ to undo
    (global-unset-key (kbd "C-y")) ;; remove  C-y to yank
    (global-unset-key (kbd "C-x h")) ;; remove C-x h to select all
    (global-unset-key (kbd "C-x C-s")) ;; remove save, its automatic

Mouse support

    (global-unset-key [S-mouse-1])
    (setq shift-select-mode t)
    
    ;; Make Shift + mouse-1 set the point (start selection)
    (global-set-key [S-mouse-1] 'mouse-set-point)
    
    ;; Make Shift + drag extend the selection
    (global-set-key [S-down-mouse-1] 'mouse-save-then-kill)
    (global-set-key [S-drag-mouse-1] 'mouse-drag-region)

Cmd + Delete (without copying)

    (defun my/backward-delete-word-no-clipboard ()
      "Delete previous word without affecting the clipboard."
      (interactive)
      (let ((p (point)))
        (backward-word 1)
        (delete-region (point) p)))
    
    (global-set-key (kbd "M-<backspace>") #'my/backward-delete-word-no-clipboard)


<a id="org02c64fd"></a>

# Custom Functions

Quick file finding

    ;; config
    (defun config ()
      (interactive)
      ;; (delete-other-windows)
      (find-file "~/.emacs.d/config.org")
    )
    
    ;; cs
    (defun cs ()
      (interactive)
      ;; (delete-other-windows)
      (dired "~/gonz/cs")
    )


<a id="org424003e"></a>

# Visuals

Startup screen (reading-list.org)

    (setq inhibit-startup-screen t)
    (setq initial-scratch-message nil)
    
    ;; startup scratch buffer as org mode
    (setq initial-major-mode 'org-mode)

No clutter

    (menu-bar-mode -1) ;; no menubar
    (tool-bar-mode -1) ;; no toolbars
    (scroll-bar-mode -1) ;; no scroll bars
    
    ;; transparent title bar
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    (add-to-list 'default-frame-alist '(ns-appearance . dark))
    (setq ns-use-proxy-icon nil)
    (setq frame-title-format nil)
    
    (add-hook 'prog-mode-hook #'display-line-numbers-mode) ;; display line nums in progr mode
    (global-visual-line-mode t) ;; lines wrap around
    
    ;; hide macOS title bar
    (add-to-list 'default-frame-alist '(undecorated . t))
    ;; add very thin borders to be able to drag
    (add-to-list 'default-frame-alist '(internal-border-width . 1))
    (add-to-list 'default-frame-alist '(drag-internal-border . t))

Themes ([custom theme generator](https://emacsfodder.github.io/emacs-theme-editor/))

    ;; imported doom themes
    (use-package doom-themes
      :config
      (setq doom-themes-enable-bold t) ;; if nil, bold is universally disabled
      (setq doom-themes-enable-italic t) ;; if nil, italics is universally disabled  
      (doom-themes-visual-bell-config);; enable flashing mode-line on errors
      (doom-themes-org-config)) ;; corrects (and improves) org-mode's native fontification
    
    ;; custom themes
    (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
    
    ;; current theme
    ;; (load-theme 'doom-spacegrey t)
    ;; (load-theme 'doom-acario-light t)
    (load-theme 'doom-moonlight t)
    
    ;; cursor width
    (setq-default cursor-type '(bar . 2))

Font

    (set-face-attribute 'default nil :font "JetBrains Mono")
    (set-face-attribute 'default nil
      :height 160
      :weight 'medium)

Modeline

    ;; aesthetic
    (use-package mood-line
      :ensure t
      :config
      (mood-line-mode))
    
    ;; remove/abbreviate minor-mode names in modeline
    (use-package diminish)

Window size

    ;; full screen
    (add-to-list 'default-frame-alist '(fullscreen . maximized))
    ;; (setq initial-frame-alist
    ;;       '((width . 122)
    ;;         (height . 36)
    ;;         (top . 50)
    ;;         (left . 110)))
    
    (setq frame-resize-pixelwise t) ;; pixel precision in window movement

Tabs

    (tab-bar-mode 1)
    
    ;; tab bindings
    (global-set-key (kbd "C-S-<tab>") 'tab-bar-switch-to-prev-tab)
    (global-set-key (kbd "C-<tab>") 'tab-bar-switch-to-next-tab)
    (global-set-key (kbd "M-t") 'tab-bar-new-tab)
    (global-set-key (kbd "M-w") 'tab-bar-close-tab)
    
    ;; aesthetics
    (setq tab-bar-show 1) ;; hide bar if less than 1 tabs open
    (setq tab-bar-new-tab-to 'right) ;; put new tab immediate right of current tab
    (setq tab-bar-close-button-show nil) ;; remove button that closes tab
    (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator))

All the icons (provides icons)

    (use-package all-the-icons
      :if (display-graphic-p))
    
    (use-package all-the-icons-dired
      :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

Olivetti

    (use-package olivetti
      :ensure t
      :config
      (setq-default olivetti-body-width 100))

Scrolling

    ;; Emacs-mac
    (mac-mouse-wheel-mode 1)
    (setq mac-mouse-wheel-smooth-scroll t)
    
    ;; Non emacs-mac
    ;; (pixel-scroll-precision-mode 1)
    ;; (setq pixel-scroll-precision-use-momentum t)     ;; enable inertial/momentum scrolling
    ;; (setq pixel-scroll-precision-interpolation-factor 0.0001) ;; speed of scroll animation
    ;; (setq pixel-scroll-precision-large-scroll-height 2)  ;; default is 100

Transparency

    ;; 0..100 (100 = opaque, 0 = fully transparent)
    ;; (add-to-list 'default-frame-alist '(alpha . (90 . 90)))
    ;; (add-to-list 'default-frame-alist '(alpha . (95 . 95)))


<a id="orgb5505d6"></a>

# Assist Tools

Which-key (suggests commands)
p
    (use-package which-key
      :init
        (which-key-mode 1)
      :diminish
      :config
      (setq which-key-side-window-location 'bottom
            which-key-sort-order #'which-key-key-order-alpha
            which-key-sort-uppercase-first nil
            which-key-add-column-padding 1
            which-key-max-display-columns nil
            which-key-min-display-lines 6
            which-key-side-window-slot -10
            which-key-side-window-max-height 0.25
            which-key-idle-delay 0.8
            which-key-max-description-length 25
            which-key-allow-imprecise-window-fit t
            which-key-separator " → ")
      )

Company (in-buffer text autocomplete suggestions)

    (use-package company
      :defer 2
      :diminish
      :custom
      (company-begin-commands '(self-insert-command))
      (company-idle-delay .1)
      (company-minimum-prefix-length 2)
      (company-show-numbers t)
      (company-tooltip-align-annotations 't)
      (global-company-mode t)
    )
    
    (use-package company-box
      :after company
      :diminish
      :hook (company-mode . company-box-mode)
    )
    
    ;; disable company in shell
    (add-hook 'shell-mode-hook (lambda () (company-mode -1)) 'append)

Ivy (emacs completion, like `M-x` command suggestions)

    (use-package ivy
      :custom
      (setq ivy-use-virtual-buffers t)
      (setq ivy-count-format "(%d/%d) ")
      (setq enable-recursive-minibuffers t)
      :config
      (ivy-mode))

Counsel (extension on Ivy)

    (use-package counsel
      :after ivy
      :config (counsel-mode))

Ivy rich (enhancement layer for Ivy)

    (use-package ivy-rich
      :after ivy
      :init (ivy-rich-mode 1) ;; this gets us descriptions in M-x
      :custom
      (ivy-virtual-abbreviate 'full
       ivy-rich-switch-buffer-align-virtual-buffer t
       ivy-rich-path-style 'abbrev))
    
    (use-package all-the-icons-ivy-rich
      :init (all-the-icons-ivy-rich-mode 1))

Flycheck

    (use-package flycheck
      :defer t
      :diminish ;;explanation of what diminish does, search for "DIMINISH"
      :init (global-flycheck-mode))

Treemacs

    (use-package treemacs)


<a id="org8c403dd"></a>

# Git

Magit

    (use-package magit
      :defer t
      :bind (:map magit-mode-map
                  ("C-M-f" . magit-section-forward)
                  ("C-M-b" . magit-section-backward))
      :bind (:map custom-bindings-map
                  ("M-g b" . magit-blame-addition))
      :hook
      ((magit-pre-refresh  . diff-hl-magit-pre-refresh)
       (magit-post-refresh . diff-hl-magit-post-refresh))
      :config
      (setq magit-mode-quit-window 'magit-restore-window-configuration
        magit-auto-revert-mode t)
      ; Remove tags from status buffer headings to speed up refresh slightly
      (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header))

Git Timemachine

    (use-package git-timemachine
      :defer t)


<a id="org7410e63"></a>

# Markdown

Markdown mode

    (use-package markdown-mode
      :ensure t
      :mode ("\\.md\\'" . markdown-mode))

Grip mode (renders markdown in github-like manner)

    (use-package grip-mode
      :ensure t
      ;; :commands (grip-mode grip-show-preview)
      ;; :hook (markdown-mode . grip-mode)
      ;; :hook (org-mode . grip-mode)
      :config
      ;; split vertically
      (setq split-width-threshold 0)
      (setq split-height-threshold nil)
    )
    
    ;; !! Github username and access key for grip-mode set in secrets.el


<a id="org746393d"></a>

# Org Mode


<a id="org3d3b3f6"></a>

## Test out

    (use-package org
      :defer t
      :hook ((org-mode . olivetti-mode)
       ;; (org-mode . variable-pitc-mhode)
       (org-mode . visual-line-mode) ;; line wrap
       (org-mode . (lambda () (setq-local mode-line-format nil))) ;; no modeline
       )
      :config
      ;; Inline formatting
      (setq org-hide-emphasis-markers t)
      (setq org-pretty-entities t)
      (setq org-startup-with-inline-images t)
    
      ;; Clean up source blocks
      (setq org-edit-src-content-indentation 0)
    
      ;; Visually indent content under headings
      (setq org-startup-indented t)
    
    
      ;; Load code block execution support
      (org-babel-do-load-languages
       'org-babel-load-languages
       '((python . t)))
    
      ;; Latex
      (setq org-startup-with-latex-preview t)
      (plist-put org-format-latex-options :scale 1.35)
    
      ;; Make org subscripts defined with: _{}
      (setq org-use-sub-superscripts '{})
    
     )
    
    ;; Org modern
     (use-package org-modern
      :ensure t
      :hook (org-mode . org-modern-mode)
      :config
      ;; Make sure folding indicators like ▶ or ▼ are OFF
      (setq org-modern-fold-stars nil)       ; disables > looking arrows
      (setq org-modern-hide-stars nil)       ; shows original stars
      (setq org-modern-star '("◉" "○" "✸" "✿" "➤" "•"))
      (setq org-hide-leading-stars t))
    
    
    ;; modern editor (gdocs, pagse) behavior 
    (use-package org-autolist
      :ensure t
      :hook (org-mode . org-autolist-mode))
    
    ;; Toc-org: table of contents
    (use-package toc-org
      :hook (org-mode . toc-org-enable))


<a id="orgbdb1bd3"></a>

# Latex

Auctex

    (use-package auctex
      :hook
      (LaTeX-mode . turn-on-prettify-symbols-mode)
      (LaTeX-mode . reftex-mode)
      (LaTeX-mode . outline-minor-mode)
      (LaTeX-mode . olivetti-mode))

Reftex

    (setq reftex-toc-split-windows-horizontally t
        reftex-toc-split-windows-fraction     0.2)


<a id="org30b54e8"></a>

# Programming Mode

Editing

    ;; highlight all occurrences of word selected / cursor at
    (use-package symbol-overlay
      :ensure t
      :hook (prog-mode . symbol-overlay-mode))
    ;; automatic closing parenthesis
    (electric-pair-mode 1)

Visuals

    ;; line numbers
    (add-hook 'prog-mode-hook #'display-line-numbers-mode)
    ;; olivetti
    (add-hook 'prog-mode-hook #'olivetti-mode)
    ;; diff-hl (show git changess on buffer left side)
    (use-package diff-hl
      :hook (prog-mode . diff-hl-mode))

Language Support

    (add-to-list 'load-path "~/.emacs.d/language-support/") ;; ocaml, serpent-mode, why3
    (add-to-list 'auto-mode-alist '("\\.c0\\'" . c-mode)) ;; c0
    
    ;; why3 (not needed)
    ;; (require 'why3)
    ;; serpent (too long, not worth activating)
    ;; (add-to-list 'load-path "~/.emacs.d/language-support/serpent-mode.el")

Indenting

    (setq-default indent-tabs-mode nil)  ;; always use spaces
    (setq-default tab-width 2)           ;; how wide to *show* any existing tabs
    (add-hook 'c-mode-common-hook
              (lambda ()
                (setq indent-tabs-mode nil  ; enforce spaces in C/C++
                      c-basic-offset 2)))


<a id="org05c337b"></a>

## LSP Mode

    (use-package lsp-mode
      :ensure t
      :commands (lsp lsp-deferred)
      :hook ((python-mode . lsp-deferred)
             (c-mode . lsp-deferred)
       ;; (c++-mode . lsp-deferred)
             (go-mode . lsp-deferred))
      :config
      (global-set-key [C-mouse-1] #'lsp-ui-peek-find-definitions)
      (setq lsp-headerline-breadcrumb-enable 1)) ; we'll use a different method for function tracking
    
    (use-package lsp-ui
      :ensure t
      :after lsp-mode
      :commands lsp-ui-mode
      :hook (lsp-mode . lsp-ui-mode)
      :config
      (lsp-enable-which-key-integration t)
      (setq lsp-ui-doc-enable t
            lsp-ui-doc-include-signature t
            lsp-ui-sideline-enable nil ;; disable sideline
            lsp-ui-doc-position 'at-point   ;; where to show the popup
            lsp-ui-doc-show-with-cursor nil ;; don't show popup if (text) cursor is on word
            lsp-ui-doc-show-with-mouse t)) ;; shows variable/function info when mouse hovers over


<a id="org7d98b3e"></a>

## LSP Servers

Pyright

    ;; Installed with npm install -g pyright
    (use-package lsp-pyright
      :ensure t
      :hook (python-mode . (lambda () (require 'lsp-pyright) (lsp))))

Gopls (go server, installed via homebrew)

    (use-package exec-path-from-shell
      :ensure t
      :config
      (exec-path-from-shell-initialize))


<a id="org6aa91a2"></a>

# Modernize Text Editing

    (delete-selection-mode 1)    ;; typing on selected text deletes it
    (save-place-mode 1)          ;; remember cursor location in files after closing
    (global-auto-revert-mode t)  ;; automatically show changes if the file has changed
    
    ;; undo and redo
    (use-package undo-fu
      :config
      (global-unset-key (kbd "C-z"))
      (global-set-key (kbd "M-z")   'undo-fu-only-undo)
      (global-set-key (kbd "M-Z") 'undo-fu-only-redo))
    
    ;; apply copy/cut/delete to entire line
    (use-package whole-line-or-region
      :ensure t
      :config
      (whole-line-or-region-global-mode))

Autosave

    (use-package super-save
      :ensure t
      :config
      (setq super-save-auto-save-when-idle t
            super-save-idle-duration 1) ;; save after 1s idle
      (super-save-mode +1))

No backup files

    (setq make-backup-files nil)  ;; Disable backup files like file~
    (setq auto-save-default nil)  ;; Disable auto-save files like #file#

