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

(use-package auto-package-update
  :custom
  (auto-package-update-interval 60)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

(add-to-list 'load-path "~/.emacs.d/scripts/")

(use-package auctex)
(setq pdf-view-use-scaling t) ;; helps avois blurry PDFs on Mac retina display

(use-package tex
   :ensure auctex
   :init
   ;; Set default TeX engine to xetex
   (setq TeX-engine 'xetex)
   ;; or for LuaLaTeX, uncomment the following line and comment out the XeTeX line
   ;; (setq TeX-engine 'luatex)
   (setq TeX-PDF-mode t)
   :config
   ;; Use LaTeX-mode for .tex files
   (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
)

(use-package ivy
  :custom
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode))

(use-package counsel
  :after ivy
  :config (counsel-mode))

(use-package all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :after ivy
  :init (ivy-rich-mode 1) ;; this gets us descriptions in M-x
  :custom
  (ivy-virtual-abbreviate 'full
   ivy-rich-switch-buffer-align-virtual-buffer t
   ivy-rich-path-style 'abbrev))

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

(add-hook 'shell-mode-hook (lambda () (company-mode -1)) 'append)

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

(setq initial-scratch-message nil)
(setq inhibit-startup-screen t)

;; (use-package dashboard
;;   :init
;;   (setq dashboard-display-icons-p t)     ; display icons on both GUI and terminal
;;   (setq dashboard-icon-type 'all-the-icons)
  
;;   (setq dashboard-banner-logo-title "Emacs Is More Than A Text Editor!")
;;   (setq dashboard-center-content nil) ;; set to 't' for centered content
;;   (setq dashboard-items '((recents . 10)
;;                           (bookmarks . 10)
;; 			  ))
;;   :custom
;;   (dashboard-modify-heading-icons '((recents . "file-text")
;;                                     (bookmarks . "book")))
;;   :config
;;   (dashboard-setup-startup-hook)
;;   (setq dashboard-center-content t)
;;   (setq dashboard-item-shortcuts '((recents   . "r")
;;                                    (bookmarks . "b")))
;; )

(menu-bar-mode -1) ;; no menubar
(tool-bar-mode -1) ;; no toolbars
(scroll-bar-mode -1) ;; no scroll bars
;; (add-to-list 'default-frame-alist '(undecorated . t)) ;; remove apple window bar


(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon nil)
(setq frame-title-format nil)

(add-hook 'prog-mode-hook #'display-line-numbers-mode) ;; display line nums in progr mode
(global-visual-line-mode t) ;; lines wrap around

;; full screen on startup
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; specific height and width on startup
;; (add-to-list 'default-frame-alist '(height . 50))
;; (add-to-list 'default-frame-alist '(width . 100))

(setq-default cursor-type '(bar . 2)) ;; cursor width

(set-face-attribute 'default nil :font "JetBrains Mono")
(set-face-attribute 'default nil
	:height 160
	:weight 'medium)

(tab-bar-mode 1)

;; keybindings
(global-set-key (kbd "C-S-<tab>") 'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "C-<tab>") 'tab-bar-switch-to-next-tab)
(global-set-key (kbd "M-t") 'tab-bar-new-tab)
(global-set-key (kbd "M-w") 'tab-bar-close-tab)

;; aesthetics
(setq tab-bar-show 1) ;; hide bar if less than 1 tabs open
(setq tab-bar-new-tab-to 'right) ;; put new tab immediate right of current tab
(setq tab-bar-close-button-show nil) ;; remove button that closes tab
(setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator))

(use-package moody
  :config
  (moody-replace-mode-line-front-space)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package treemacs)

(use-package diminish)

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(use-package git-timemachine
  :after git-timemachine
  :hook (evil-normalize-keymaps . git-timemachine-hook)
  :config
    (evil-define-key 'normal git-timemachine-mode-map (kbd "C-j") 'git-timemachine-show-previous-revision)
    (evil-define-key 'normal git-timemachine-mode-map (kbd "C-k") 'git-timemachine-show-next-revision)
)

(use-package magit)

(use-package diff-hl
  :ensure t
  :hook ((prog-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (diff-hl-flydiff-mode) ;; real-time updating
)

(use-package grip-mode
  :ensure t
  ;; :commands (grip-mode grip-show-preview)
  :hook (markdown-mode . grip-mode)
  :config
  ;; split vertically
  (setq split-width-threshold 0)
  (setq split-height-threshold nil)


(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode))

; Hide modeline mode: (source: https://protesilaos.com/codelog/2020-07-16-emacs-focused-editing/)
(define-minor-mode hidden-mode-line-mode
  "Toggle modeline visibility in the current buffer."
  :init-value nil
  :global nil
  (if hidden-mode-line-mode
      (setq-local mode-line-format nil)
    (kill-local-variable 'mode-line-format)
    (force-mode-line-update)
  )
)

;; DISABLED
;; Writeroom: Focused writing 
;; (use-package writeroom-mode
;;   :ensure t
;;   :config
;;   (setq writeroom-mode-line nil)  ;; display mode-line while in writeroom-mode
;;   (setq writeroom-width 100)     ;; set the width of the writing area
;;   (setq writeroom-global-effects (delq 'fullscreen writeroom-global-effects))
;; )

;; Focused writing (replaced writeroom)
(use-package olivetti
  :ensure t
  :config
  (setq-default olivetti-body-width 100)  ;; Set the body width to 100 characters
)


;; Org bullets: rounded title circles
(use-package org-bullets
  :config
  (setq org-bullets-bullet-list '("\u200b")) ;; make bullets width 0 (i.e. hide them)
)

;; Toc-org: table of contents
(use-package toc-org)

(defun my-org-mode-setup ()
  (hidden-mode-line-mode 1)
  (variable-pitch-mode 1)
  (olivetti-mode 1)
  (org-bullets-mode 1)
  (toc-org-enable)
  (setq org-hide-emphasis-markers t)
  (setq org-edit-src-content-indentation 0) ;; don't indent code blocks
  (setq org-startup-with-inline-images t)
  (setq org-pretty-entities t) ;; can put latex inline in org
)

;; fullscreen alias to exit org mode fullscreen
(defalias 'fullscreen 'toggle-frame-fullscreen)

(add-hook 'org-mode-hook 'my-org-mode-setup)

(font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

(defun config ()
  (interactive)
  ;; (delete-other-windows)
  (find-file "~/.emacs.d/config.org")
)

(add-hook 'after-init-hook ;; after init, because have to wait for theme to fully load
          (lambda ()
            (let* ((variable-tuple
                    (cond ((x-list-fonts "ETBembo") '(:font "ETBembo"))
                          ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                          ((x-list-fonts "Lucida Grande") '(:font "Lucida Grande"))
                          ((x-list-fonts "Verdana") '(:font "Verdana"))
                          ((x-family-fonts "Sans Serif") '(:family "Sans Serif"))
                          (nil (warn "Cannot find a Sans Serif Font. Install Source Sans Pro."))))
                   (base-font-color (face-foreground 'default nil 'default))
                   (headline `(:inherit default :weight bold :foreground ,base-font-color)))
              (custom-theme-set-faces
               'user
	       ;; org headers
               `(org-level-8 ((t (,@headline ,@variable-tuple))))
               `(org-level-7 ((t (,@headline ,@variable-tuple))))
               `(org-level-6 ((t (,@headline ,@variable-tuple))))
               `(org-level-5 ((t (,@headline ,@variable-tuple))))
               `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
               `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.1))))
               `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.1))))
               `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.25))))
               `(org-document-title ((t (,@headline ,@variable-tuple :height 1.75 :underline nil))))

	       ;; Defining variable and mono-spaced
	       '(variable-pitch ((t (:family "ETBembo" :height 220 :weight thin))))
	       '(fixed-pitch ((t ( :family "JetBrains Mono" :height 158 :weight medium))))

	       ;; Face settings
	       '(org-block ((t (:inherit fixed-pitch)))) ; source code blocks set to fixed-pitch (monospaced)
	       '(org-code ((t (:inherit (shadow fixed-pitch)))))
	       '(org-document-info ((t (:foreground "dark orange"))))
	       '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
	       '(org-link ((t (:foreground "royal blue" :underline t))))
	       '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
	       '(org-property-value ((t (:inherit fixed-pitch))) t)
	       '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
	       (set-face-attribute 'font-lock-comment-face nil :slant 'italic) ;; set commented regions to itallic
	       '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
	       '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
	       '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))))))

;; (use-package org-roam
;;   :init
;;   (setq org-roam-v2-ack t)
;;   :custom
;;   (org-roam-directory "~/gonz/Obsidian")
;;   (org-roam-completion-everywhere t)
;;   :bind (("C-c r t" . org-roam-buffer-toggle)
;;          ("C-c r f" . org-roam-node-find)
;;          ("C-c r i" . org-roam-node-insert)
;; 	     ("C-c r g" . org-roam-ui-open)
;; 	     ("C-c r G" . org-roam-graph)
;; 	    )
;;   :config
;;   (org-roam-db-autosync-mode)
;;   (org-roam-setup))

;; ;; Required dependencies for ui graph package
;; (use-package websocket
;;   :after org-roam)

;; (use-package org-roam-ui
;;     :after org-roam
;;     :config
;;     (setq org-roam-ui-sync-theme t
;;           org-roam-ui-follow t
;;           org-roam-ui-update-on-save t
;;           org-roam-ui-open-on-start t))

(add-hook 'prog-mode-hook #'display-line-numbers-mode) ;; enable line numbers only in programming modes
(add-hook 'prog-mode-hook #'olivetti-mode)

(electric-pair-mode 1)

(use-package haskell-mode)
(use-package php-mode)

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((python-mode . lsp-deferred)
         (c-mode . lsp-deferred)
	 (c++-mode . lsp-deferred)
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

;; Run: npm install -g pyright
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda () (require 'lsp-pyright) (lsp))))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; Major mode for OCaml programming
(use-package tuareg
  :ensure t
  :mode (("\\.ocamlinit\\'" . tuareg-mode)))

;; Major mode for editing Dune project files
(use-package dune
  :ensure t)

;; Merlin provides advanced OCaml IDE features
(use-package merlin
  :ensure t
  :config
  (add-hook 'tuareg-mode-hook #'merlin-mode)
  ;; we're using flycheck instead
  (setq merlin-error-after-save nil))


; Make company aware of merlin
(with-eval-after-load 'company
  (add-to-list 'company-backends 'merlin-company-backend))
; Enable company on merlin managed buffers
(add-hook 'merlin-mode-hook 'company-mode)

(use-package merlin-eldoc
  :ensure t
  :hook ((tuareg-mode) . merlin-eldoc-setup))

;; appends opam's bin directory to the PATH environment variable in Emacs, allowing it to find ocamlmerlin
(let ((opam-bin (expand-file-name "~/.opam/default/bin")))
  (add-to-list 'exec-path opam-bin)
  (setenv "PATH" (concat opam-bin ":" (getenv "PATH"))))


;; This uses Merlin internally
(use-package flycheck-ocaml
  :ensure t
  :config
  (flycheck-ocaml-setup))

(add-to-list 'auto-mode-alist '("\\.c0\\'" . c-mode)) ;; turn on C-mode with any file ending in .c0

(require 'why3)

;; (require 'serpent-mode) ;; request to load mode as a response to the (provide 'serpent-mode) line in the serpent-mode.el file
;; (add-to-list 'auto-mode-alist '("\\.srp\\'" . serpent-mode))

;; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;; remove error message when launching python
(setq python-shell-completion-native-enable nil)

(use-package outli
  :load-path "./scripts/outli/"
  :bind (:map outli-mode-map ; convenience key to get back to containing heading
	      ("C-c C-p" . (lambda () (interactive) (outline-back-to-heading))))
  :hook ((prog-mode text-mode) . outli-mode) ; programming modes
)

;; The following adds all our own-made themes in the themes folder to Emacs' custom themes list
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; The following adds a bunch of doom-themes to Emacs' custom themes list, which we can load using M-x load-theme

;; Doom themes: [https://github.com/doomemacs/themes]
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
)

;; Set location
;; (setq calendar-latitude 40.4)
;; (setq calendar-longitude -79.9)
;; (setq calendar-location-name "Pittsburgh, PA")

;; (setq calendar-latitude 40.4)
;; (setq calendar-longitude -3.7)
;; (setq calendar-location-name "Madrid, ES")


;; (use-package theme-changer
;;   :config
;;   ;; (change-theme 'material-light 'dracula)
;;   ;; (change-theme 'material-light 'material-light)
;;   (change-theme 'spacemacs-light 'spacemacs-dark)
;; )
;;(load-theme 'spacemacs-light t)
(load-theme 'doom-spacegrey t)

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  (setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))

(delete-selection-mode 1)    ;; You can select text and delete it by typing.
(save-place-mode 1)          ;; Remember and restore the last cursor location of opened files
(global-auto-revert-mode t)  ;; Automatically show changes if the file has changed
;; Required for gnupg (gpg) encryption to work

;; undo and redo
(use-package undo-fu
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "M-z")   'undo-fu-only-undo)
  (global-set-key (kbd "M-S-z") 'undo-fu-only-redo))

;; apply copy/cut/delete to entire line
(use-package whole-line-or-region
  :ensure t
  :config
  (whole-line-or-region-global-mode))

;; indent entire region with a tab or unindent shift tab
;; (add-hook 'prog-mode-hook
;;   (lambda ()
;;     (local-set-key (kbd "TAB")
;;       (lambda () (when (use-region-p)
;;                    (indent-rigidly (region-beginning) (region-end) 4))))
;;     (local-set-key (kbd "S-TAB")
;;       (lambda () (when (use-region-p)
;;                    (indent-rigidly (region-beginning) (region-end) -4))))))

;; highlight all occurrences of word selected / cursor at
(use-package symbol-overlay
  :ensure t
  :hook (prog-mode . symbol-overlay-mode))

(global-unset-key [S-mouse-1])
(setq shift-select-mode t)

;; Make Shift + mouse-1 set the point (start selection)
(global-set-key [S-mouse-1] 'mouse-set-point)

;; Make Shift + drag extend the selection
(global-set-key [S-down-mouse-1] 'mouse-save-then-kill)
(global-set-key [S-drag-mouse-1] 'mouse-drag-region)

(global-unset-key (kbd "C-_")) ;; remove C-S-_ to undo
(global-unset-key (kbd "C-y")) ;; remove  C-y to yank
(global-unset-key (kbd "C-x h")) ;; remove C-x h to select all 


(global-set-key (kbd "M-c") 'kill-ring-save) ;; copy
(global-set-key (kbd "M-v") 'yank) ;; paste
(global-set-key (kbd "M-z") 'undo) ;; undo
(global-set-key (kbd "M-a") 'mark-whole-buffer) ;; select all

(use-package ultra-scroll-mac
  :if (eq window-system 'mac)
  :load-path "./scripts/ultra-scroll-mac/" ; if you git clone'd instead of package-vc-install
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0) 
  :config
  (setq ultra-scroll-increment 0)        ;; smaller = smoother scroll steps
  (setq ultra-scroll-delay 0)           ;; lower = faster animation frames  
  (ultra-scroll-mac-mode 1))


;; (use-package good-scroll
;;   :ensure t
;;   :config
;;   ;; Disable native pixel scroll (optional)
;;   (when (fboundp 'pixel-scroll-precision-mode)
;;     (pixel-scroll-precision-mode -1))
;;   (good-scroll-mode 1))

(pixel-scroll-precision-mode 1)
(setq pixel-scroll-precision-use-momentum t)     ;; enable inertial/momentum scrolling
(setq pixel-scroll-precision-interpolation-factor 0.0005) ;; speed of scroll animation
(setq pixel-scroll-precision-large-scroll-height 20)  ;; default is 100

(use-package flycheck
  :defer t
  :diminish ;;explanation of what diminish does, search for "DIMINISH"
  :init (global-flycheck-mode))

(use-package real-auto-save
  :ensure t
  :init
  (setq real-auto-save-interval 1) ;; in seconds
  :config
  (add-hook 'find-file-hook #'real-auto-save-mode) ;; apply to any opened file
)

(setq make-backup-files nil)  ;; Disable backup files like file~
(setq auto-save-default nil)  ;; Disable auto-save files like #file#

(setq initial-major-mode 'org-mode)

;; (defun ar/show-welcome-buffer ()
;;   "Show *Welcome* buffer."
;;   (with-current-buffer (get-buffer-create "*Welcome*")
;;     (setq truncate-lines t)
;;     (let* ((buffer-read-only)
;;            (image-path "~/.emacs.d/images/startup-emacs.png")
;;            (image (create-image image-path))
;;            (size (image-size image))
;;            (height (cdr size))
;;            (width (car size))
;;            (top-margin (floor (/ (- (window-height) height) 2)))
;;            (left-margin (floor (/ (- (window-width) width) 2)))
;;            (prompt-title "Welcome to Emacs!"))
;;       (erase-buffer)
;;       (setq mode-line-format nil)
;;       (goto-char (point-min))
;;       (insert (make-string top-margin ?\n ))
;;       (insert (make-string left-margin ?\ ))
;;       (insert-image image)
;;       (insert "\n\n\n")
;;       (insert (make-string (floor (/ (- (window-width) (string-width prompt-title)) 2)) ?\ ))
;;       (insert prompt-title))
;;     (read-only-mode +1)
;;     (switch-to-buffer (current-buffer))
;;     (local-set-key (kbd "q") 'kill-this-buffer)))



;; (when (< (length command-line-args) 2)
;;   (add-hook 'emacs-startup-hook (lambda ()
;;                                   (when (display-graphic-p)
;;                                     (ar/show-welcome-buffer))))
;; )

;; CODE NOT WORKING
;; (pdf-tools-install) ; Very nice PDF viewer (needs separate installation)
;; (load-library "pdf-tools")

;; (setq TeX-view-program-selection '((output-pdf "pdf-tools")))
;; (setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
;; (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

;; (setq pdf-sync-backward-display-action t)
;; (setq pdf-sync-forward-display-action t)

;; (setq org-agenda-files 
;;       '("~/gonz/OrgFiles/")
;; )

;; (setq org-ellipsis " ▾")
;; (setq org-agenda-start-with-log-mode t)
;; ;; (setq org-log-done 'time) ;; This is to put in agenda time when a task was marked as done
;; (setq org-log-into-drawer t)

;; (setq org-todo-keywords
;;       '(
;; 	    (sequence "TODO" "|" "DONE")
;; 	    (sequence "ATTEND" "|" "DONE")
;; 	    (sequence "GYM" "|" "DONE")
;; 	    (sequence "FUN" "|" "DONE")
;;        )
;; )


;; (setq org-todo-keyword-faces
;;       (quote (("TODO" :foreground "deep sky blue" :weight bold)
;; 	          ("DONE" :foreground "forest green" :weight bold)
;; 		  ("ATTEND" :foreground "light salmon" :weight bold)
;;               ("GYM" :foreground "cyan" :weight bold)
;; 	          ("FUN" :foreground "medium spring green" :weight bold))
;; 	  )
;; )

;; (defun tasks ()
;;   (interactive)
;;   ;;(delete-other-windows)
;;   (find-file "~/gonz/OrgFiles/Tasks.org")
;; )

;; (defun reminders ()
;;   (interactive)
;;   ;;(delete-other-windows)
;;   (find-file "~/gonz/OrgFiles/Reminders.org")
;; )

;; (defun today ()
;;    (interactive)
;;    (delete-other-windows) 
;;    (let ((org-agenda-span 'day)) ; for this command only
;;         (org-agenda nil "a")
;; 	 )   
;;    (delete-other-windows)  
;; )

;; (setq org-agenda-span 10 ;; number of days to include in week view
;;       org-agenda-start-on-weekday nil ;; sets week view's first day to be today
;; )

;; (defun week ()
;;    (interactive)
;;    (delete-other-windows)   
;;    (let ((org-agenda-span 'week)) ; for this command only
;;         (org-agenda nil "a")
;; 	 )
;;       (delete-other-windows)  
;; )

;; (require 'org-habit)
;; (add-to-list 'org-modules 'org-habit)
;; (setq org-habit-graph-colum 80)  ;; what column in your agenda it pops up on
