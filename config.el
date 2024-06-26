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
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

(add-to-list 'load-path "~/.emacs.d/scripts/")

(add-to-list 'load-path "~/.emacs.d/scripts/nano-emacs") ;; Path to nano emacs modules (mandatory)
(require 'nano-layout) ;; Default layout (optional)

;; Theming Command line options (this will cancel warning messages)
(add-to-list 'command-switch-alist '("-dark"   . (lambda (args))))
(add-to-list 'command-switch-alist '("-light"  . (lambda (args))))
(add-to-list 'command-switch-alist '("-default"  . (lambda (args))))
(add-to-list 'command-switch-alist '("-no-splash" . (lambda (args))))
(add-to-list 'command-switch-alist '("-no-help" . (lambda (args))))
(add-to-list 'command-switch-alist '("-compact" . (lambda (args))))

;; Theme
(require 'nano-faces)
(require 'nano-theme)
(require 'nano-theme-dark)
(require 'nano-theme-light)

(cond
 ((member "-default" command-line-args) t)
 ((member "-dark" command-line-args) (nano-theme-set-dark))
 (t (nano-theme-set-light)))
(call-interactively 'nano-refresh-theme)


(require 'nano-defaults) ;; Nano default settings (optional)
(require 'nano-modeline) ;; Nano header & mode lines (optional)
(require 'nano-session) ;; Nano session saving (optional)

(when (member "-compact" command-line-args) ;; Compact layout (need to be loaded after nano-modeline)
  (require 'nano-compact))


;; Welcome message (optional)
(let ((inhibit-message t))
  (message "Welcome to GNU Emacs / N Λ N O edition")
  (message (format "Initialization time: %s" (emacs-init-time))))

;; Splash (optional)
(unless (member "-no-splash" command-line-args)
  (require 'nano-splash))

;; Help (optional)
(unless (member "-no-help" command-line-args)
  (require 'nano-help))

;; (custom-set-faces
;;  '(font-lock-keyword-face ((t (:foreground "#0222E8"))))
;;  ;;'(font-lock-function-name-face ((t (:foreground "#EB600D"))))
;;  '(font-lock-function-name-face ((t (:foreground "#EA3001")))) 
;;  '(font-lock-type-face ((t (:foreground "#61EAB3"))))
;; )

(use-package auctex)

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

(setq backup-directory-alist '((".*" . "~/.local/share/Trash/files")))

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

;; (use-package dashboard
;;   :init
;;   (setq dashboard-set-heading-icons t)
;;   (setq dashboard-set-file-icons t)
;;   (setq dashboard-banner-logo-title "Emacs Is More Than A Text Editor!")
;; ;;   ;;(setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
;;   (setq dashboard-startup-banner "~/.emacs.d/images/multicolor-gnu.jpg")  ;; use custom image as banner
;;   (setq dashboard-center-content nil) ;; set to 't' for centered content
;;   (setq dashboard-items '((recents . 5)
;;                           (agenda . 5 )
;;                           (bookmarks . 3)
;;                           (registers . 3)))
;;   :custom
;;   (dashboard-modify-heading-icons '((recents . "file-text")
;;                                     (bookmarks . "book")))
;;   :config
;;   (dashboard-setup-startup-hook))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-display-line-numbers-mode 1)
(global-visual-line-mode t)

;; full screen
;;(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; specific height and width
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 100))

(set-face-attribute 'default nil
	:height 150
	:weight 'medium)

;; Sets commented regions to itallic
(set-face-attribute 'font-lock-comment-face nil
   :slant 'italic)

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(use-package diminish)

(use-package flycheck
  :defer t
  :diminish ;;explanation of what diminish does, search for "DIMINISH"
  :init (global-flycheck-mode))

(use-package git-timemachine
  :after git-timemachine
  :hook (evil-normalize-keymaps . git-timemachine-hook)
  :config
    (evil-define-key 'normal git-timemachine-mode-map (kbd "C-j") 'git-timemachine-show-previous-revision)
    (evil-define-key 'normal git-timemachine-mode-map (kbd "C-k") 'git-timemachine-show-next-revision)
)

(use-package magit)

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

(require 'org)

(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t;; hide markers such as ** for bold, or __ for underline
 )

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(setq org-edit-src-content-indentation 0) ;; sets org code indentation to 0 spaces by default

(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.2))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.0))))
 '(org-level-3 ((t (:inherit outline-5 :height 1.0))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-3 :height 1.0))))
 '(org-level-6 ((t (:inherit outline-6 :height 1.0))))
 '(org-level-7 ((t (:inherit outline-7 :height 1.0)))))

(font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

(setq org-agenda-files 
      '("~/gonz/OrgFiles/")
)

(setq org-ellipsis " ▾")
(setq org-agenda-start-with-log-mode t)
;; (setq org-log-done 'time) ;; This is to put in agenda time when a task was marked as done
(setq org-log-into-drawer t)

(setq org-todo-keywords
      '(
	    (sequence "TODO" "|" "DONE")
	    (sequence "ATTEND" "|" "DONE")
	    (sequence "GYM" "|" "DONE")
	    (sequence "FUN" "|" "DONE")
       )
)


(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "deep sky blue" :weight bold)
	          ("DONE" :foreground "forest green" :weight bold)
		  ("ATTEND" :foreground "light salmon" :weight bold)
              ("GYM" :foreground "cyan" :weight bold)
	          ("FUN" :foreground "medium spring green" :weight bold))
	  )
)

(defun tasks ()
  (interactive)
  ;;(delete-other-windows)
  (find-file "~/gonz/OrgFiles/Tasks.org")
)

(defun reminders ()
  (interactive)
  ;;(delete-other-windows)
  (find-file "~/gonz/OrgFiles/Reminders.org")
)

(defun today ()
   (interactive)
   (delete-other-windows) 
   (let ((org-agenda-span 'day)) ; for this command only
        (org-agenda nil "a")
	 )   
   (delete-other-windows)  
)

(setq org-agenda-span 10 ;; number of days to include in week view
      org-agenda-start-on-weekday nil ;; sets week view's first day to be today
)

(defun week ()
   (interactive)
   (delete-other-windows)   
   (let ((org-agenda-span 'week)) ; for this command only
        (org-agenda nil "a")
	 )
      (delete-other-windows)  
)

(defun config ()
  (interactive)
  ;; (delete-other-windows)
  (find-file "~/.emacs.d/config.org")
)

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/gonz/Obsidian")
  (org-roam-completion-everywhere t)
  :bind (("C-c r t" . org-roam-buffer-toggle)
         ("C-c r f" . org-roam-node-find)
         ("C-c r i" . org-roam-node-insert)
	     ("C-c r g" . org-roam-ui-open)
	     ("C-c r G" . org-roam-graph)
	    )
  :config
  (org-roam-db-autosync-mode)
  (org-roam-setup))

;; Required dependencies for ui graph package
(use-package websocket
  :after org-roam)

(use-package org-roam-ui
    :after org-roam
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(use-package outli
  :load-path "./scripts/outli/"
  :bind (:map outli-mode-map ; convenience key to get back to containing heading
	      ("C-c C-p" . (lambda () (interactive) (outline-back-to-heading))))
  :hook ((prog-mode text-mode) . outli-mode) ; programming modes
)

;; (use-package outshine
;;   :config
;;   (add-hook 'python-mode-hook 'outshine-mode)
;;   (add-hook 'c-mode-hook 'outshine-mode)
;;   (add-hook 'cpp-mode-hook 'outshine-mode)
;; )

;; Helps avois blurry PDFs on Mac retina display
(setq pdf-view-use-scaling t)


;; CODE NOT WORKING

;; (pdf-tools-install) ; Very nice PDF viewer (needs separate installation)
;; (load-library "pdf-tools")

;; (setq TeX-view-program-selection '((output-pdf "pdf-tools")))
;; (setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
;; (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

;; (setq pdf-sync-backward-display-action t)
;; (setq pdf-sync-forward-display-action t)

;; ;; Auto revert in doc view buffers, yeah! Plus new bindings
;; (if (< emacs-major-version 23)
;;     ()
;;   (add-hook 'pdf-view-mode-hook 'auto-revert-mode)
;;   (add-hook 'pdf-view-mode-hook 'my-pdf-view-set-bindings))

(electric-pair-mode 1) ;; auto fill parentheses

(use-package haskell-mode)
(use-package php-mode)

(add-to-list 'auto-mode-alist '("\\.c0\\'" . c-mode)) ;; turn on C-mode with any file ending in .c0

(require 'serpent-mode) ;; request to load mode as a response to the (provide 'serpent-mode) line in the serpent-mode.el file
(add-to-list 'auto-mode-alist '("\\.srp\\'" . serpent-mode))

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;; remove error message when launching python
(setq python-shell-completion-native-enable nil)

;; The following adds all our own-made themes in the themes folder to Emacs' custom themes list
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; The following adds a bunch of doom-themes to Emacs' custom themes list, which we can load using M-x load-theme

;; Dom themes: [https://github.com/doomemacs/themes]
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
;; ;; (setq calendar-latitude 40.4)
;; ;; (setq calendar-longitude -79.9)
;; ;; (setq calendar-location-name "Pittsburgh, PA")

;; (setq calendar-latitude 40.4)
;; (setq calendar-longitude -3.7)
;; (setq calendar-location-name "Madrid, ES")


;; (use-package theme-changer
;;   :config
;;   (change-theme 'leuven 'doom-Iosvkem)
;;   ;; (change-theme 'doom-Iosvkem 'doom-Iosvkem)
  
;; )



(use-package treemacs)

(use-package ultra-scroll-mac
  :if (eq window-system 'mac)
  :load-path "./scripts/ultra-scroll-mac/" ; if you git clone'd instead of package-vc-install
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0) 
  :config
  (ultra-scroll-mac-mode 1))

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  (setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))

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

(delete-selection-mode 1)    ;; You can select text and delete it by typing.
(save-place-mode 1)          ;; Remember and restore the last cursor location of opened files
(global-auto-revert-mode t)  ;; Automatically show changes if the file has changed
;; Required for gnupg (gpg) encryption works
(defvar epa-pinentry-mode)
(setq epa-pinentry-mode 'loopback)
