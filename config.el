(when (eq system-type 'darwin)
  (let ((custom-paths '("/opt/homebrew/opt/coreutils/libexec/gnubin"   ;; 1. GNU Utilities (Highest Priority)
                        "/opt/homebrew/bin"                            ;; 2. Apple Silicon Homebrew (gopls, fd)
                        "/usr/local/bin")))                            ;; 3. Intel Mac Homebrew

    ;; We loop through the list in reverse order
    ;; Because we push each path to the *front* of Emacs's list,
    ;; reversing ensures the GNU utilities end up at the very front!
    (dolist (dir (reverse custom-paths))
      (when (file-directory-p dir)
        
        ;; 1. Tell Emacs where to look for commands
        (add-to-list 'exec-path dir)
        
        ;; 2. Tell the internal terminal where to look (if not already there)
        (unless (string-match-p (regexp-quote dir) (getenv "PATH"))
          (setenv "PATH" (concat dir ":" (getenv "PATH"))))))))

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

;; LAZY-LOADING ENGINE
;; this prevents Emacs from checking or initializing packages on startup
;; It will only load a package when you actually execute a command that needs it.
(require 'use-package)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 60)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  ;; Run the update check smoothly in the background post-startup (to reduce startup time)
  (run-with-idle-timer 5 nil #'auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                   (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(setq mac-command-modifier 'meta) ;; Cmd as M
(global-set-key (kbd "M-c") 'kill-ring-save) ;; copy
(global-set-key (kbd "M-v") 'yank) ;; paste
(global-set-key (kbd "M-z") 'undo) ;; undo
(global-set-key (kbd "M-a") 'mark-whole-buffer) ;; select all

(global-unset-key (kbd "C-_")) ;; remove C-S-_ to undo
(global-unset-key (kbd "C-y")) ;; remove  C-y to yank
(global-unset-key (kbd "C-x h")) ;; remove C-x h to select all
(global-unset-key (kbd "C-x C-s")) ;; remove save, its automatic

(global-unset-key [S-mouse-1])
(setq shift-select-mode t)

;; Make Shift + mouse-1 set the point (start selection)
(global-set-key [S-mouse-1] 'mouse-set-point)

;; Make Shift + drag extend the selection
(global-set-key [S-down-mouse-1] 'mouse-save-then-kill)
(global-set-key [S-drag-mouse-1] 'mouse-drag-region)

(defun my/backward-delete-word-no-clipboard ()
  "Delete previous word without affecting the clipboard."
  (interactive)
  (let ((p (point)))
    (backward-word 1)
    (delete-region (point) p)))

(global-set-key (kbd "M-<backspace>") #'my/backward-delete-word-no-clipboard)

;; config
(defun config ()
  (interactive)
  ;; (delete-other-windows)
  (find-file "~/.emacs.d/config.org")
)

;; gonz
(defun gonz ()
  (interactive)
  ;; (delete-other-windows)
  (dired "~/gonz/")
)

;; cs
(defun cs ()
  (interactive)
  ;; (delete-other-windows)
  (dired "~/gonz/cs")
)

;; (use-package dashboard
;;   :ensure t
;;   :after all-the-icons
;;   :config
;;   (require 'all-the-icons)
;;   (dashboard-setup-startup-hook)
  
;;   ;; UI Settings
;;   (setq dashboard-center-content t)
;;   (setq dashboard-startup-banner 'logo)
;;   (setq dashboard-banner-logo-title "E M A C S")
;;   (setq dashboard-set-heading-icons t)
;;   (setq dashboard-set-file-icons t)
;;   (setq dashboard-display-icons-p t)
;;   (setq dashboard-icon-type 'all-the-icons)

;;   ;; Configure Vanilla Widgets
;;   (setq dashboard-items '((projects . 4)
;;                           (recents  . 4)))
  
;;   (setq dashboard-titles '((recents  . "Recent Files")
;;                            (projects . "Jump To Goals & Work")))

  
;;   ;; Hardcoded list of files to display under the projects widget
;;   (setq dashboard-projects-list '("~/gonz/goals/social.org"
;;                                   "~/gonz/goals/free-time.org"
;;                                   "~/gonz/goals/finances.org"
;;                                   "~/gonz/goals/two-sigma.org")))

(load-file "~/.emacs.d/letters.el")
;; 1. Tell Emacs to open the buffer on startup, but leave it blank for a split second
(setq initial-buffer-choice (lambda () (get-buffer-create "*Letters*")))

;; 2. Draw the art exactly ONCE, after Treemacs has split the window (so calculates centering based on actual buffer size after treemacs is loaded)
(add-hook 'window-setup-hook
          (lambda ()
            (with-current-buffer "*Letters*"
              (letters-setup-ascii-banner))))

(setq initial-major-mode 'org-mode)

(menu-bar-mode -1) ;; no menubar
(tool-bar-mode -1) ;; no toolbars
(scroll-bar-mode -1) ;; no scroll bars
(set-face-attribute 'fringe nil :background 'unspecified) ;; no fringes on the sides



;; Spacing: Only apply to Org mode and plain text files
(defun my/set-text-spacing ()
  (setq-local line-spacing 0.2))
(add-hook 'org-mode-hook #'my/set-text-spacing)
(add-hook 'text-mode-hook #'my/set-text-spacing)

;; transparent title bar
;; Hide the title bar string but keep the window control buttons
(setq frame-title-format nil)

;; Use a transparent titlebar to let it merge seamlessly with the Emacs background
(add-to-list 'default-frame-alist '(titlebar-value . nil))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(setq ns-use-proxy-icon nil)

;; display line nums in progr mode
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(global-visual-line-mode t) ;; lines wrap around

;; import doom-themes
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t) ;; if nil, bold is universally disabled
  (setq doom-themes-enable-italic t) ;; if nil, italics is universally disabled  
  (doom-themes-visual-bell-config);; enable flashing mode-line on errors
  ;; (doom-themes-org-config) ;; corrects (and improves) org-mode's native fontification
)

;; import github-emacs-theme
;; (use-package github-dark-vscode-theme :ensure t)
(use-package vscode-dark-plus-theme
  :ensure t
  :config
  ;; Remove the border around the TODO word on org-mode files
  (setq vscode-dark-plus-box-org-todo nil)
  ;; Do not set different heights for some org faces
  (setq vscode-dark-plus-scale-org-faces nil)
  ;; Avoid inverting hl-todo face
  (setq vscode-dark-plus-invert-hl-todo nil)
  ;; Configure current line highlighting style (works best with Emacs 28 or newer)
  (setq vscode-dark-plus-render-line-highlight 'line))

;; import my custom themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Changing from light to dark depending on Mac OS system
(use-package auto-dark
  :config
  ;; 1. Set themes
  (setq auto-dark-themes '((vscode-dark-plus) (modus-operandi)))

  ;; 2. Define visual tweaks that themes natively try to overwrite
  (defun my/apply-theme-tweaks ()
    "Enforces custom UI choices regardless of what the active theme dictates."
    ;; Fix fringes: Keep them transparent
    (set-face-attribute 'fringe nil :background 'unspecified)
    
    ;; Dynamically steal the exact background colors from your current theme.
    ;; The 't' resolves inheritance, and 'or' catches any nil values during fast theme switches.
    (let ((bg-main (or (face-background 'default nil t) 'unspecified))
          (bg-inactive (or (face-background 'mode-line-inactive nil t) 'unspecified)))
      
      ;; The bar itself matches the inactive mode line
      (set-face-attribute 'tab-bar nil 
                          :background bg-inactive :box 'unspecified :font "JetBrains Mono" :height 150)
      
      ;; Active tab forcibly matches the dark editor background
      (set-face-attribute 'tab-bar-tab nil 
                          :background bg-main :foreground "#E5A822" :box 'unspecified :font "JetBrains Mono" :height 150 :overline "#E5A822" :weight 'bold)
      
      ;; Inactive tabs forcibly match the bar background
      (set-face-attribute 'tab-bar-tab-inactive nil 
                          :background bg-inactive :foreground 'unspecified :box 'unspecified :font "JetBrains Mono" :height 150 :overline nil :weight 'normal)))

  ;; 3. Define the title bar update functions
  (defun my/set-titlebar-dark ()
    "Forces the macOS title bar to dark mode and applies theme tweaks."
    (setf (alist-get 'ns-appearance default-frame-alist) 'dark)
    (mapc (lambda (frame) (modify-frame-parameters frame '((ns-appearance . dark)))) (frame-list))
    (my/apply-theme-tweaks))

  (defun my/set-titlebar-light ()
    "Forces the macOS title bar to light mode and applies theme tweaks."
    (setf (alist-get 'ns-appearance default-frame-alist) 'light)
    (mapc (lambda (frame) (modify-frame-parameters frame '((ns-appearance . light)))) (frame-list))
    (my/apply-theme-tweaks))

  ;; 4. Hook them into auto-dark's transitions
  (add-hook 'auto-dark-dark-mode-hook #'my/set-titlebar-dark)
  (add-hook 'auto-dark-light-mode-hook #'my/set-titlebar-light)

  ;; 5. Start auto-dark LAST so it triggers the hooks correctly on initialization
  (auto-dark-mode 1))

;; cursor
(setq-default cursor-type '(bar . 2))
;; Apple Notes Yellow Accent
(set-cursor-color "#E5A822")            ;; Cursor color

;; JetBrains Mono for code...
(set-face-attribute 'default nil :font "JetBrains Mono" :height 160 :weight 'medium)
;; Apple notes font for text
(set-face-attribute 'variable-pitch nil :font "SF Pro Text" :height 180 :weight 'normal)
;; Make bold be semi-bold, looks more modern
(set-face-attribute 'bold nil :weight 'semi-bold)

(setq-default mode-line-format nil)

;; (use-package mood-line
;;   :custom
;;   ;; Use pretty Fira Code-compatible glyphs
;;   (mood-line-glyph-alist mood-line-glyphs-fira-code)
;;   :config
;;   (mood-line-mode) ;; enable mood-line

;;   (setq mood-line-format
;;         (mood-line-defformat
;;          :left
;;          (((mood-line-segment-buffer-status) . " ")
;;           ((mood-line-segment-buffer-name)   . " : ")
;;           (mood-line-segment-major-mode))
;;          :right
;;          (((mood-line-segment-scroll)             . " ")
;;           ((mood-line-segment-cursor-position)    . "  ")
;;           ((when (mood-line-segment-checker) "|") . "  ")
;;           ((mood-line-segment-checker)            . "  ")))))

;; Disable native macOS fullscreen virtual spaces transition
(setq ns-use-native-fullscreen nil)

;; Force pixel-precise resizing to eliminate gaps at screen boundaries
(setq frame-resize-pixelwise t)

;; Tell Emacs to maximize the window on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package all-the-icons
  :if (display-graphic-p))

;; 0..100 (100 = opaque, 0 = fully transparent)
;; (add-to-list 'default-frame-alist '(alpha . (90 . 90)))
;; (add-to-list 'default-frame-alist '(alpha . (95 . 95)))
;; (add-to-list 'default-frame-alist '(alpha . (98 . 98)))

(pixel-scroll-precision-mode 1)

;; Activate Yamamoto Mitsuharu's native smooth scrolling for Mac
;; (mac-mouse-wheel-mode 1)

;; Force Emacs to fetch the repository if you don't have it locally
(unless (package-installed-p 'image-slicing)
  (package-vc-install "https://github.com/ginqi7/image-slicing"))

(use-package image-slicing
  :config
  ;; -- EWW Configuration --
  ;; Cancels the default eww image render and uses image-slicing
  (add-to-list 'shr-external-rendering-functions '(img . image-slicing-tag-img))
  (push #'image-slicing-mode eww-after-render-hook)

  ;; -- Elfeed Configuration (Optional) --
  ;; Advice `elfeed-show-entry` to trigger `image-slicing-mode`
  (advice-add #'elfeed-show-entry :after #'image-slicing-mode))

(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode +1))

(use-package olivetti
  :ensure t
  :hook ((text-mode . olivetti-mode)
         (prog-mode . olivetti-mode))
  :custom
  ;; A float forces a strict window percentaged
  (olivetti-body-width 0.85)
  :config
  ;; remove ugly fringes
  (fringe-mode 0)
)

;; make divider color match theme color
(set-face-background 'window-divider
                     (face-background 'mode-line-inactive))
;; 't' means both bottom and right
(setq window-divider-default-places t)
;; Thickness of horizontal line in pixels
(setq window-divider-default-bottom-width 1)
;; Thickness of vertical line in pixels
(setq window-divider-default-right-width 1)
;; Turn the mode on
(window-divider-mode 1)

;; 1. Enable built-in workspace tabs
(tab-bar-mode 1)

;; 2. Core Visuals
(setq tab-bar-show 1)                      
(setq tab-bar-close-button-show nil)       
(setq tab-bar-new-button-show nil)         

;; 3. Modern UI Styling (The "Browser" Illusion)
(setq tab-bar-tab-name-format-function
      (lambda (tab i)
        (let ((current-p (eq (car tab) 'current-tab)))
          (propertize
           (concat "  " (alist-get 'name tab) "  ")
           'face (if current-p 'tab-bar-tab 'tab-bar-tab-inactive)))))

;; 4. New Tab Behavior
(defun my/tab-bar-new-untitled-buffer ()
  "Generates a new untitled buffer instead of *scratch*."
  (generate-new-buffer "untitled"))

(setq tab-bar-new-tab-choice #'my/tab-bar-new-untitled-buffer)

;; 5. Shortcuts
(defun my/tab-close-and-kill-buffers ()
  "Close the current tab and kill all buffers that were visible inside it."
  (interactive)
  ;; 1. Capture all unique buffers visible in the current tab's windows
  (let ((buffers-to-kill (delete-dups (mapcar #'window-buffer (window-list)))))
    ;; 2. Close the tab first (switches to the next tab safely)
    (tab-close)
    ;; 3. Loop through and kill the captured buffers
    (dolist (buf buffers-to-kill)
      (when (and (buffer-live-p buf)
                 (not (minibufferp buf))
                 (not (string-prefix-p " " (buffer-name buf)))) ; Skip internal hidden buffers
        (kill-buffer buf)))))

(global-set-key (kbd "M-t") #'tab-new)
(global-set-key (kbd "M-w") #'my/tab-close-and-kill-buffers)
(global-set-key (kbd "<C-tab>") #'tab-next)
(global-set-key (kbd "<C-S-tab>") #'tab-previous)

;; (use-package centaur-tabs
;;   :ensure t
;;   :demand t
;;   :config
;;   ;; 1. Core Visuals
;;   (setq centaur-tabs-style "bar"
;;         centaur-tabs-height 32
;;         centaur-tabs-set-icons t
;;         centaur-tabs-show-navigation-buttons nil
;;         centaur-tabs-set-modified-marker t
;;         centaur-tabs-show-new-tab-button nil)
        
;;   (centaur-tabs-change-fonts "JetBrains Mono" 130)

;;   ;; 2. WRAP AROUND TABS
;;   (setq centaur-tabs-cycle-scope 'tabs)

;;   ;; 3. ONE SINGLE MAIN TAB GROUP (With a separate group for background utilities)
;;   (defun my/centaur-tabs-buffer-groups ()
;;     (let ((name (buffer-name)))
;;       (if (member name '("*Ilist*" "*scratch*" "*Messages*" "*Warnings*" "*Compile-Log*"))
;;           (list "System")  ;; Banished to their own hidden group
;;         (list "Main"))))   ;; Your actual workspace tabs
;;   (setq centaur-tabs-buffer-groups-function #'my/centaur-tabs-buffer-groups)

;;   ;; 4. THE CLEANED HIDE FUNCTION
;;   ;; Only hide actual internal/minibuffer noise so the tab bar never breaks.
;;   (defun my/centaur-tabs-hide-tab (buf)
;;     (let ((name (buffer-name buf)))
;;       (or (string-prefix-p " " name)
;;           (string-prefix-p "*Minibuf-" name))))
;;   (setq centaur-tabs-hide-tab-function #'my/centaur-tabs-hide-tab)

;;   ;; Force the UI to refresh immediately
;;   (when (fboundp 'centaur-tabs-display-update)
;;     (centaur-tabs-display-update))

;;   ;; 5. Sane Shortcuts
;;   (defun my/new-empty-buffer ()
;;     "Create a new untitled buffer."
;;     (interactive)
;;     (switch-to-buffer (generate-new-buffer "untitled")))

;;   (global-set-key (kbd "M-t") #'my/new-empty-buffer)
;;   (global-set-key (kbd "M-w") 'kill-current-buffer)
;;   (global-set-key (kbd "<C-tab>")  'centaur-tabs-forward)
;;   (global-set-key (kbd "<C-S-tab>") 'centaur-tabs-backward)

;;   ;; Turn the package on
;;   (centaur-tabs-mode t))

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

(use-package corfu
  :custom
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-delay 0.1)         ;; Very fast popup
  (corfu-auto-prefix 2)          ;; Start after 2 chars
  (corfu-quit-no-match t)        ;; Quit if no match
  :init
  (global-corfu-mode))

;; [NEW] Vertico: Fast, minimalist vertical completions
(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t)) ;; Cycle through options at the bottom

;; [NEW] consult provides asynchronous search, live previews, and enhanced navigation commands
(use-package consult
  :ensure t
  :bind
  ;; C-c r to find recent files quickly
  (("C-c r" . consult-recent-file)
  ;; Replace default search with Consult's powerful ripgrep integration
  ("C-s" . consult-line)) 
  ;; Spotlight-like file search
  (("C-x f" . my/spotlight-gonz))
  :config
  (defun my/spotlight-gonz ()
    "Spotlight-like recursive search for files inside ~/gonz/"
    (interactive)
    (let* ((default-directory "~/gonz/")
           ;; 'fd' searches for files and directories
           (paths (split-string (shell-command-to-string "fd --hidden --exclude .git .") "\n" t))
           (choice (completing-read "Rec Find (~/gonz/): " paths)))
      (when choice
        (find-file (expand-file-name choice "~/gonz/")))))
  :init
  ;; Enhances your existing Vertico completion
  (setq completion-in-region-function #'consult-completion-in-region))

;; no more searching backwards, rely on consult
(keymap-unset global-map "C-r")

;; [NEW] Marginalia: Adds beautiful descriptions/docs next to Vertico commands
(use-package marginalia
:init
(marginalia-mode))

;; [NEW] Orderless: improved file search, can type fragments of words separated by spaces
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package flycheck
  :defer t
  :diminish ;;explanation of what diminish does, search for "DIMINISH"
  :init (global-flycheck-mode)
  :config
  ;; only check syntax when file saved (to not cause typing lag)
  (setq flycheck-check-syntax-automatically '(save mode-enable))
)

;; (use-package imenu-list
;;   :ensure t
;;   :bind (("M-i" . imenu-list-smart-toggle))
;;   ;; Hides centaur tabs only inside the imenu-list buffer
;;   :hook ((imenu-list-major-mode . centaur-tabs-local-mode)
;;          (org-mode . imenu-list-minor-mode)
;;          (prog-mode . imenu-list-minor-mode))
;;   :config
;;   (setq imenu-list-position 'left)
;;   (setq imenu-list-size 30)
;;   (setq imenu-list-focus-after-activation nil)
;;   (setq imenu-list-auto-resize t)
;;   ;; Dynamic Theme Matching (Inherit from your active theme's faces)
;;   (set-face-attribute 'imenu-list-entry-face-0 nil :inherit 'font-lock-keyword-face :weight 'bold)
;;   (set-face-attribute 'imenu-list-entry-face-1 nil :inherit 'font-lock-function-name-face :weight 'normal)
;;   (set-face-attribute 'imenu-list-entry-face-2 nil :inherit 'font-lock-variable-name-face)
;;   (set-face-attribute 'imenu-list-entry-face-3 nil :inherit 'font-lock-comment-face)
;; )

(use-package magit
  :defer t
  :bind (:map magit-mode-map
              ("C-M-f" . magit-section-forward)
              ("C-M-b" . magit-section-backward))
  :hook
  ((magit-pre-refresh  . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (setq magit-mode-quit-window 'magit-restore-window-configuration
		magit-auto-revert-mode t)
  ; Remove tags from status buffer headings to speed up refresh slightly
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header))

(use-package git-timemachine
  :defer t)

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode))

(use-package org
  :defer t
  :hook
  (org-mode . visual-line-mode) ;; line wrap
  (org-mode . org-indent-mode)  ;; <--- FORCE INDENTATION ON
  :config
  ;; Inline formatting
  (setq org-hide-emphasis-markers t)
  (setq org-startup-with-inline-images t)

  ;; short keystrokes for org block autocomplete
  ;; e.g. <s -> source block
  (require 'org-tempo) 

  ;; Clean up source blocks
  (setq org-edit-src-content-indentation 0)
  
  ;; #+title text 4x larger
  (set-face-attribute 'org-document-title nil :height 2.0)
  
  ;; Load code block execution support
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)))

  ;; Make org subscripts defined with: _{}
  (setq org-use-sub-superscripts '{})
)

;; [NEW] Mixed-pitch: This ensures that while your regular text uses Sans-Serif,
;; your Org-mode code blocks and tables stay cleanly aligned in Monospace!
(use-package mixed-pitch
:hook (org-mode . mixed-pitch-mode)
:config
 ;; tells mixed-pitch to respect your custom variable-pitch height!
  (setq mixed-pitch-set-height t))

;; Org modern
 (use-package org-modern
  :hook (org-mode . org-modern-mode)
  :config
  ;; replace the heading stars
  (setq org-modern-star 'replace)
  ;; set the replacement character to a blank space
  (setq org-modern-replace-stars " ")

  ;; Hide folding arrows
  (setq org-modern-fold-stars nil)
  
  ;; Clean Apple-style bullet points for standard lists (- or + or *)
  (setq org-modern-list 
        '((43 . "•")) ; plus sign becomes a solid dot
  )

;; makes bulleted and numbered lists behave like modern editor (gdocs, pages)
;; (renumbering, automatic new bullet when press enter, delete when press enter, ...)
(use-package org-autolist
  :hook (org-mode . org-autolist-mode))

;; Toc-org: table of contents
(use-package toc-org
  :hook (org-mode . toc-org-enable))
)

;; Better previews (SVG) if available
(setq org-preview-latex-default-process 'dvisvgm)
;; Adjust size
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1))
;; Auto-preview LaTeX fragments when you open the file
(setq org-startup-with-latex-preview t)

;; make headings scale up like Apple Notes titles
(custom-set-faces
 '(org-document-title ((t (:inherit variable-pitch :height 1.3 :weight bold))))
 '(org-level-1 ((t (:inherit variable-pitch :height 1.2 :weight bold))))
 '(org-level-2 ((t (:inherit variable-pitch :height 1.1 :weight semi-bold))))
 '(org-level-3 ((t (:inherit variable-pitch :height 1.0 :weight medium))))
 '(org-level-4 ((t (:inherit variable-pitch :height 1.0 :weight normal))))
 ;; ensures code blocks inside Org-mode don't have the line spacing applied to text org mode lines
 '(org-block ((t (:inherit fixed-pitch :line-spacing 0.0))))
)

(use-package auctex
  :hook
  (LaTeX-mode . turn-on-prettify-symbols-mode)
  (LaTeX-mode . reftex-mode)
  (LaTeX-mode . outline-minor-mode)
  ;; (LaTeX-mode . olivetti-mode)
  :config
  (setq TeX-PDF-mode t) ;; always build PDFs
  (setq TeX-engine 'xetex) ;; make the engine XeTeX
  (setq TeX-command-default "LaTeX"))

(setq reftex-toc-split-windows-horizontally t
	  reftex-toc-split-windows-fraction     0.2)

;; highlight all occurrences of word selected / cursor at
(use-package symbol-overlay
  :hook (prog-mode . symbol-overlay-mode))

;; automatic closing parenthesis
(electric-pair-mode 1)
;; disable <> pairing
(setq electric-pair-inhibit-predicate
      (lambda (c)
        (if (char-equal c ?<) t (electric-pair-default-inhibit c))))

;; line numbers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
;; diff-hl (show git changes on buffer left side)
(use-package diff-hl
  :hook (prog-mode . diff-hl-mode))

(add-to-list 'load-path "~/.emacs.d/language-support/") ;; ocaml, serpent-mode, why3
(add-to-list 'auto-mode-alist '("\\.c0\\'" . c-mode)) ;; c0
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))
;; terraform
(use-package terraform-mode)


;; why3 (not needed)
;; (require 'why3)
;; serpent (too long, not worth activating)
;; (add-to-list 'load-path "~/.emacs.d/language-support/serpent-mode.el")

(setq-default indent-tabs-mode nil)  ;; always use spaces
(setq-default tab-width 2)           ;; how wide to *show* any existing tabs
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq indent-tabs-mode nil  ; enforce spaces in C/C++
                  c-basic-offset 2)))

(use-package lsp-mode
  :custom
  ;; Tell LSP not to auto-configure company-mode, using corfu
  (lsp-completion-provider :none)
  :commands (lsp lsp-deferred)
  :hook ((python-mode . lsp-deferred)
         (c-mode . lsp-deferred)
	 ;; (c++-mode . lsp-deferred)
         (go-mode . lsp-deferred))
  :config
  (global-set-key [C-mouse-1] #'lsp-ui-peek-find-definitions)
  (setq lsp-headerline-breadcrumb-enable 1)) ; we'll use a different method for function tracking

(use-package lsp-ui
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

;; 1. PYRIGHT (installed with npm install -g pyright)
(use-package lsp-pyright
  :hook (python-mode . (lambda () (require 'lsp-pyright) (lsp))))

;; 2. Gopls (go server, installed via homebrew)

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
  :config
  (whole-line-or-region-global-mode))

(use-package super-save
  :config
  (setq super-save-auto-save-when-idle t
        super-save-idle-duration 1) ;; save after 1s idle
  (super-save-mode +1))

(setq make-backup-files nil)  ;; Disable backup files like file~
(setq auto-save-default nil)  ;; Disable auto-save files like #file#

(use-package org-download
  :after org ;; lazily loads when org opens, not on startup, saves startup time
  :custom
  (org-download-method 'directory)
  (org-download-image-dir "org-imgs")
  (org-download-heading-lvl nil) ; this must be nil to stop folder creation
  (org-download-timestamp-prefix-format nil) ; Optional: prevents timestamps in file names
  (org-image-actual-width nil)
  :config
  (add-hook 'org-mode-hook 'org-download-enable) ;; enables in org mode
  (add-hook 'dired-mode-hook 'org-download-enable) ;; enables in dired mode
  
  (setq org-download-annotate-function
        (lambda (link)
          "#+ATTR_ORG: :width 500\n")))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "M-k") #'org-insert-link))

(use-package wrap-region
  :ensure t
  :config
  ;; Enable wrap-region globally across all modes
  (wrap-region-global-mode 1)
  
  ;; Define the global wrappers and their shortcut keys
  ;; Syntax: (wrap-region-add-wrapper "left" "right" "trigger-key")
  (wrap-region-add-wrapper "/" "/" "M-i")
  (wrap-region-add-wrapper "_" "_" "M-u"))

;; when C-c C-f, start already by "~/gonz/" instead of "~/"
(setq default-directory "~/gonz/")

(use-package treemacs
  :ensure t
  :bind
  ("C-t" . treemacs)
  :hook
  (emacs-startup . treemacs)
  :config
  ;; treemacs follows whichever buffer you're on
  (treemacs-follow-mode t)
  ;; treemacs follows and sets the correct project root
  (treemacs-project-follow-mode t)
  ;; make tree font 85% the size of main buffer font
  (set-face-attribute 'treemacs-root-face nil :height 0.85)
  (set-face-attribute 'treemacs-directory-face nil :height 0.85)
  (set-face-attribute 'treemacs-file-face nil :height 0.85)
  (set-face-attribute 'treemacs-tags-face nil :height 0.85)
  ;; explained here: https://github.com/Alexander-Miller/treemacs#git-mode
  (treemacs-git-mode 'extended))

;; To get the specific VS Code style icons:
(use-package treemacs-all-the-icons
  :ensure t
  :config
  (treemacs-load-theme "all-the-icons"))

;; Tell Emacs where to look for Homebrew binaries and GNU utilities on macOS
(when (eq system-type 'darwin)
  ;; For Apple Silicon Macs (M1/M2/M3/M4)
  (let ((gnu-path "/opt/homebrew/opt/coreutils/libexec/gnubin"))
    (when (file-directory-p gnu-path)
      (setenv "PATH" (concat gnu-path ":" (getenv "PATH")))
      (add-to-list 'exec-path gnu-path))))

(use-package dirvish
  :init
  ;; Open dirvish instead of default dired globally
  (dirvish-override-dired-mode)  
  :custom
  ;; Display file icons using all-the-icons
  ;; Adding 'subtree-state' enables built-in VSCode-style folder expansion
  (dirvish-attributes '(all-the-icons file-size subtree-state collapse))  
  :config
  ;; Enable live preview of images, PDFs, and code files as you scroll
  (dirvish-peek-mode)
  ;; Make the sidebar automatically follow the currently active buffer
  (dirvish-side-follow-mode 1)
  
  ;; 3. No Text Wrapping: Truncate lines in Dirvish/Dired
  (add-hook 'dired-mode-hook (lambda () (setq truncate-lines t)))
  
  ;; Custom Keybindings inside Dirvish/Dired
  :bind (:map dirvish-mode-map
         ("h"   . dirvish-up-directory)     ;; Left/Up: Go to parent folder
         ("l"   . dired-find-file)          ;; Right/Down: Open file or folder
         ("q"   . dirvish-quit)             ;; Cleanly close Dirvish and restore windows
         ("a"   . dirvish-quick-access)     ;; Jump to bookmarked folders
         ("y"   . dirvish-yank-menu)        ;; Advanced copy/paste/move engine
         
         ;; 1. Search current directory and subdirectories (requires 'fd' installed)
         ("f"   . dirvish-fd)              
         
         ;; --- Essential Recommended Shortcuts ---
         ("TAB" . dirvish-subtree-toggle)   ;; Expand/collapse directory trees in place
         ("t"   . dirvish-layout-toggle)    ;; Instantly hide/show the preview pane
         ("s"   . dirvish-quicksort)        ;; Quick menu to sort by date, size, name, etc.
         ("M-n" . dirvish-history-go-forward) ;; Navigate forward in dirvish history
         ("M-p" . dirvish-history-go-backward) ;; Navigate backward in dirvish history
         ("E"   . wdired-change-to-wdired-mode))) ;; (Crucial) Bulk-edit file names

(use-package leetcode
    :config
    (setq leetcode-prefer-language "cpp")
    (setq leetcode-save-solutions t)
    (setq leetcode-directory "~/gonz/cs/notes/algorithms/leetcode"))
