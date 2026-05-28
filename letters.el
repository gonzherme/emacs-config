;;; letters.el --- my startup screen, modified version of: https://github.com/wwxodus/Letters/blob/main/Letters.el

;;; Commentary:

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'button)

;; Optional but highly recommended package for dashboard icons.
(declare-function all-the-icons-icon-for-file "all-the-icons")
(declare-function all-the-icons-octicon "all-the-icons")

(defgroup letters nil
  "Customizable ASCII banner and dashboard options."
  :group 'applications)

(defcustom letters-banner-width 80
  "Default target width for the letters banner alignment."
  :type 'integer
  :group 'letters)

(defcustom letters-top-pos 3
  "Vertical positioning fraction.
2 - Perfect center; 3 - A bit higher from the buffer center."
  :type 'integer
  :group 'letters)

(defcustom letters-quick-access-files
  '(("Social Goals" . "~/gonz/goals/social.org")
    ("Free Time"    . "~/gonz/goals/free-time.org")
    ("Finances"     . "~/gonz/goals/finances.org"))
  "Alist of files to display as quick-access dashboard shortcuts."
  :type '(alist :key-type string :value-type string)
  :group 'letters)

(defcustom letters-project-directories
  '(("Dotfiles"  . "~/.config/")
    ("Emacs"     . "~/.emacs.d/")
    ("Work"      . "~/work/")
    ("Two Sigma" . "~/gonz/goals/two-sigma/"))
  "Alist of directories for project quick-access."
  :type '(alist :key-type string :value-type string)
  :group 'letters)

(defcustom letters-button-spacing 6
  "Number of spaces between horizontal quick-access buttons.
Increased for a more breathable, minimalist aesthetic."
  :type 'integer
  :group 'letters)

;;; --- Faces & UI Styling ---

(defface letters-loaded-face
  '((t :foreground "#689d6a" :weight bold :height 0.85))
  "Face used for displaying the count of loaded packages."
  :group 'letters)

(defface letters-uninstalled-face
  '((t :foreground "#fe8019" :weight bold :height 0.85))
  "Face used for displaying the count of uninstalled packages."
  :group 'letters)

(defface letters-header-face
  '((t :foreground "#d3869b" :weight bold :height 1.2 :slant italic))
  "Face used for dashboard section titles.  Made slightly larger and italic."
  :group 'letters)

(defface letters-button-face
  '((t :foreground "#83a598" :weight semi-bold :underline nil :height 1.0))
  "Sleek modern face used for dashboard quick-access text buttons."
  :group 'letters)

(defface letters-button-mouse-face
  '((t :foreground "#ebdbb2" :background "#3c3836" :weight bold :height 1.0))
  "Face used when hovering over dashboard quick-access items."
  :group 'letters)

(defface letters-divider-face
  '((t :foreground "#3c3836" :weight light))
  "Face used for the divider line asset.  Darkened to be subtle."
  :group 'letters)

(defface letters-accent-face
  '((t :foreground "#b16286" :weight normal))
  "Face for subtle decorative accents."
  :group 'letters)

;;; --- Core Logic ---

(defun letters--center (len s)
  "Pad string S with spaces to center it within LEN characters."
  (let ((padding (max 0 (/ (- len (string-width s)) 2))))
    (concat (make-string padding ?\s) s)))

(defun letters-get-loaded-packages-count ()
  "Return the count of currently loaded packages efficiently."
  (length package-alist))

(defun letters-get-not-installed-packages-count ()
  "Return the count of available packages that are not installed."
  (- (length package-archive-contents) (length package-alist)))

(defvar letters--separate-banner
  '((letter-e . ("     ___      "
                 "    /\\  \\     "
                 "   /::\\  \\    "
                 "  /:/\\:\\  \\   "
                 " /::\\~\\:\\  \\  "
                 "/:/\\:\\ \\:\\__\\ "
                 "\\:\\~\\:\\ \\/__/ "
                 " \\:\\ \\:\\__\\   "
                 "  \\:\\ \\/__/   "
                 "   \\:\\__\\     "
                 "    \\/__/     "))
    (letter-m . ("     ___      "
                 "    /\\  \\     "
                 "   /||\\  \\    "
                 "  /|/\\|\\  \\   "
                 " /|/ /||\\  \\  "
                 "/|/ /|/\\|\\__\\ "
                 "\\/_/|/ /|/  / "
                 "  /|/ /|/  /  "
                 "  \\/_/|/  /   "
                 "    /|/  /    "
                 "    \\/__/     "))
    (letter-a . ("     ___      "
                 "    /\\  \\     "
                 "   /::\\  \\    "
                 "  /:/\\:\\  \\   "
                 " /::\\~\\:\\  \\  "
                 "/:/\\:\\ \\:\\__\\ "
                 "\\/__\\:\\/:/  / "
                 "     \\::/  /  "
                 "     /:/  /   "
                 "    /:/  /    "
                 "    \\/__/     "))
    (letter-c . ("     ___      "
                 "    /\\  \\     "
                 "   /::\\  \\    "
                 "  /:/\\:\\  \\   "
                 " /:/  \\:\\  \\  "
                 "/:/__/ \\:\\__\\ "
                 "\\:\\  \\  \\/__/ "
                 " \\:\\  \\       "
                 "  \\:\\  \\      "
                 "   \\:\\__\\     "
                 "    \\/__/     "))
    (letter-s . ("     ___      "
                 "    /\\  \\     "
                 "   /||\\  \\    "
                 "  /|/\\|\\  \\   "
                 " _\\|\\~\\|\\  \\  "
                 "/\\ \\|\\ \\|\\__\\ "
                 "\\|\\ \\|\\ \\/__/ "
                 " \\|\\ \\|\\__\\   "
                 "  \\|\\/|/  /   "
                 "   \\||/  /    "
                 "    \\/__/     ")))
  "List of individual ASCII art letter components.")

(defun letters--get-letter-color (letter)
  "Return the face style plist associated with the given LETTER."
  (cond ((eq letter 'letter-e) '(:foreground "#b16286"))
        ((eq letter 'letter-m) '(:foreground "#8f3f71"))
        ((eq letter 'letter-a) '(:foreground "#076678"))
        ((eq letter 'letter-c) '(:foreground "#98971a"))
        ((eq letter 'letter-s) '(:foreground "#8ec07c"))
        (t '(:foreground "white"))))

(defun letters--make-banner ()
  "Concatenate the stylized letters with their distinct group colors."
  (let* ((letters '(letter-e letter-m letter-a letter-c letter-s))
         (banner-lines (mapcar (lambda (line-index)
                                 (string-join
                                  (mapcar (lambda (letter)
                                            (let ((letter-string (nth line-index (cdr (assoc letter letters--separate-banner)))))
                                              (propertize letter-string 'face (letters--get-letter-color letter))))
                                          letters)
                                  ""))
                               (number-sequence 0 10))))
    banner-lines))

(defun letters--get-icon (path)
  "Fetch icon for PATH via `all-the-icons', handling directories and files cleanly."
  (if (fboundp 'all-the-icons-icon-for-file)
      (let ((expanded-path (expand-file-name path)))
        (if (or (string-suffix-p "/" path)
                (file-directory-p expanded-path))
            (concat (all-the-icons-octicon "file-directory" :v-adjust -0.05 :height 0.95 :face 'all-the-icons-blue) " ")
          (concat (all-the-icons-icon-for-file (file-name-nondirectory path) :v-adjust -0.05 :height 0.95) " ")))
    "• "))

(defun letters--insert-button-group (title items current-width)
  "Render an aesthetically integrated header and centered group of buttons."
  (when items
    (insert "\n\n")
    
    ;; 1. Integrated Header Design
    (let* ((line (make-string 12 ?─))
           (styled-line (propertize line 'face 'letters-divider-face))
           (accent (propertize " ❖ " 'face 'letters-accent-face))
           (styled-title (propertize title 'face 'letters-header-face))
           (header-str (concat styled-line accent styled-title accent styled-line)))
      (insert (letters--center current-width header-str) "\n\n"))
    
    ;; 2. Button Layout
    (let* ((spacing-str (make-string letters-button-spacing ?\s))
           (buttons-data
            (mapcar (lambda (item)
                      (let* ((label (car item))
                             (path (expand-file-name (cdr item)))
                             (icon (letters--get-icon path))
                             (display-string (concat icon label)))
                        (list :path path :label display-string :width (string-width display-string))))
                    items))
           (total-buttons-width (cl-loop for b in buttons-data sum (plist-get b :width)))
           (total-spacing-width (* letters-button-spacing (1- (length items))))
           (combined-row-width (+ total-buttons-width total-spacing-width))
           (left-margin (max 0 (/ (- current-width combined-row-width) 2))))
      
      (insert (make-string left-margin ?\s))
      (cl-loop for idx from 0
               for btn in buttons-data
               do (progn
                    (when (> idx 0) (insert spacing-str))
                    (insert-text-button (plist-get btn :label)
                                        'action (lambda (b) (find-file (button-get b 'file-path)))
                                        'file-path (plist-get btn :path)
                                        'follow-link t
                                        'help-echo (format "Open %s" (plist-get btn :path))
                                        'face 'letters-button-face
                                        'mouse-face 'letters-button-mouse-face)))
      (insert "\n"))))

(defun letters-draw-ascii-banner-fn ()
  "Clear buffer and draw the fully stylized dashboard."
  (let* ((banner (letters--make-banner))
         (longest-line (apply #'max (mapcar #'length banner)))
         (current-width (window-width))
         (banner-height (length banner))
         (padding-top (max 0 (floor (/ (- (window-height) (+ banner-height 15)) letters-top-pos))))
         (padding-string (make-string longest-line ?\s)))
    (let ((inhibit-read-only t)
          (loaded-packages (letters-get-loaded-packages-count))
          (not-installed (letters-get-not-installed-packages-count)))
      (erase-buffer)
      
      ;; 1. Top padding
      (dotimes (_ padding-top)
        (insert "\n"))
      
      ;; 2. ASCII letters
      (dolist (line banner)
        (insert (letters--center current-width
                                 (concat line (make-string (max 0 (- longest-line (length line))) ?\s))) "\n"))
      
      ;; 3. Telemetry Section
      (insert padding-string "\n")
      (insert (letters--center current-width
                               (propertize (format "Loaded Packages: %d" loaded-packages)
                                           'face 'letters-loaded-face)) "\n")
      (insert (letters--center current-width
                               (propertize (format "Not installed Packages: %d" not-installed)
                                           'face 'letters-uninstalled-face)) "\n")
      
      ;; 4. Dashboard Shortcut Groups
      (letters--insert-button-group "Active Projects" letters-project-directories current-width)
      (letters--insert-button-group "Quick Access Files" letters-quick-access-files current-width))))

;;; --- Buffer & Mode Mechanics ---

(define-derived-mode letters-mode special-mode "Letters"
  "Major mode for showing custom ASCII dashboard statistics."
  :interactive nil
  (setq-local buffer-read-only t)
  (setq-local truncate-lines t)
  (define-key letters-mode-map (kbd "g") (lambda () (interactive) (letters-draw-ascii-banner-fn)))
  (define-key letters-mode-map (kbd "n") 'forward-button)
  (define-key letters-mode-map (kbd "p") 'backward-button))

(defun letters-setup-ascii-banner ()
  "Initialize the dashboard buffer and establish window hooks."
  (let ((buffer (get-buffer-create "*Letters*")))
    (with-current-buffer buffer
      (letters-mode)
      (letters-draw-ascii-banner-fn))
    (add-hook 'window-size-change-functions #'letters--resize-handler)
    (switch-to-buffer buffer)
    buffer))

(defun letters--resize-handler (_)
  "Redraw the ASCII banner dynamically when windows switch sizes."
  (when-let ((buffer (get-buffer "*Letters*"))
             ((window-live-p (get-buffer-window buffer))))
    (with-current-buffer buffer
      (letters-draw-ascii-banner-fn))))

(provide 'letters)

;;; letters.el ends here
