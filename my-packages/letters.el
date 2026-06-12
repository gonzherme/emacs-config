;;; letters.el --- Minimal ASCII banner engine for Zone -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(defgroup letters nil
  "Customizable minimalist ASCII banner."
  :group 'applications)

(defcustom letters-top-pos 3
  "Vertical positioning fraction. 2 for center, 3 for higher."
  :type 'integer
  :group 'letters)

(defvar letters-banner
  '((e . ("     ___      "
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
    (m . ("     ___      "
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
    (a . ("     ___      "
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
    (c . ("     ___      "
          "    /\\  \\     "
          "   /::\\  \\    "
          "  /|/\\:\\  \\   "
          " /:/  \\:\\  \\  "
          "/:/__/ \\:\\__\\ "
          "\\:\\  \\  \\/__/ "
          " \\:\\  \\       "
          "  \\:\\  \\      "
          "   \\:\\__\\     "
          "    \\/__/     "))
    (s . ("     ___      "
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
  "The raw multi-colored ASCII letters asset matrix.")

(defun letters--get-color (char-sym)
  "Return theme hex colors for each ASCII component."
  (cond ((eq char-sym 'e) '(:foreground "#b16286"))
        ((eq char-sym 'm) '(:foreground "#8f3f71"))
        ((eq char-sym 'a) '(:foreground "#076678"))
        ((eq char-sym 'c) '(:foreground "#98971a"))
        ((eq char-sym 's) '(:foreground "#8ec07c"))
        (t '(:foreground "white"))))

(defun letters--build-banner ()
  "Assemble lines with proper text properties applied."
  (let ((letters '(e m a c s)))
    (mapcar (lambda (line-idx)
              (string-join
               (mapcar (lambda (l)
                         (let ((str (nth line-idx (cdr (assoc l letters-banner)))))
                           (propertize str 'face (letters--get-color l))))
                       letters)
               ""))
            (number-sequence 0 10))))

;;;###autoload
(defun zone-pgm-letters-minimal ()
  "Clean screensaver loop operating directly on zone's working buffer with a silent echo area."
  (let ((inhibit-read-only t)
        ;; SILENCE THE MINIBUFFER NOTIFICATIONS
        (inhibit-message t) 
        (banner (letters--build-banner))
        (w-width (window-body-width))
        (w-height (window-body-height))
        (tab-bar-was-on (and (boundp 'tab-bar-mode) tab-bar-mode))
        (centaur-tabs-was-on (and (boundp 'centaur-tabs-mode) centaur-tabs-mode)))
    
    (unwind-protect
        (progn
          (when (fboundp 'tab-bar-mode) (tab-bar-mode -1))
          (when (fboundp 'centaur-tabs-mode) (centaur-tabs-mode -1))
          
          (erase-buffer)
          (setq-local cursor-type nil)
          (setq-local tab-line-format nil)
          (setq-local header-line-format nil)
          (setq-local mode-line-format nil)
          
          (when (fboundp 'display-line-numbers-mode)
            (display-line-numbers-mode -1))
          
          (let* ((longest-line (apply #'max (mapcar #'string-width banner)))
                 (banner-height (length banner))
                 (padding-top (max 0 (floor (/ (- w-height banner-height) letters-top-pos)))))
            
            (dotimes (_ padding-top) (insert "\n"))
            
            (dolist (line banner)
              (let ((padding-left (max 0 (/ (- w-width longest-line) 2))))
                (insert (make-string padding-left ?\s) line "\n")))
            
            ;; FORCE CLEAR any lingering text in the bottom echo area
            (message nil) 
            
            (while (not (input-pending-p))
              (sit-for 1))))
      
      (when (and tab-bar-was-on (fboundp 'tab-bar-mode))
        (tab-bar-mode 1))
      (when (and centaur-tabs-was-on (fboundp 'centaur-tabs-mode))
        (centaur-tabs-mode 1)))))


(provide 'letters)
;;; letters.el ends here
