;;; org-excalidraw.el --- Tools for working with excalidraw drawings -*- lexical-binding: t; -*-
;; Copyright (C) 2022 David Wilson

;; Author:  David Wilson <wdavew@gmail.com>
;; URL: https://github.com/wdavew/org-excalidraw
;; Created: 2022
;; Version: 0.1.3-patched
;; Keywords: convenience, outlines
;; Package-Requires: ((org "9.3") (emacs "26.1"))

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;;; org-excalidraw.el is a package to for embedding excalidraw drawings into Emacs.
;;; it adds an org-mode link type for excalidraw files to support inline display
;;; and opening the diagrams from Emacs for editing.

;;; Code:
(require 'cl-lib)
(require 'filenotify)
(require 'org-id)
(require 'ol)

(defun org-excalidraw--default-base ()
  "Get default JSON template used for new excalidraw files."
  "{
    \"type\": \"excalidraw\",
    \"version\": 2,
    \"source\": \"https://excalidraw.com\",
    \"elements\": [],
    \"appState\": {
      \"gridSize\": null,
      \"viewBackgroundColor\": \"#ffffff\"
    },
    \"files\": {}
  }
")

(defgroup org-excalidraw nil
  "Customization options for org-excalidraw."
  :group 'org
  :prefix "org-excalidraw-")

(defcustom org-excalidraw-directory "~/org-excalidraw"
  "Directory to store excalidraw files."
  :type 'string
  :group 'org-excalidraw)

(defcustom org-excalidraw-base (org-excalidraw--default-base)
  "JSON string representing base excalidraw template for new files."
  :type 'string
  :group 'org-excalidraw)

(defun org-excalidraw--validate-excalidraw-file (path)
  "Validate the excalidraw file at PATH is usable."
  (unless (string-suffix-p ".excalidraw" path)
    (error
     "Excalidraw file must have .excalidraw extension")))

(defun org-excalidraw--shell-cmd-to-svg (path)
  "Construct shell cmd for converting excalidraw file with PATH to svg."
  (concat "excalidraw_export --rename_fonts=true " (format "\"%s\"" path)))

(defun org-excalidraw--shell-cmd-open (path os-type)
  "Construct shell cmd to open excalidraw file with PATH for OS-TYPE."
  (if (eq os-type 'darwin)
      (concat "open " (shell-quote-argument path))
    (concat "xdg-open " (shell-quote-argument path))))

(defun org-excalidraw--open-file-from-svg (path)
  "Open corresponding .excalidraw file for svg located at PATH."
  (let ((excal-file-path (string-remove-suffix ".svg" path)))
    (org-excalidraw--validate-excalidraw-file excal-file-path)
    (shell-command (org-excalidraw--shell-cmd-open excal-file-path system-type))))

(defun org-excalidraw--handle-file-change (event)
  "Handle file update EVENT to convert files to svg."
  (let* ((action (cadr event))
         ;; 'renamed events put the new filename in the 4th position (cadddr).
         ;; 'changed/'created events put the filename in the 3rd position (caddr).
         (filename (if (eq action 'renamed) (cadddr event) (caddr event))))
    (when (and filename 
               (string-suffix-p ".excalidraw" filename)
               (memq action '(changed created renamed)))
      (shell-command (org-excalidraw--shell-cmd-to-svg filename))
      ;; NEW: Auto-refresh images in all active org buffers so they live-update
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (derived-mode-p 'org-mode)
            (org-display-inline-images nil t)))))))

;;;###autoload
(defun org-excalidraw-create-drawing ()
  "Create an excalidraw drawing and insert an 'org-mode' link to it at Point."
  (interactive)
  (let* ((filename (format "%s.excalidraw" (org-id-uuid)))
         (path (expand-file-name filename org-excalidraw-directory))
         (link (format "[[file:%s.svg]]" path)))
    (org-excalidraw--validate-excalidraw-file path)
    (insert link)
    (with-temp-file path (insert org-excalidraw-base))
    
    ;; NEW: Force the SVG to generate immediately so Emacs has something to render
    (shell-command (org-excalidraw--shell-cmd-to-svg path))
    ;; NEW: Automatically render inline images in the current buffer
    (org-display-inline-images nil t)
    
    (shell-command (org-excalidraw--shell-cmd-open path system-type))))

;;;###autoload
(defun org-excalidraw-delete-drawing-at-point ()
  "Delete the excalidraw drawing link at point and its associated files on disk."
  (interactive)
  (let* ((context (org-element-context))
         (type (org-element-type context)))
    (unless (eq type 'link)
      (user-error "No link at point"))
    (let ((path (org-element-property :path context)))
      (unless (and path (string-match-p "\\.excalidraw\\.svg\\'" path))
        (user-error "Not on a valid excalidraw file link"))
      (let* ((svg-file (expand-file-name path))
             (excalidraw-file (string-remove-suffix ".svg" svg-file))
             (begin (org-element-property :begin context))
             (end (org-element-property :end context)))
        (if (y-or-n-p "Delete this Excalidraw drawing and its files permanently? ")
            (progn
              (when (file-exists-p svg-file) (delete-file svg-file))
              (when (file-exists-p excalidraw-file) (delete-file excalidraw-file))
              (delete-region begin end)
              (message "Excalidraw drawing deleted."))
          (message "Aborted."))))))

;;;###autoload
(defun org-excalidraw-initialize ()
  "Setup excalidraw.el. Call this after 'org-mode initialization."
  (interactive)
  (unless (file-directory-p org-excalidraw-directory)
    (error
     "Excalidraw directory %s does not exist"
     org-excalidraw-directory))
  (file-notify-add-watch org-excalidraw-directory '(change) 'org-excalidraw--handle-file-change)
  
  ;; Hijack file: links ending in .excalidraw.svg to open via our app
  (add-to-list 'org-file-apps 
               '("\\.excalidraw\\.svg\\'" . (lambda (file link) (org-excalidraw--open-file-from-svg file))))

  ;; Keep the old custom link registered just in case you have old links lying around
  (org-link-set-parameters "excalidraw"
                           :follow 'org-excalidraw--open-file-from-svg
                           :image-data-fun (lambda (_protocol link _desc)
                                             (when (file-exists-p link)
                                               (with-temp-buffer (insert-file-contents-literally link)
                                                                 (buffer-substring-no-properties
                                                                  (point-min)
                                                                  (point-max)))))))

(provide 'org-excalidraw)
;;; org-excalidraw.el ends here
