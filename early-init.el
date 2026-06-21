;; early-init.el

;; Garbage Collection Hack (Remember to reset in init.el!)
(setq gc-cons-threshold most-positive-fixnum)

;; pushing loads these rules into memory before a single pixel is drawn on
;; screen
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)

(menu-bar-mode -1) ;; no menubar
(tool-bar-mode -1) ;; no toolbars
(scroll-bar-mode -1) ;; no scroll bars

;; Prevent the frame from resizing itself while booting up
(setq frame-inhibit-implied-resize t)

;; disable the startup screen so it doesn't flash
(setq inhibit-startup-screen t)
