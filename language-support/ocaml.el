;; Config source : https://batsov.com/articles/2022/08/23/setting-up-emacs-for-ocaml-development/
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
