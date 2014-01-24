(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-bindings
                      clojure-mode cider auto-complete ac-nrepl
                      org rainbow-delimiters auto-complete cider)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p))) 

(require 'ac-nrepl)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))

(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-interaction-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-mode))

(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook 'nrepl-turn-on-eldoc-mode)
(setq nrepl-hide-special-buffers t)
(setq cider-auto-select-error-buffer t)
(setq cider-repl-display-in-current-window t)
(setq cider-repl-print-length 100)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

(eval-after-load "cider"
    '(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))

(add-hook 'prog-mode-hook 'auto-complete-mode)

(add-hook 'nrepl-mode-hook 'paredit-mode)
(eval-after-load 'paredit '(define-key paredit-mode-map (kbd "C-M-]") 'paredit-forward-barf-sexp))
(eval-after-load 'paredit '(define-key paredit-mode-map (kbd "C-M-[") 'paredit-backward-barf-sexp))
(eval-after-load 'paredit '(define-key paredit-mode-map (kbd "M-]") 'paredit-forward-slurp-sexp))
(eval-after-load 'paredit '(define-key paredit-mode-map (kbd "M-[") 'paredit-backward-slurp-sexp))

(setq x-select-enable-clipboard t)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((epa-file-encrypt-to arthur@ulfeldt\.com) (whitespace-line-column . 80) (lexical-binding . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "color-234" :foreground "unspecified-fg" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default")))))
