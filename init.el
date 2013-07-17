(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-bindings clojure-mode
                      nrepl auto-complete ac-nrepl org rainbow-delimiters)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p))) 

(require 'ac-nrepl)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
     '(add-to-list 'ac-modes 'nrepl-mode))
(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))

(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

(add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)
(define-key nrepl-interaction-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)
(add-hook 'prog-mode-hook 'auto-complete-mode)

(add-hook 'nrepl-interaction-mode-hook
            'nrepl-turn-on-eldoc-mode)

(add-hook 'nrepl-mode-hook 'paredit-mode)
(eval-after-load 'paredit '(define-key paredit-mode-map (kbd "C-M-]") 'paredit-forward-barf-sexp))
(eval-after-load 'paredit '(define-key paredit-mode-map (kbd "C-M-[") 'paredit-backward-barf-sexp))
(eval-after-load 'paredit '(define-key paredit-mode-map (kbd "M-]") 'paredit-forward-slurp-sexp))
(eval-after-load 'paredit '(define-key paredit-mode-map (kbd "M-[") 'paredit-backward-slurp-sexp))

(setq org-directory "~/Dropbox/org/")
(setq org-mobile-directory "~/Dropbox/org/")
(setq org-mobile-inbox-for-pull "~/Dropbox/org/log.org")
(setq org-agenda-files '("~/Dropbox/org/log.org"))

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
