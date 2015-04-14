(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)


(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(better-defaults
		      paredit paredit-everywhere
                      idle-highlight-mode find-file-in-project smex ido-ubiquitous magit
                      clojure-mode dash cider company
                      org rainbow-delimiters ace-jump-mode go-mode
                      projectile visual-regexp tuareg clj-refactor cider-spy
                      powerline elisp-slime-nav
                      color-theme-solarized soft-charcoal-theme spacegray-theme ample-theme zenburn-theme
                      rainbow-identifiers yaml-mode markdown-mode
                      flycheck-clojure flycheck-pos-tip)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p))) 

(load-theme 'zenburn t)

(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook #'turn-on-elisp-slime-nav-mode)

(define-key global-map (kbd "C-x g") 'magit-status)
(define-key global-map (kbd "C-c r") 'revert-buffer)

(require 'cider)
(require 'cider-eldoc)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'cider-turn-on-eldoc-mode)
(setq nrepl-hide-special-buffers t)
(setq cider-auto-select-error-buffer t)
(setq cider-repl-display-in-current-window nil)
(setq cider-repl-print-length 100)
(setq cider-prompt-for-symbol nil)
(require 'company)
(global-company-mode)
(setq company-idle-delay .2
      company-tooltip-flip-when-above t)
(global-set-key (kbd "C-<tab>") 'company-manual-begin)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)

(require 'clj-refactor)
(add-hook 'clojure-mode-hook
          (lambda ()
            (clj-refactor-mode 1)
            (cljr-add-keybindings-with-prefix "C-c C-m")))
(add-hook 'nrepl-connected-hook #'cljr-update-artifact-cache)

(require 'paredit)
(define-key paredit-mode-map (kbd "C-M-]") 'paredit-forward-barf-sexp)
(define-key paredit-mode-map (kbd "C-M-[") 'paredit-backward-barf-sexp)
(define-key paredit-mode-map (kbd "M-]") 'paredit-forward-slurp-sexp)
(define-key paredit-mode-map (kbd "M-[") 'paredit-backward-slurp-sexp)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'nrepl-mode-hook 'paredit-mode)

;; Append result of evaluating previous expression (Clojure):
(defun cider-eval-last-sexp-and-append ()
  "Evaluate the expression preceding point and append result."
  (interactive)
  (let ((last-sexp (cider-last-sexp)))
    ;; we have to be sure the evaluation won't result in an error
    (cider-eval-and-get-value last-sexp)
    (with-current-buffer (current-buffer)
      (insert ";;=>")
      (push-mark)
      (cider-interactive-eval-print last-sexp)
      (comment-region (mark) (point) -1)
      (pop-mark))))

(define-key clojure-mode-map (kbd "C-x y") 'cider-eval-last-sexp-and-append)

(require 'paredit-everywhere)
(add-hook 'prog-mode-hook 'paredit-everywhere-mode)

(add-hook 'clojure-mode-hook (lambda () (set-fill-column 0)))

(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)

(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(setq x-select-enable-clipboard t)
(defun yank-to-x-clipboard ()
  (interactive)
  (if (region-active-p)
        (progn
          (shell-command-on-region (region-beginning) (region-end) "xsel -i -b")
          (message "Yanked region to clipboard!")
          (deactivate-mark))
    (message "No region active; can't yank to clipboard!")))

(define-key global-map (kbd "M-W") 'yank-to-x-clipboard)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)

(eval-after-load 'flycheck '(flycheck-clojure-setup))
(add-hook 'after-init-hook #'global-flycheck-mode)

(eval-after-load 'flycheck
  '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

;; if all children of a TODO are done, then change status of TODO to
;; DONE
;; make clojure code blocks work in org mode
(require 'org)
(require 'ob-clojure)
(define-key org-mode-map (kbd "C-c SPC") 'ace-jump-mode)
(setq org-agenda-files (list "~/ownCloud/org/log.org.gpg"))
;; make source code in org-mode files look pretty
(setq org-src-fontify-natively t)

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
(add-hook 'org-mode-hook 'org-indent-home)
(setq org-hide-leading-stars t)
(setq org-startup-truncated nil)
(setq org-log-done 'time)
(setq org-tags-column (- 4 (window-width)))
(add-hook 'org-mode-hook 'flyspell-mode)

(projectile-global-mode)

(require 'powerline)
(powerline-default-theme)

(add-to-list 'load-path "~/.emacs.d/dockerfile-mode/")
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "color-233" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((docker-image-name . "yummly/api") (epa-file-encrypt-to arthur@ulfeldt\.com) (whitespace-line-column . 80) (lexical-binding . t)))))
