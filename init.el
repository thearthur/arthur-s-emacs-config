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
                      clojure-mode dash cider auto-complete ac-nrepl
                      org rainbow-delimiters auto-complete ace-jump-mode go-mode
                      projectile visual-regexp tuareg)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p))) 

(require 'ac-nrepl)

(eval-after-load "auto-complete"
  '(progn
     (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
     (add-to-list 'ac-modes 'cider-mode)
     (defun set-auto-complete-as-completion-at-point-function ()
       (setq completion-at-point-functions '(auto-complete)))
     (add-hook 'prog-mode-hook 'auto-complete-mode)))

(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

(define-key global-map (kbd "C-x g") 'magit-status)

(require 'cider)
(require 'cider-eldoc)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-interaction-mode-hook 'ac-nrepl-setup)
(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)
(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'cider-turn-on-eldoc-mode)
(setq nrepl-hide-special-buffers t)
(setq cider-auto-select-error-buffer t)
(setq cider-repl-display-in-current-window nil)
(setq cider-repl-print-length 100)




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

;; make source code in org-mode files look pretty
(setq org-src-fontify-natively t)
;; if all children of a TODO are done, then change status of TODO to
;; DONE
;; make clojure code blocks work in org mode
(require 'org)
(require 'ob-clojure)

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
(add-hook 'org-mode-hook 'org-indent-home)
(setq org-hide-leading-stars t)
(setq org-startup-truncated nil)
(add-hook 'org-mode-hook 'flyspell-mode)

(projectile-global-mode)

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
