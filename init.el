(require 'package)
(package-initialize)

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;; Copyright 2010, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019 Arthur Ulfeldt


(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(when (not package-archive-contents)
  (package-refresh-contents))

 (setq package-archive-priorities
      '(("melpa" . 99)
        ("marmalade" . 20)
        ("org" . 15)
        ("melpa-stable" . 0)))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))



(use-package cider
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

;; (use-package cider-spy
;;   :ensure t)

(use-package better-defaults
  :ensure t)

(use-package color-theme
  :ensure t)

(use-package paredit
  :ensure t)

(use-package idle-highlight-mode
  :ensure t)

(use-package find-file-in-project
  :ensure t)

(use-package groovy-mode
  :ensure t)

(use-package smex
  :ensure t)

(use-package ido-completing-read+
  :ensure t)

(use-package magit
  :ensure t)

(use-package dash
  :ensure t)

(use-package company
  :ensure t)

(use-package org
  :ensure t)

(use-package rainbow-delimiters
  :ensure t)

(use-package ace-jump-mode
  :ensure t)

(use-package go-mode
  :ensure t)

(use-package diminish
  :ensure t)

(use-package projectile
  :ensure t
  :config
  (diminish 'projectile-mode)
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package visual-regexp
  :ensure t)

(use-package powerline
  :ensure t)

(use-package elisp-slime-nav
  :ensure t)

(use-package zenburn-theme
  :ensure t)

(use-package rainbow-identifiers
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (diminish 'yas-minor-mode))

(use-package avy
  :ensure t
  :bind (("C-c SPC" . avy-goto-char)
         ("M-g g" . avy-goto-line))
  :config (progn (eval-after-load 'conf-mode
                   '(bind-key "C-c SPC" 'avy-goto-word-1 conf-mode-map))))

(use-package clojure-mode
  :ensure t
  :config
  (yas-global-mode 1)
  (add-hook 'before-save-hook #'delete-trailing-whitespace)
  (add-hook 'clojure-mode-hook
            (lambda ()
              (define-key clojure-mode-map (kbd "C-c SPC") 'avy-goto-word-1))))

(load-theme 'zenburn t)

(use-package magit
  :ensure t
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")
  (define-key global-map (kbd "C-x g") 'magit-status)
  (define-key global-map (kbd "C-c r") 'revert-buffer))

(use-package cider
  :ensure t
  :config
  (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook 'rainbow-identifiers-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook #'turn-on-elisp-slime-nav-mode)
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook 'eldoc-mode)
  (set-face-attribute 'cider-deprecated-face nil
                      :foreground nil
                      :background "#655")

  (set-face-attribute 'cider-test-error-face nil
                      :foreground nil
                      :background "#655")
   ;; (setq cljr-warn-on-eval nil) ;; this line breaks things horribly
  )

(use-package cider-eldoc
  :config
  (setq nrepl-hide-special-buffers t)
  (setq cider-auto-select-error-buffer t)
  (setq cider-repl-display-in-current-window nil)
  (setq cider-repl-print-length 100)
  (setq cider-prompt-for-symbol nil)
  (diminish 'eldoc-mode))


(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-idle-delay .2
        company-tooltip-flip-when-above t)
  (global-set-key (kbd "C-<tab>") 'company-manual-begin)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (diminish 'company-mode))

(use-package clj-refactor
  :ensure t
  :config
  ;; (add-hook 'cider-mode-hook
  ;;           (lambda ()
  ;;             (clj-refactor-mode 1)
  ;;             ;(cljr-add-keybindings-with-prefix "C-c C-m")
  ;;             (define-key cider-mode-map (kbd "C-c C-m") 'hydra-cljr-help-menu/body)))

  ;; ;(add-hook 'nrepl-connected-hook #'cljr-update-artifact-cache)
  ;; (define-key clojure-mode-map (kbd "C-c C-m") 'hydra-cljr-help-menu/body)
  ;; (diminish 'clj-refactor-mode)
  )



(use-package paredit
  :ensure t
  :config
  (define-key paredit-mode-map (kbd "C-M-]") 'paredit-forward-barf-sexp)
  (define-key paredit-mode-map (kbd "C-M-[") 'paredit-backward-barf-sexp)
  (define-key paredit-mode-map (kbd "M-]") 'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "M-[") 'paredit-backward-slurp-sexp)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'nrepl-mode-hook 'paredit-mode))

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

(use-package paredit-everywhere
  :ensure t
  :config (add-hook 'prog-mode-hook 'paredit-everywhere-mode))

(add-hook 'clojure-mode-hook (lambda () (set-fill-column 0)))

(use-package ace-jump-mode
  :ensure t
  :bind (("C-x SPC" . ace-jump-mode-pop-mark)
         ("C-c SPC" . ace-jump-mode))
  :config (ace-jump-mode-enable-mark-sync))

(defun yank-to-x-clipboard ()
  (interactive)
  (if (region-active-p)
        (progn
          (shell-command-on-region (region-beginning) (region-end) "xsel -i -b")
          (message "Yanked region to clipboard!")
          (deactivate-mark))
    (message "No region active; can't yank to clipboard!")))

(define-key global-map (kbd "M-W") 'yank-to-x-clipboard)

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (diminish 'flycheck-mode))

(eval-after-load 'flycheck
  '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

(defun maybe-git-auto-commit-mode ()
  (let* ((bfn (buffer-file-name))
         (root (and bfn (vc-git-root bfn)))
         (full-root (and root (expand-file-name root))))
    (if (string-equal full-root "/home/arthur/ownCloud/org/")
        (git-auto-commit-mode +1))))

(use-package git-auto-commit-mode
  :ensure t
  :config
  (setq gac-automatically-push-p nil))

(use-package org
  :ensure t
  :commands org-mode
  :config
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  (define-key org-mode-map (kbd "C-c SPC") 'ace-jump-mode)
  (setq org-agenda-files (list "~/ownCloud/org/log.org.gpg"))
  ;; make source code in org-mode files look pretty
  (setq org-src-fontify-natively t)
  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
  (add-hook 'org-mode-hook 'org-indent-home)
  (setq org-hide-leading-stars t)
  (setq org-startup-truncated nil)
  (setq org-log-done 'time)
  (setq org-tags-column (- 4 (window-width)))
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook #'maybe-git-auto-commit-mode)
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  ;; GPG key to use for encryption
  ;; Either the Key ID or set to nil to use symmetric encryption.
  (setq org-crypt-key "A20F3E34E472C3BB")
  (define-key org-mode-map (kbd "C-M-s-d") 'org-decrypt-entry))

(require 'ob-clojure)

(add-hook 'prog-mode-hook (lambda ()
                             (flyspell-prog-mode)))

(use-package powerline
  :config
  (powerline-default-theme))

(add-to-list 'load-path "~/.emacs.d/dockerfile-mode/")
(use-package dockerfile-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(defun live-delete-whitespace-except-one ()
    "Delete all whitespace around point except for 1 space,
includes the deletion of new lines."
    (interactive)
      (just-one-space -1))

(use-package frames-only-mode
  :ensure t
  :config (frames-only-mode))

(use-package haskell-mode
  :ensure t)

(use-package emojify
  ; :smiley-cat: :white-check-mark:
  :ensure t
  :config (add-hook 'after-init-hook #'global-emojify-mode))

(use-package org-bullets
  :ensure t
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package git-link
  :ensure t)

(use-package arduino-mode
  :ensure t)

(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "/path/to/elfeed.org")))

(require 'json)

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C-S-w C-S-w") 'mc/mark-all-dwim)
  (global-set-key (kbd "C-S-e C-S-e") 'mc/edit-ends-of-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(magit-commit-arguments (quote ("--gpg-sign=A20F3E34E472C3BB")))
 '(package-selected-packages
   (quote
    (elfeed-org arduino-mode groovy-mode org-crypt git-auto-commit-mode git-auto-commit git-link org-bullets emojify emacs-emojify haskell-mode
                (progn t elisp--witness--lisp)
                which-key dockerfile-mode flycheck clj-refactor markdown-mode yaml-mode rainbow-identifiers zenburn-theme ample-theme spacegray-theme soft-charcoal-theme color-theme-solarized elisp-slime-nav powerline visual-regexp projectile go-mode ace-jump-mode rainbow-delimiters company magit ido-ubiquitous smex find-file-in-project idle-highlight-mode paredit-everywhere paredit color-theme better-defaults cider-spy cider use-package)))
 '(safe-local-variable-values
   (quote
    ((cider-preferred-build-tool . "clojure-cli")
     (cider-refresh-after-fn . "connected-cooking.core/start-server")
     (cider-refresh-before-fn . "connected-cooking.core/stop-server")
     (prettier-js-args "--single-quote")
     (eval progn
           (put
            (quote defendpoint)
            (quote clojure-doc-string-elt)
            3)
           (put
            (quote api/defendpoint)
            (quote clojure-doc-string-elt)
            3)
           (put
            (quote defsetting)
            (quote clojure-doc-string-elt)
            2)
           (put
            (quote setting/defsetting)
            (quote clojure-doc-string-elt)
            2)
           (put
            (quote s/defn)
            (quote clojure-doc-string-elt)
            2)
           (define-clojure-indent
             (api-let 2)
             (assert 1)
             (assoc 1)
             (auto-parse 1)
             (catch-api-exceptions 0)
             (check 1)
             (checkp 1)
             (context 2)
             (create-database-definition 1)
             (ex-info 1)
             (execute-query 1)
             (execute-sql! 2)
             (expect 0)
             (expect-with-all-engines 0)
             (expect-with-engine 1)
             (expect-with-engines 1)
             (let-400 1)
             (let-404 1)
             (let-500 1)
             (match 1)
             (match-$ 1)
             (merge-with 1)
             (post-select 1)
             (pre-delete 1)
             (pre-insert 1)
             (pre-update 1)
             (project 1)
             (qp-expect-with-engines 1)
             (render-file 1)
             (resolve-private-vars 1)
             (select 1)
             (sync-in-context 2)
             (when-testing-engine 1)
             (with-redefs-fn 1)))))))

(provide 'init)
; init.el ends here
