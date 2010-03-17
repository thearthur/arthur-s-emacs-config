;; clojure-mode
(add-to-list 'load-path "~/opt/clojure-mode")
(require 'clojure-mode)

;; disable bell function
(setq ring-bell-function 'ignore)

;; disable toolbar
(tool-bar-mode -1)

;; disable scrollbar
(toggle-scroll-bar -1)

;; do away with annoying clickey things
(menu-bar-mode -1)

;; disable splash screen
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(show-paren-mode t)
 '(transient-mark-mode nil))

;; current buffer name in title bar
(setq frame-title-format "%b")

(column-number-mode)

;; clojure-mode
(add-to-list 'load-path "~/opt/clojure-mode")
(require 'clojure-mode)

;; swank-clojure
(add-to-list 'load-path "~/opt/swank-clojure/src/emacs")

(setq swank-clojure-jar-path "~/.clojure/clojure.jar"
      swank-clojure-extra-classpaths (list
				      "~/opt/swank-clojure/src/main/clojure"
				      "~/.clojure/clojure-contrib.jar"))

(require 'swank-clojure-autoload)

;; slime
(eval-after-load "slime"
  '(progn (slime-setup '(slime-repl))))

(add-to-list 'load-path "~/opt/slime")
(require 'slime)
(slime-setup)

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
		       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key [f11] 'fullscreen)

(set-fringe-style 0)