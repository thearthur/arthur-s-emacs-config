
(add-to-list 'load-path "~/.emacs.d/clojure-mode")
(require 'clojure-mode)
(require 'clojurescript-mode)

(add-to-list 'load-path "~/.emacs.d/slime")
(require 'slime)
(slime-setup)

(provide 'clojure)

(load-file "rainbow-delimiters.el")
(require 'rainbow-delimiters)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
