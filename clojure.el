
;; swank-clojure
;(add-to-list 'load-path "~/opt/swank-clojure/src/emacs")

;(setq swank-clojure-jar-path "~/.clojure/clojure.jar"
;      swank-clojure-extra-classpaths (list
;				      "~/opt/swank-clojure/src/main/clojure"
;				      "~/.clojure/clojure-contrib.jar"))

;(require 'swank-clojure-autoload)

;; slime
;(eval-after-load "slime"
;  '(progn (slime-setup '(slime-repl))))

;(add-to-list 'load-path "~/opt/slime")
;(require 'slime)
;(slime-setup)


;; clojure-mode
(add-to-list 'load-path "~/.emacs.d/clojure-mode")
(require 'clojure-mode)
(require 'clojurescript-mode)


;; swank-clojure
;(add-to-list 'load-path "~/opt/swank-clojure/src/emacs")

;(setq swank-clojure-jar-path "~/.clojure/clojure.jar"
;      swank-clojure-extra-classpaths (list
;				      "~/opt/swank-clojure/src/main/clojure"
;				      "~/.clojure/clojure-contrib.jar"))

;(require 'swank-clojure-autoload)

;; slime
;(eval-after-load "slime"
;  '(progn (slime-setup '(slime-repl))))

(add-to-list 'load-path "~/opt/slime")
(require 'slime)
(slime-setup)

(provide 'clojure)
