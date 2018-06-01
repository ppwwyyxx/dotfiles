;; -*- no-byte-compile: t; -*-

(package! smart-forward :disable t)

(when IS-LINUX (package! fcitx))
(package! aggressive-indent)
(package! stickyfunc-enhance)

;; disable irony-related stuff
(package! irony :disable t)
(package! company-irony :disable t)
(package! company-irony-c-headers :disable t)
(package! flycheck-irony :disable t)
(package! irony-eldoc :disable t)
(package! rtags :disable t)
(package! ivy-rtags :disable t)

;; cc stuff
(package! lsp-mode)
(package! lsp-ui)
(package! company-lsp)
(package! ccls)
(package! clang-format)

;; ui
(package! highlight-indentation :disable t)
(package! highlight-indent-guides)
(package! highlight-parentheses)

