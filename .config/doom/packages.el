;; -*- no-byte-compile: t; -*-
(package! irony :disable t)
(package! company-irony :disable t)
(package! company-irony-c-headers :disable t)
(package! flycheck-irony :disable t)
(package! irony-eldoc :disable t)
(package! rtags :disable t)
(package! ivy-rtags :disable t)
(package! smart-forward :disable t)
(package! highlight-indentation :disable t)

(package! highlight-indent-guides)
(package! highlight-parentheses)

(package! lsp-mode)
(package! lsp-ui)
(package! company-lsp)
(package! ccls)

(when IS-LINUX (package! fcitx))
