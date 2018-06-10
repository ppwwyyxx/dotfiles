;; -*- no-byte-compile: t; -*-

(package! smart-forward :disable t)

(when IS-LINUX (package! fcitx))
(package! aggressive-indent)
(package! stickyfunc-enhance)
(package! vim-empty-lines-mode)
(when IS-LINUX
  (package! zeal-at-point))

(package! edit-server)
(package! gmail-message-mode)

;; disable irony-related stuff
(package! irony :disable t)
(package! company-irony :disable t)
(package! company-irony-c-headers :disable t)
(package! flycheck-irony :disable t)
(package! irony-eldoc :disable t)
(package! rtags :disable t)
(package! ivy-rtags :disable t)
(package! ido-completing-read+ :disable t)

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

;; file format
(package! pkgbuild-mode)
(package! protobuf-mode)
(package! nginx-mode)
