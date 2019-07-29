;; -*- no-byte-compile: t; -*-

(package! doom-snippets :disable t)

(package! smart-forward :disable t)

(when IS-LINUX (package! fcitx))
(package! aggressive-indent)
(package! stickyfunc-enhance)
(package! vim-empty-lines-mode)
(when IS-LINUX
  (package! zeal-at-point))

(package! edit-server)
(package! gmail-message-mode)

(package! ido-completing-read+ :disable t)

;; cc stuff
(package! lsp-mode)
(package! lsp-ui)
(package! company-lsp)
(package! ccls)
(package! clang-format)

;; ui
(package! highlight-parentheses)

;; file format
(package! pkgbuild-mode)
(package! protobuf-mode)
(package! nginx-mode)
