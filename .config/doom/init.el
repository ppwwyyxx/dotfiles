;; -*- no-byte-compile: t; -*-

;; Environment:
(add-to-list 'load-path (concat doom-private-dir "vendor/"))
(add-to-list 'exec-path (substitute-in-file-name "$HOME/bin"))
(add-to-list 'exec-path (substitute-in-file-name "$HOME/.local/bin"))

;; fix some packages
(use-package-hook! evil-snipe
  :post-init
  (setq evil-snipe-override-evil-repeat-keys nil)   ; don't override ; and ,
  :pre-config
  nil)

(use-package-hook! evil-goggles
  :post-init
  (setq evil-goggles-enable-delete t)
  (setq evil-goggles-enable-change t))

(use-package-hook! ivy
  :pre-config
  (setq ivy-do-completion-in-region nil)  ;; don't use it for evil-ex completion
  t)

(setq +evil-collection-disabled-list
      '(simple diff-mode anaconda-mode ivy dired minibuffer))

;;(setq debug-on-error t)

(doom! :completion
       company           ; the ultimate code completion backend
       (ivy              ; a search engine for love and life
        +childframe)

       :ui
       doom              ; what makes DOOM look the way it does
       modeline          ; a snazzy Atom-inspired mode-line
       ophints           ; display visual hints when editing in evil
       indent-guides
       fill-column
       hl-todo           ; highlight TODO/FIXME/NOTE tags
       nav-flash         ; blink the current line after jumping
       neotree           ; a project drawer, like NERDTree for vim
       ;;treemacs
       pretty-code
       ;;tabbar            ; FIXME an (incomplete) tab bar for Emacs
       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
       unicode           ; extended unicode support for various languages
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       vc-gutter
       window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold
       multiple-cursors
       rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       ;;parinfer

       :emacs
       (dired            ; making dired pretty [functional]
        +icons +ranger)
       electric          ; smarter, keyword-based electric-indent
       vc

       :term
       eshell            ; a consistent, cross-platform shell (WIP)
       term              ; terminals in Emacs

       :tools
       debugger          ; FIXME stepping through code, to help you add bugs
       eval              ; run code, run (also, repls)
       editorconfig
       (lookup +docsets)
       ;;ein               ; tame Jupyter notebooks with emacs
       ;;gist              ; interacting with github gists
       ;;macos             ; MacOS-specific commands
       make              ; run make tasks from Emacs
       magit             ;
       pass              ; password manager for nerds
       prodigy
       pdf               ; pdf enhancements
       rgb               ; creating color strings
       tmux              ; an API for interacting with tmux
       ;;upload            ; map local to remote projects via ssh/ftp
       docker

       :checkers
       spell
       syntax

       :lang
       assembly          ; assembly for fun or debugging
       cc                ; C/C++/Obj-C madness
       data              ; config/data formats
       emacs-lisp        ; drown in parentheses
       ess               ; emacs speaks statistics
       go                ; the hipster dialect
       (haskell +intero) ; a language that's lazier than I am
       (java +meghanada) ; the poster child for carpal tunnel syndrome
       javascript        ; all(hope(abandon(ye(who(enter(here))))))
       latex             ; writing papers in Emacs has never been so fun
       lua               ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
       (org              ; organize your plain life in plain text
        +attach          ; custom attachment system
        +babel           ; running code in org
        +capture         ; org-capture in and outside of Emacs
        +export          ; Exporting org to whatever you want
        +present         ; Emacs for presentations
        +publish)        ; Emacs+Org as a static site generator
       (python
        +conda
        +ipython)        ; beautiful is better than ugly
       ruby              ; 1.step do {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       sh                ; she sells (ba|z)sh shells on the C xor
       web               ; the tubes

       ;; Applications are complex and opinionated modules that transform Emacs
       ;; toward a specific purpose. They may have additional dependencies and
       ;; should be loaded late.
       :collab
       :app
       ;;(email +gmail)    ; emacs as an email client
       ;;irc               ; how neckbeards socialize
       ;;(rss +org)        ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought
       ;;(write            ; emacs as a word processor (latex + org + markdown)
       ;; +wordnut         ; wordnet (wn) search
       ;; +langtool)       ; a proofreader (grammar/style check) for Emacs

       :config
       default
       )
