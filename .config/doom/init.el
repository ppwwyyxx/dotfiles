(disable-packages!
  company-irony company-irony-c-headers flycheck-irony irony irony-eldoc
  smart-forward
  ivy-rtags rtags
  evil-embrace
  )

(def-package-hook! evil-snipe
  :post-init
  (setq evil-snipe-override-evil-repeat-keys nil)   ; don't override ; and ,
  :pre-config
  nil
  )
(def-package-hook! evil-collection
  :pre-config
  (delq 'diff-mode evil-collection-mode-list)  ; breaks too much
  t
  )

(setq debug-on-error nil)

(doom! :feature
       (evil +everywhere); come to the dark side, we have cookies
       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
       debugger          ; FIXME stepping through code, to help you add bugs
       eval              ; run code, run (also, repls)
       file-templates    ; auto-snippets for empty files
       lookup
       ;(lookup           ; helps you navigate your code and documentation
       ; +devdocs         ; ...on devdocs.io online
       ; +docsets)        ; ...or in Dash docsets locally
       services          ; TODO managing external services & code builders
       snippets          ; my elves. They type so I don't have to
       spellcheck        ; tasing you for misspelling mispelling
       (syntax-checker
         +childframe)    ; tasing you for every semicolon you forget
       version-control   ; remember, remember that commit in November
       workspaces        ; tab emulation, persistence & separate workspaces

       :completion
       (company
        +auto +childframe)     ; the ultimate code completion backend
       (ivy +childframe) ; a search engine for love and life

       :ui
       doom              ; what makes DOOM look the way it does
       doom-modeline     ; a snazzy Atom-inspired mode-line
       ;doom-quit         ; DOOM quit-message prompts when you quit Emacs
       evil-goggles      ; display visual hints when editing in evil
       hl-todo           ; highlight TODO/FIXME/NOTE tags
       nav-flash         ; blink the current line after jumping
      ;tabbar            ; FIXME an (incomplete) tab bar for Emacs
       unicode           ; extended unicode support for various languages
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows

       :tools
       dired             ; making dired pretty [functional]
       editorconfig
       ein               ; tame Jupyter notebooks with emacs
       electric-indent   ; smarter, keyword-based electric-indent
       eshell            ; a consistent, cross-platform shell (WIP)
       gist              ; interacting with github gists
       imenu             ; an imenu sidebar and searchable code index
       impatient-mode    ; show off code over HTTP
      ;macos             ; MacOS-specific commands
       make              ; run make tasks from Emacs
       magit             ;
       neotree           ; a project drawer, like NERDTree for vim
       password-store    ; password manager for nerds
       pdf               ; pdf enhancements
      ;rgb               ; creating color strings
       rotate-text       ; cycle region at point between text candidates
       term              ; terminals in Emacs
       tmux              ; an API for interacting with tmux
       upload            ; map local to remote projects via ssh/ftp

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
       python            ; beautiful is better than ugly
       ruby              ; 1.step do {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       sh                ; she sells (ba|z)sh shells on the C xor
       typescript        ; javascript, but better
       web               ; the tubes

       ;; Applications are complex and opinionated modules that transform Emacs
       ;; toward a specific purpose. They may have additional dependencies and
       ;; should be loaded late.
       :app
      ;(email +gmail)    ; emacs as an email client
      ;irc               ; how neckbeards socialize
      ;(rss +org)        ; emacs as an RSS reader
      ;twitter           ; twitter client https://twitter.com/vnought
      ;(write            ; emacs as a word processor (latex + org + markdown)
      ; +wordnut         ; wordnet (wn) search
      ; +langtool)       ; a proofreader (grammar/style check) for Emacs

       :config
       (default
         +evil-commands)
    )
