;;;  -*- lexical-binding: t; -*-


(after! evil-mc
  (global-evil-mc-mode 0)
  (setq evil-mc-key-map (make-sparse-keymap))  ; don't pollute keys
)

;; expand-region's prompt can't tell what key contract-region is bound to, so we tell it explicitly.
(setq expand-region-contract-fast-key "V")

(map! [remap newline]          #'newline-and-indent
      ;; Ensure there are no conflicts
      :nmvo doom-leader-key nil
      :nmvo doom-localleader-key nil
      (:map special-mode-map
        :nmvo doom-leader-key nil
        )
      (:after dired :map dired-mode-map
        :nmvo doom-leader-key nil
        )

      ;; --- Global keybindings ---------------------------
      ;; clean-ups:
      :gnvime "M-l" nil
      :gnvime "M-h" nil

      (:map input-decode-map
        [S-iso-lefttab] [backtab]
        (:unless window-system "TAB" [tab])) ; Fix TAB in terminal


      ;; Make M-x available everywhere
      :gnvime "M-x" #'execute-extended-command
      ;; :gnvime "M-:" #'doom/open-scratch-buffer
      :gnvime "C-x C-b" #'ibuffer

      :ne "M-r"   #'+eval/buffer
      :ne "M-f"   #'swiper
      ;;:n  "M-s"   #'save-buffer

      :nm  ";"     #'evil-ex
      (:after dired :map dired-mode-map :nm  ";"     #'evil-ex)

      :nv [tab]   #'+evil/matchit-or-toggle-fold

      ;; :nv "K"  #'+lookup/documentation  ; TODO not cool
      :m  "gd" #'+lookup/definition
      :m  "gD" #'+lookup/references
      ;;:m  "gs" #'+default/easymotion  ; lazy-load `evil-easymotion'
      :n  "gx"  #'evil-exchange  ; https://github.com/tommcdo/vim-exchange

      :nv "C-a"   #'evil-numbers/inc-at-pt
      :nv "C-S-a" #'evil-numbers/dec-at-pt

      ;; editing commands
      :i "C-S-V" #'evil-paste-after
      :i "C-a"   #'doom/backward-to-bol-or-indent
      :i "C-e"   #'doom/forward-to-last-non-comment-or-eol
      :i "C-u"   #'doom/backward-kill-to-bol-and-indent
      :i "C-b"   #'backward-word
      :i "C-f"   #'forward-word
      (:map (minibuffer-local-map
             minibuffer-local-ns-map
             minibuffer-local-completion-map
             minibuffer-local-must-match-map
             minibuffer-local-isearch-map
             read-expression-map)
        [escape] #'abort-recursive-edit
        "C-a"    #'move-beginning-of-line
        "C-w"    #'backward-kill-word
        "C-u"    #'backward-kill-sentence
        "C-b"    #'backward-word
        "C-f"    #'forward-word
        )
      (:after evil :map evil-ex-completion-map
        "C-a"   #'move-beginning-of-line
        "C-b"   #'backward-word
        "C-f"   #'forward-word
        "C-S-V" #'evil-paste-after
        )

      ;; expand-region
      :v  "v"  #'er/expand-region
      :v  "V"  #'er/contract-region

      ; workspace/tab related
      :me "M-t"       #'+workspace/new
      :me "M-T"       #'+workspace/display
      :me "M-1"       (λ! (+workspace/switch-to 0))
      :me "M-2"       (λ! (+workspace/switch-to 1))
      :me "M-3"       (λ! (+workspace/switch-to 2))
      :me "M-4"       (λ! (+workspace/switch-to 3))
      :me "M-5"       (λ! (+workspace/switch-to 4))
      :me "M-6"       (λ! (+workspace/switch-to 5))
      :me "M-7"       (λ! (+workspace/switch-to 6))
      :me "M-8"       (λ! (+workspace/switch-to 7))
      :me "M-9"       (λ! (+workspace/switch-to 8))
      :me "M-0"       #'+workspace/switch-to-last
      ; window management
      :me "C-h"   #'evil-window-left
      :me "C-j"   #'evil-window-down
      :me "C-k"   #'evil-window-up
      :me "C-l"   #'evil-window-right

      :m  "]b" #'next-buffer
      :m  "[b" #'previous-buffer
      ; TODO if under GUI, use alt-hl
      :m  "]w" #'+workspace/switch-right
      :m  "[w" #'+workspace/switch-left
      :m  "]e" #'next-error
      :m  "[e" #'previous-error
      :m  "]d" #'git-gutter:next-hunk
      :m  "[d" #'git-gutter:previous-hunk

      (:leader
        ;; :desc "M-x"                     :nv ":"  #'execute-extended-command
        ; jumps:
        :desc "Find file in project"    :n "SPC" #'projectile-find-file
        :desc "Switch workspace buffer" :n ","   #'persp-switch-to-buffer
        :desc "Switch buffer"           :n "<"   #'switch-to-buffer
        :desc "Browse files"            :n "."   #'find-file
        :desc "Toggle last popup"       :n "`"   #'+popup/toggle
        :desc "Blink cursor line"       :n "DEL" #'+nav-flash/blink-cursor
        :desc "Create or jump to bookmark" :n "RET" #'bookmark-jump

        :desc "Universal argument"      :n "u"  #'universal-argument
        :desc "window"                  :n "w"  evil-window-map


        (:desc "previous..." :prefix "["
          :desc "Buffer"                :nv "b" #'previous-buffer
          :desc "Diff Hunk"             :nv "d" #'git-gutter:previous-hunk
          :desc "Error"                 :nv "e" #'previous-error
          ;:desc "Spelling error"        :nv "s" #'evil-prev-flyspell-error
          ;:desc "Spelling correction"   :n  "S" #'flyspell-correct-previous-word-generic
        )
        (:desc "next..." :prefix "]"
          :desc "Buffer"                :nv "b" #'next-buffer
          :desc "Diff Hunk"             :nv "d" #'git-gutter:next-hunk
          :desc "Error"                 :nv "e" #'next-error
          ;:desc "Spelling error"        :nv "s" #'evil-next-flyspell-error
          ;:desc "Spelling correction"   :n  "S" #'flyspell-correct-word-generic
        )

        (:desc "search" :prefix "/"
          :desc "Project"                :nv "/" #'+ivy/project-search
          :desc "Project"                :nv "p" #'+ivy/project-search
          :desc "This Directory"         :nv "d" (λ! (+ivy/project-search t))
          :desc "In Buffer (swiper)"     :nv "b" #'swiper
          :desc "Tags (imenu)"           :nv "t" #'imenu
          :desc "Tags across buffers"    :nv "T" #'imenu-anywhere)

        (:desc "workspace" :prefix "TAB"
          :desc "Display tab bar"          :n "TAB" #'+workspace/display
          :desc "New workspace"            :n "n"   #'+workspace/new
          :desc "Load workspace from file" :n "l"   #'+workspace/load
          :desc "Load last session"        :n "L"   (λ! (+workspace/load-session))
          :desc "Save workspace to file"   :n "s"   #'+workspace/save
          :desc "Autosave current session" :n "S"   #'+workspace/save-session
          :desc "Switch workspace"         :n "."   #'+workspace/switch-to
          :desc "Kill all buffers"         :n "x"   #'doom/kill-all-buffers
          :desc "Delete session"           :n "X"   #'+workspace/kill-session
          :desc "Delete this workspace"    :n "d"   #'+workspace/delete
          ;:desc "Load session"             :n "L"   #'+workspace/load-session
          :desc "Rename workspace"         :n "r"   #'+workspace/rename
          :desc "Next workspace"           :n "]"   #'+workspace/switch-right
          :desc "Previous workspace"       :n "["   #'+workspace/switch-left)

        (:desc "buffer" :prefix "b"
          :desc "Kill other buffers"      :n "o" #'doom/kill-other-buffers
          :desc "Switch workspace buffer" :n "b" #'switch-to-buffer
          :desc "Next buffer"             :n "]" #'next-buffer
          :desc "Previous buffer"         :n "[" #'previous-buffer
          :desc "Sudo edit this file"     :n "S" #'doom/sudo-this-file)

        (:desc "code" :prefix "c"
          ; TODO https://github.com/redguardtoo/evil-nerd-commenter
          :desc "Commentary"              :v "c" #'evil-commentary
                                          :n "c" #'evil-commentary-line
          :desc "List errors"             :n "x" #'flycheck-list-errors
          :desc "Evaluate buffer/region"  :n "e" #'+eval/buffer
                                          :v "e" #'+eval/region
          :desc "Jump to definition"      :n "d" #'+lookup/definition
          :desc "Jump to references"      :n "D" #'+lookup/references
          :desc "Rotate text"             :n "!" #'rotate-text  ;https://www.emacswiki.org/emacs/RotateText
          )

        (:desc "file" :prefix "f"
          :desc "File Manager"              :n "m" #'+neotree/find-this-file
          :desc "Find file"                 :n "." #'find-file
          :desc "Sudo find file"            :n ">" #'doom/sudo-find-file
          :desc "Find file in project"      :n "p" #'projectile-find-file
          :desc "Find file from here"       :n "?" #'counsel-file-jump
          :desc "Find directory"            :n "d" #'dired
          :desc "Switch buffer"             :n "b" #'switch-to-buffer

          :desc "Recent files"              :n "r" #'recentf-open-files
          :desc "Recent project files"      :n "R" #'projectile-recentf
          :desc "Copy current filename"     :n "y" #'+default/yank-buffer-filename
          :desc "Find emacs library"        :n "l" #'find-library

          :desc "Find file in emacs.d"      :n "e" #'+default/find-in-emacsd
          :desc "Browse emacs.d"            :n "E" #'+default/browse-emacsd
          :desc "Find file in dotfiles"     :n "D" #'+default/find-in-config
          )

        (:desc "help" :prefix "h"
          :n "h" help-map
          :desc "Apropos"               :n  "a" #'apropos
          :desc "Describe bindings"     :n  "b" #'describe-bindings
          :desc "Describe char"         :n  "c" #'describe-char
          :desc "Describe DOOM module"  :n  "D" #'doom/describe-module
          :desc "Describe function"     :n  "f" #'describe-function
          :desc "Describe face"         :n  "F" #'describe-face
          :desc "Info"                  :n  "i" #'info-lookup-symbol
          :desc "Describe key"          :n  "k" #'describe-key
          :desc "Find documentation"    :n  "K" #'+lookup/documentation
          :desc "Command log"           :n  "L" #'global-command-log-mode
          :desc "Describe mode"         :n  "m" #'describe-mode
          :desc "Toggle Emacs log"      :n  "M" #'view-echo-area-messages
          :desc "Describe variable"     :n  "v" #'describe-variable
          :desc "Where is"              :n  "w" #'where-is
          :desc "Describe at point"     :n  "." #'helpful-at-point
          :desc "What face"             :n  "'" #'doom/what-face
          :desc "What minor modes"      :n  ";" #'doom/what-minor-mode)

        (:desc "toggle" :prefix "t"
          :desc "Spell"                  :n "S" #'flyspell-mode
          :desc "Syntax (flycheck)"      :n "s" #'flycheck-mode
          :desc "Taglist (imenu-list)"   :nv "l" #'imenu-list-smart-toggle
          :desc "Line numbers"           :n "L" #'doom/toggle-line-numbers
          :desc "Neotree"                :n "f" #'+neotree/open
          :desc "Frame fullscreen"       :n "F" #'toggle-frame-fullscreen
          :desc "Indent guides"          :n "i" #'highlight-indentation-mode
          ;:desc "Impatient mode"         :n "h" #'+impatient-mode/toggle
          ;:desc "Big mode"               :n "b" #'doom-big-font-mode
          ;:desc "Evil goggles"           :n "g" #'+evil-goggles/toggle
          ;:desc "org-tree-slide mode"    :n "p" #'+org-present/start)
          )

        (:desc "project" :prefix "p"
          :desc "Browse project"          :n  "." #'+default/browse-project
          :desc "Run cmd in project root" :nv "!" #'projectile-run-shell-command-in-root
          :desc "Compile project"         :n  "c" #'projectile-compile-project
          :desc "Switch project"          :n  "p" #'projectile-switch-project
          :desc "Recent project files"    :n  "r" #'projectile-recentf
          :desc "List project tasks"      :n  "t" #'+ivy/tasks
          :desc "Invalidate cache"        :n  "x" #'projectile-invalidate-cache)

        (:prefix "r"
          :desc "Eval Buffer" :n "r" #'+eval/buffer
          )
        ; TODO magit, insert, notes, remote, snippet
        )  ; end of leader

      ;; company-mode
      :i "C-SPC"  #'+company/complete
      (:prefix "C-x"
        :i "C-l"   #'+company/whole-lines
        :i "C-k"   #'+company/dict-or-keywords
        :i "C-f"   #'company-files
                                        ;:i "C-]"   #'company-etags
                                        ;:i "s"     #'company-ispell
                                        ;:i "C-s"   #'company-yasnippet
        :i "C-o"   #'+company/complete
                                        ;:i "C-o"   #'company-capf
        :i "C-n"   #'company-dabbrev-code
        :i "C-p"   #'+company/dabbrev-code-previous)
      (:after company
        (:map company-active-map
          ;; Don't interfere with `evil-delete-backward-word' in insert mode
          "C-w"     nil
          "C-n"     #'company-select-next
          "C-p"     #'company-select-previous
          "C-h"     #'company-show-doc-buffer
          "C-s"     #'company-filter-candidates
          "TAB"     #'company-complete-common-or-cycle
          [tab]     #'company-complete-common-or-cycle
          "S-TAB"   #'company-select-previous
          [backtab] #'company-select-previous)
        (:map company-search-map
          "C-n"     #'company-select-next-or-abort
          "C-p"     #'company-select-previous-or-abort
          "C-s"     (λ! (company-search-abort) (company-filter-candidates))
          [escape]  #'company-search-abort)
        )

      :n "C-p" #'counsel-projectile-find-file
      (:after counsel :map counsel-ag-map
          [backtab]  #'+ivy/wgrep-occur      ; search/replace on results
          "C-SPC"    #'ivy-call-and-recenter ; preview
        )

      (:after evil
        :textobj "a" #'evil-inner-arg                    #'evil-outer-arg
        :textobj "i" #'evil-indent-plus-i-indent         #'evil-indent-plus-a-indent
        (:map evil-window-map ; prefix "C-w"
          ;; Navigation
          "C-h"     #'evil-window-left
          "C-j"     #'evil-window-down
          "C-k"     #'evil-window-up
          "C-l"     #'evil-window-right
          "C-w"     #'other-window
          ;; Swapping windows
          "H"       #'+evil/window-move-left
          "J"       #'+evil/window-move-down
          "K"       #'+evil/window-move-up
          "L"       #'+evil/window-move-right
          ;; Window undo/redo
          "u"       #'winner-undo
          "C-r"     #'winner-redo
          "o"       #'doom/window-enlargen
          ;; Delete window
          "c"       #'+workspace/close-window-or-workspace
          "C-C"     #'ace-delete-window)
        )

      ;; surround
      (:after evil-surround
        :map evil-surround-mode-map
        :v "s" 'evil-surround-region
        )   ; originally was snipe
      :o  "s"  #'evil-surround-edit

      ;; flycheck
      (:after flycheck
        :map flycheck-error-list-mode-map
        :n "C-j" #'evil-window-down
        :n "C-k" #'evil-window-up
        :n "j"   #'flycheck-error-list-next-error
        :n "k"   #'flycheck-error-list-previous-error
        :n "RET" #'flycheck-error-list-goto-error)

      ;; ivy
      (:after ivy
        (:map ivy-minibuffer-map
          [escape] #'keyboard-escape-quit
          "C-SPC"  #'ivy-call-and-recenter  ; preview

          ;; basic editing
          "C-z"    #'undo
          "C-S-V"  #'yank
          ;;"C-r"    #'evil-paste-from-register
          "C-w"    #'ivy-backward-kill-word
          "C-u"    #'ivy-kill-line
          "C-b"    #'backward-word
          "C-f"    #'forward-word

          ;; movement
          "C-k"    #'ivy-previous-line
          "C-j"    #'ivy-next-line
          "C-v"    (lambda! (my/ivy-exit-new-window 'right))
          "C-s"    (lambda! (my/ivy-exit-new-window 'below))
          )
        (:map ivy-switch-buffer-map
          "C-d" 'ivy-switch-buffer-kill
          )
        )
      (:after swiper
        (:map swiper-map
          [backtab]  #'+ivy/wgrep-occur))

      ;; neotree
      (:after neotree
        :map neotree-mode-map
        :n "g"         nil
        :n [tab]       #'neotree-quick-look
        :n "RET"       #'neotree-enter
        :n [backspace] #'evil-window-prev
        :n "q"         #'neotree-hide
        :n "R"         #'neotree-refresh

        :n "c"         #'neotree-create-node
        :n "r"         #'neotree-rename-node
        :n "d"         #'neotree-delete-node

        :n "j"         #'neotree-next-line
        :n "k"         #'neotree-previous-line
        :n "h"         #'+neotree/collapse-or-up
        :n "l"         #'+neotree/expand-or-open
        :n "J"         #'neotree-select-next-sibling-node
        :n "K"         #'neotree-select-previous-sibling-node
        :n "H"         #'neotree-select-up-node
        :n "L"         #'neotree-select-down-node

        :n "G"         #'evil-goto-line
        :n "gg"        #'evil-goto-first-line
        :n "C-v"       #'neotree-enter-vertical-split
        :n "C-s"       #'neotree-enter-horizontal-split
        )

      ;(:after markdown-mode
      ;  (:map markdown-mode-map
      ;    ;; fix conflicts with private bindings
      ;    "<backspace>" nil
      ;    "<M-left>"    nil
      ;    "<M-right>"   nil))

      (:after comint
        ;; TAB auto-completion in term buffers
        :map comint-mode-map [tab] #'company-complete)
      (:map* (help-mode-map helpful-mode-map)
        :n "o"  #'ace-link-help
        :n "q"  #'quit-window
        :n "Q"  #'ivy-resume)
      ;(:after vc-annotate
      ;  :map vc-annotate-mode-map
      ;  [remap quit-window] #'kill-this-buffer)

      ; TODO: magit, MC, multiedit, snipe, flyspell, git timemachine,
      ; gist, realgud, yasnippet, undo-tree, ?markdown-mode
      )

