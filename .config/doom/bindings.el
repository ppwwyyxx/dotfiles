;;; -*- lexical-binding: t; no-byte-compile: t -*-

(defvar +default-minibuffer-maps
  `(minibuffer-local-map
    minibuffer-local-ns-map
    minibuffer-local-completion-map
    minibuffer-local-must-match-map
    minibuffer-local-isearch-map
    read-expression-map
    ,@(if (featurep! :completion ivy) '(ivy-minibuffer-map)))
  "A list of all the keymaps used for the minibuffer.")

;; expand-region's prompt can't tell what key contract-region is bound to, so we tell it explicitly.
(setq expand-region-contract-fast-key "H")

;; Don't let evil-collection interfere with certain keys
(setq evil-collection-key-blacklist
      (list "C-j" "C-k"
            "gd" "gf" "K"
            "[" "]"
            ";"  ;; my custom :
            doom-leader-key doom-localleader-key
            doom-leader-alt-key doom-localleader-alt-key))

(after! evil-mc
  (global-evil-mc-mode 0)
  (add-hook 'evil-mc-after-cursors-deleted #'turn-off-evil-mc-mode))

;; Make M-x harder to miss
(define-key! 'override
  "M-x" #'execute-extended-command
  "A-x" #'execute-extended-command)

;; Smarter C-a/C-e for both Emacs and Evil. C-a will jump to indentation.
;; Pressing it again will send you to the true bol. Same goes for C-e, except
;; it will ignore comments+trailing whitespace before jumping to eol.
(map! :gi "C-a" #'doom/backward-to-bol-or-indent
      :gi "C-e" #'doom/forward-to-last-non-comment-or-eol
      ;; Standardize the behavior of M-RET/M-S-RET as a "add new item
      ;; below/above" key.
      :gni [M-return]    #'+default/newline-below
      :gni [M-S-return]  #'+default/newline-above
      :gni [C-return]    #'+default/newline-below
      :gni [C-S-return]  #'+default/newline-above)

(map!
 [remap evil-jump-to-tag] #'projectile-find-tag
 [remap find-tag]         #'projectile-find-tag

 ;; Ensure there are no conflicts
 :nmvo doom-leader-key nil
 :nmvo doom-localleader-key nil

 ;; Smarter RET in normal mode
 :n "RET" (general-predicate-dispatch nil
            (and (bound-and-true-p flyspell-mode)
                 (+flyspell-correction-at-point-p))
            'flyspell-correct-word-generic)

 ;; Smarter newlines
 :i [remap newline] #'newline-and-indent  ; auto-indent on newline
 :i "C-j"           #'+default/newline    ; default behavior

 ;; expand-region
 :v "L"   (general-predicate-dispatch 'er/expand-region
            (eq (evil-visual-type) 'line)
            'evil-visual-char)
 :v "H" #'er/contract-region

 ;; smarter tab
 :n [tab] (general-predicate-dispatch nil
            (derived-mode-p 'magit-mode)
            'magit-section-toggle
            (derived-mode-p 'deadgrep-mode)
            'deadgrep-toggle-file-results
            (and (featurep! :editor fold)
                 (save-excursion (end-of-line) (invisible-p (point))))
            '+fold/toggle
            (fboundp 'evilmi-jump-items)
            'evilmi-jump-items)
 :v [tab] (general-predicate-dispatch nil
            (and (bound-and-true-p yas-minor-mode)
                 (or (eq evil-visual-selection 'line)
                     (and (fboundp 'evilmi-jump-items)
                          (save-excursion
                            (/= (point)
                                (progn (evilmi-jump-items nil)
                                       (point)))))))
            'yas-insert-snippet
            (fboundp 'evilmi-jump-items)
            'evilmi-jump-items)

 (:after vc-annotate
   :map vc-annotate-mode-map
   [remap quit-window] #'kill-this-buffer)

 (:map (help-mode-map helpful-mode-map)
   :n "o"  #'ace-link-help
   :n "q"  #'quit-window)

 ;; --- Global keybindings ---------------------------
 ;; clean-ups:
 :gnvime "M-l" nil
 :gnvime "M-h" nil

 :n  "]b" #'next-buffer
 :n  "[b" #'previous-buffer

 :n  "gp"    #'+evil/reselect-paste
 :n  "g="    #'widen
 :v  "g="    #'+evil:narrow-buffer
 :nm "gw" #'avy-goto-word-1
 :n  "gx"  #'evil-exchange  ; https://github.com/tommcdo/vim-exchange

 :gnvime "C-x C-b" #'ibuffer

 :n "M-f"   #'swiper

 :nm  ";"     #'evil-ex
 :nm  "*"     #'highlight-symbol-at-point
 :i "C-S-V"   #'yank

 ;; delete to blackhole register
 :v  [delete] (cmd! ()
                    (let ((evil-this-register ?_))
                      (call-interactively #'evil-delete)))

 ;; Vim-like editing commands
 :i "C-j"   #'evil-next-line
 :i "C-k"   #'evil-previous-line
 :i "C-h"   #'evil-backward-char
 :i "C-l"   #'evil-forward-char

 ;; window management
 :nm "C-h"   #'evil-window-left
 :nm "C-j"   #'evil-window-down
 :nm "C-k"   #'evil-window-up
 :nm "C-l"   #'evil-window-right
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
   "C-C"     #'ace-delete-window
   [up]      #'evil-window-increase-height
   [down]    #'evil-window-decrease-height
   [right]   #'evil-window-increase-width
   [left]    #'evil-window-decrease-width)

 :textobj "x" #'evil-inner-xml-attr               #'evil-outer-xml-attr
 :textobj "a" #'evil-inner-arg                    #'evil-outer-arg
 :textobj "B" #'evil-textobj-anyblock-inner-block #'evil-textobj-anyblock-a-block
 :textobj "i" #'evil-indent-plus-i-indent         #'evil-indent-plus-a-indent
 :textobj "k" #'evil-indent-plus-i-indent-up      #'evil-indent-plus-a-indent-up
 :textobj "j" #'evil-indent-plus-i-indent-up-down #'evil-indent-plus-a-indent-up-down

 ;; evil-surround
 :v "s" #'evil-surround-region
 :o "s" #'evil-surround-edit

 (:after dired :map dired-mode-map
   :nmv ";" nil
   :nmv "]" nil
   :nmv "[" nil)
)


;;
;; Universal evil integration
(when (featurep! :editor evil +everywhere)
  (map!
   ;; Have C-u behave similarly to `doom/backward-to-bol-or-indent'.
   ;; NOTE SPC u replaces C-u as the universal argument.
   :i "C-u"   #'doom/backward-kill-to-bol-and-indent
   :i "C-w"   #'backward-kill-word
   ;; Vimmish ex motion keys
   :gi "C-b"   #'backward-word
   :gi "C-f"   #'forward-word)

  (after! view
    (define-key view-mode-map [escape] #'View-quit-all))
  (after! man
    (evil-define-key* 'normal Man-mode-map "q" #'kill-this-buffer))

  ;; Minibuffer
  (define-key! evil-ex-completion-map
    "C-a"   #'move-beginning-of-line
    "C-b"   #'backward-word
    "C-f"   #'forward-word
    "C-S-V" #'yank
    "C-s" (if (featurep! :completion ivy)
              #'counsel-minibuffer-history
            #'helm-minibuffer-history))

  (define-key! :keymaps +default-minibuffer-maps
    [escape] #'abort-recursive-edit
    "C-S-V"  #'yank
    "C-a"    #'move-beginning-of-line
    "C-w"    #'backward-kill-word
    "C-u"    #'backward-kill-sentence
    "C-b"    #'backward-word
    "C-f"    #'forward-word)
  )

;;
;; Module keybinds

;;; :feature
(map!
 (:when (featurep! :tools eval)
   :g  "M-r" #'+eval/buffer)

 (:when (featurep! :tools lookup)
   :nv "K"  #'+lookup/documentation
   :nv "gd" #'+lookup/definition
   :nv "gD" #'+lookup/references
   :nv "gf" #'+lookup/file
   )
 (:when IS-LINUX
   :nvm "C-z" #'zeal-at-point)

 (:when (featurep! :editor snippets)
   (:after yasnippet
     (:map yas-keymap
       "C-e"           #'+snippets/goto-end-of-field
       "C-u"           #'+snippets/delete-to-start-of-field
       "C-a"           #'+snippets/goto-start-of-field
       [backspace]     #'+snippets/delete-backward-char
       [delete]        #'+snippets/delete-forward-char-or-field
       )
     (:map yas-minor-mode-map
       "SPC"           yas-maybe-expand  ;; I use space to expand a small collection of snippets
       ))
   )

 (:when (featurep! :tools flycheck)
   :m "]e" #'next-error
   :m "[e" #'previous-error
   (:after flycheck
     :map flycheck-error-list-mode-map
     :n "C-j" #'evil-window-down
     :n "C-k" #'evil-window-up
     :n "j"   #'flycheck-error-list-next-error
     :n "k"   #'flycheck-error-list-previous-error
     :n "RET" #'flycheck-error-list-goto-error))

 ;; workspace/tab related
 (:when (featurep! :ui workspaces)
   :nm "M-t"  #'+workspace/new
   :nm "M-T"  #'+workspace/display
   :nmi "M-1" #'+workspace/switch-to-0
   :nmi "M-2" #'+workspace/switch-to-1
   :nmi "M-3" #'+workspace/switch-to-2
   :nmi "M-4" #'+workspace/switch-to-3
   :nmi "M-5" #'+workspace/switch-to-4
   :nmi "M-6" #'+workspace/switch-to-5
   :nmi "M-7" #'+workspace/switch-to-6
   :nmi "M-8" #'+workspace/switch-to-7
   :nmi "M-9" #'+workspace/switch-to-8
   :nmi "M-0" #'+workspace/switch-to-last
   :nm  "]w"  #'+workspace/switch-right
   :nm  "[w"  #'+workspace/switch-left)
 )

;; :completion
(map!
 (:when (featurep! :completion company)
   ;; company-mode
   ;;"C-SPC" nil  ;; clear
   :i "C-SPC"  #'+company/complete
   (:prefix "C-x"
     :i "C-l"   #'+company/whole-lines
     :i "C-k"   #'+company/dict-or-keywords
     :i "C-f"   #'company-files
     ;;:i "C-]"   #'company-etags
     ;;:i "C-s"   #'company-yasnippet
     :i "C-s"   #'yas-expand
     :i "C-o"   #'+company/complete
     ;;:i "C-o"   #'company-capf
     :i "C-n"   #'+company/dabbrev
     :i "C-p"   #'+company/dabbrev-code-previous)

   (:after company
     (:map company-active-map
       "C-w"     nil   ;; Don't interfere with `evil-delete-backward-word' in insert mode
       "C-n"     #'company-select-next
       "C-p"     #'company-select-previous
       "C-h"     #'company-show-doc-buffer
       "C-s"     #'company-filter-candidates
       [tab]     #'company-complete-common-or-cycle
       "RET"     #'company-complete-selection
       "S-TAB"   #'company-select-previous
       [backtab] #'company-select-previous
       "<f1>"    nil
       [escape]  (cmd! (company-abort) (evil-normal-state))
       )
     (:map company-search-map ; applies to `company-filter-map' too
       "C-n"     #'company-select-next-or-abort
       "C-p"     #'company-select-previous-or-abort
       "C-s"     (λ! (company-search-abort) (company-filter-candidates))
       [escape]  #'company-search-abort)
     ;; TAB auto-completion in term buffers
     (:after comint
       :map comint-mode-map [tab] #'company-complete)
     ))

 ;; ivy
 (:when (featurep! :completion ivy)
   (:map (help-mode-map helpful-mode-map)
     :n "Q" #'ivy-resume)

   (:after ivy
     (:map ivy-minibuffer-map
       [escape] #'keyboard-escape-quit
       "C-SPC"  #'ivy-call-and-recenter  ; preview

       ;; basic editing
       "C-S-V"  #'yank
       "C-w"    #'ivy-backward-kill-word
       "C-u"    #'ivy-kill-whole-line
       "C-b"    #'backward-word
       "C-f"    #'forward-word

       ;; movement
       ;; this allows us to move out of ivy buffer
       "C-k"    #'evil-window-up
       "C-j"    #'evil-window-down
       ;; split window and execute action, similar to ctrlp.vim
       "C-v"    (cmd! (my/ivy-exit-new-window 'right))
       "C-s"    (cmd! (my/ivy-exit-new-window 'below))
       )
     (:map ivy-switch-buffer-map
       "C-d" 'ivy-switch-buffer-kill
       ))

   ;; just like vim ctrlp
   :nm "C-p" #'counsel-projectile-find-file
   (:after counsel
     :map counsel-ag-map
     "C-SPC"    #'ivy-call-and-recenter ; preview
     [backtab]  #'+ivy/wgrep-occur      ; search/replace on results
     )

   (:after swiper
     (:map swiper-map
       [backtab]  #'+ivy/wgrep-occur))
   )
 )

(map!
 (:when (featurep! :ui neotree)
   :after neotree
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

 (:when (featurep! :ui vc-gutter)
   ;; TODO if under GUI, use alt-hl
   :nm  "]d" #'git-gutter:next-hunk
   :nm  "[d" #'git-gutter:previous-hunk
   )

 :nme "C--" #'text-scale-decrease
 :nme "C-=" #'text-scale-increase
 :nme "C-0" (cmd! (text-scale-set 0))
 "<C-mouse-5>" #'text-scale-decrease
 "<C-mouse-4>" #'text-scale-increase

 (:after goto-addr
   :map goto-address-highlight-keymap
   "RET" #'goto-address-at-point)

 (:when (featurep! :editor multiple-cursors)
   ;; doom already clears the map, which is nice!
   (:after evil-mc :map evil-mc-key-map
     :nv "C-n" #'evil-mc-make-and-goto-next-match
     :nv "C-p" #'evil-mc-make-and-goto-prev-match
     :nv "C-S-n" #'evil-mc-skip-and-goto-next-match
     :nv "C-S-p" #'evil-mc-skip-and-goto-prev-match)
   )

 (:when (featurep! :emacs vc)
   ;; git-timemachine
   (:after git-timemachine
     (:map git-timemachine-mode-map
       :n "C-p" #'git-timemachine-show-previous-revision
       :n "C-n" #'git-timemachine-show-next-revision
       :n "[["  #'git-timemachine-show-previous-revision
       :n "]]"  #'git-timemachine-show-next-revision
       :n "q"   #'git-timemachine-quit
       :n "gb"  #'git-timemachine-blame))
   )
 )


(map! :leader
      :desc "Universal argument"         :n "u"  #'universal-argument
      :desc "window"                     :nm "w"  evil-window-map

      (:when (featurep! :ui workspaces)
        :desc "Switch workspace buffer" "," #'persp-switch-to-buffer
        :desc "Switch buffer"           "<" #'switch-to-buffer)


      :desc "Toggle last popup"          :n "~"   #'+popup/toggle
      :desc "Find files from here"       :n "."   #'counsel-file-jump
      :desc "Find file in project"       :n "SPC" #'projectile-find-file
      ;; :desc "Blink cursor line"          :n "DEL" #'+nav-flash/blink-cursor
      :desc "Create or jump to bookmark" :n "RET" #'bookmark-jump

      (:when (featurep! :completion ivy)
        :desc "Resume last search"     :n "'"   #'ivy-resume)

      (:desc "search" :prefix "/"
        :desc "Project"                :nv "/" #'+ivy/project-search
        :desc "Project"                :nv "p" #'+ivy/project-search
        :desc "This Directory"         :nv "d" #'+ivy/project-search-from-cwd
        :desc "In Buffer (swiper)"     :nv "b" #'swiper
        :desc "Tags (imenu)"           :nv "t" #'imenu
        :desc "Tags across buffers"    :nv "T" #'imenu-anywhere
        :desc "Online providers"       :nv "o" #'+lookup/online-select)

      (:desc "previous..." :prefix "["
        :desc "Buffer"                :nv "b" #'previous-buffer
        :desc "Diff Hunk"             :nv "d" #'git-gutter:previous-hunk
        :desc "Error"                 :nv "e" #'previous-error
        :desc "Spelling error"        :nv "s" #'evil-prev-flyspell-error
        )
      (:desc "next..." :prefix "]"
        :desc "Buffer"                :nv "b" #'next-buffer
        :desc "Diff Hunk"             :nv "d" #'git-gutter:next-hunk
        :desc "Error"                 :nv "e" #'next-error
        :desc "Spelling error"        :nv "s" #'evil-next-flyspell-error
        )

      (:desc "workspace" :prefix [tab]
        ;;:desc "Display tab bar"          :n [tab] #'+workspace/display
        :desc "Switch workspace"         :n [tab] #'+workspace/switch-to
        :desc "New workspace"            :n "n"   #'+workspace/new
        :desc "Load last session"        :n "L"   #'+workspace/load-session
        :desc "Autosave current session" :n "S"   #'+workspace/save-session
        :desc "Switch workspace"         :n "."   #'+workspace/switch-to
        :desc "Kill all buffers"         :n "x"   #'doom/kill-all-buffers
                                        ;:desc "Delete session"           :n "X"   #'+workspace/kill-session
        :desc "Delete this workspace"    :n "d"   #'+workspace/delete
                                        ;:desc "Load session"             :n "L"   #'+workspace/load-session
        :desc "Rename workspace"         :n "r"   #'+workspace/rename
        :desc "Next workspace"           :n "]"   #'+workspace/switch-right
        :desc "Previous workspace"       :n "["   #'+workspace/switch-left)

      (:desc "buffer" :prefix "b"
        :desc "Toggle narrowing"            "-"   #'doom/clone-and-narrow-buffer
        :desc "Kill buffer"             :n "k" #'kill-this-buffer
        :desc "Kill other buffers"      :n "o" #'doom/kill-other-buffers
        :desc "Switch workspace buffer" :n "b" #'switch-to-buffer
        :desc "Next buffer"             :n "]" #'next-buffer
        :desc "Previous buffer"         :n "[" #'previous-buffer
        :desc "Sudo edit this file"     :n "S" #'doom/sudo-this-file)

      (:desc "code" :prefix "c"
                                        ; TODO https://github.com/redguardtoo/evil-nerd-commenter
        :desc "Commentary"              :n "c" #'evilnc-comment-or-uncomment-lines
        :desc "Commentary"              :v "c" #'evilnc-comment-or-uncomment-lines
        :desc "List errors"             :n "x" #'flycheck-list-errors
        :desc "Evaluate buffer/region"  :n "e" #'+eval/buffer
        :v "e" #'+eval/region
        :desc "Diff with File"          :n "d" #'diff-buffer-with-file
        :desc "Rotate text"             :n "!" #'rotate-text  ;https://www.emacswiki.org/emacs/RotateText
        :desc "Insert snippet"         :nv "s" #'yas-insert-snippet
        :desc "Start MultiCursor"       :n "m" #'turn-on-evil-mc-mode
        :v "m" (cmd! () (turn-on-evil-mc-mode) (evil-mc-make-all-cursors))
        )

      (:desc "file" :prefix "f"
        :desc "File Manager"              :n "m" #'+neotree/find-this-file
        :desc "Find file from here"       :n "." #'counsel-file-jump
        :desc "Find file in other project":n ">" #'doom/browse-in-other-project
        :desc "Sudo find file"            :n "S" #'doom/sudo-find-file
        :desc "Find file in project"      :n "p" #'projectile-find-file
        :desc "Find file"                 :n "f" #'find-file
        :desc "Find directory"            :n "d" #'dired
        ;; #'deer, #'ranger
        :desc "Switch buffer"             :n "b" #'switch-to-buffer

        :desc "Recent files"              :n "r" #'recentf-open-files
        :desc "Recent project files"      :n "R" #'projectile-recentf
        :desc "Copy current filename"     :n "y" #'+default/yank-buffer-filename
        :desc "Find emacs library"        :n "l" #'find-library

        :desc "Find file in emacs.d"      :n "e" #'+default/find-in-emacsd
        :desc "Browse emacs.d"            :n "E" #'+default/browse-emacsd
        :desc "Find file in dotfiles"     :n "D" #'doom/find-file-in-private-config)

      (:desc "git" :prefix "g"
        :desc "Magit blame"            :n  "b" #'magit-blame-addition
        :desc "Magit diff this file"   :n  "d" #'magit-diff-buffer-file
        :desc "Magit diff repo"        :n  "D" #'magit-diff-working-tree
        :desc "Magit status"           :n  "g" #'magit-status
        :desc "Magit repo log"         :n  "l" #'magit-log-current
        :desc "Magit log for this file":n  "L" #'magit-log-buffer-file
        :desc "Magit push"             :n  "p" #'magit-push
        :desc "Magit pull"             :n  "P" #'magit-pull
        :desc "Git revert hunk"        :n  "r" #'git-gutter:revert-hunk
        :desc "Git revert file"        :n  "R" #'vc-revert
        :desc "Git stage hunk"         :n  "s" #'git-gutter:stage-hunk
                                        ;:desc "Git stage file"        :n  "S" #'magit-stage-file
        :desc "Git time machine"       :n  "t" #'git-timemachine-toggle
        :desc "Copy URL of line"       :n  "C" #'git-link
        :desc "Browse Issues"          :n  "I" #'+vcs/git-browse-issues
                                        ;:desc "Git unstage file"      :n  "U" #'magit-unstage-file
        :desc "Next hunk"              :nv "]" #'git-gutter:next-hunk
        :desc "Previous hunk"          :nv "[" #'git-gutter:previous-hunk)

      (:prefix ("h" . "help")
        :desc "What face"                     "'"   #'doom/what-face
        :desc "Describe at point"             "."   #'helpful-at-point
        :desc "Describe active minor modes"   ";"   #'doom/describe-active-minor-mode
        :desc "Open Doom manual"              "D"   #'doom/open-manual
        :desc "Open vanilla sandbox"          "E"   #'doom/open-vanilla-sandbox
        :desc "Describe face"                 "F"   #'describe-face
        :desc "Find documentation"            "K"   #'+lookup/documentation
        :desc "Command log"                   "L"   #'global-command-log-mode
        :desc "Describe mode"                 "M"   #'describe-mode
        :desc "Reload private config"         "R"   #'doom/reload
        :desc "Print Doom version"            "V"   #'doom/version
        :desc "Apropos"                       "a"   #'apropos
        :desc "Open Bug Report"               "b"   #'doom/open-bug-report
        :desc "Describe char"                 "c"   #'describe-char
        :desc "Describe DOOM module"          "d"   #'doom/describe-module
        :desc "Describe function"             "f"   #'describe-function
        :desc "Emacs help map"                "h"   help-map
        :desc "Info"                          "i"   #'info-lookup-symbol
        :desc "Describe key"                  "k"   #'describe-key
        :desc "Find library"                  "l"   #'find-library
        :desc "View *Messages*"               "m"   #'view-echo-area-messages
        :desc "Toggle profiler"               "p"   #'doom/toggle-profiler
        :desc "Reload theme"                  "r"   #'doom/reload-theme
        :desc "Describe DOOM setting"         "s"   #'doom/describe-setters
        :desc "Describe variable"             "v"   #'describe-variable
        :desc "Man pages"                     "w"   #'+default/man-or-woman
        )

      (:prefix ("p" . "project")
        :desc "Browse project"               "." #'+default/browse-project
        :desc "Find file in project"         "/" #'projectile-find-file
        :desc "Run cmd in project root"      "!" #'projectile-run-shell-command-in-root
        :desc "Compile project"              "c" #'projectile-compile-project
        :desc "Find other file"              "o" #'projectile-find-other-file
        :desc "Switch project"               "p" #'projectile-switch-project
        :desc "Recent project files"         "r" #'projectile-recentf
        :desc "List project tasks"           "t" #'+default/project-tasks
        :desc "Invalidate cache"             "x" #'projectile-invalidate-cache)

      ;; Unorganized:
      (:desc "run Stuff" :prefix "r"
        :desc "Eval Buffer" :n "r" #'+eval/buffer
        :desc "Terminal"    :n "t" #'multi-term
        :desc "Make"        :n "m" #'+make/run
        )

      (:desc "toggle" :prefix "t"
        :desc "Spell"                  :n "S" #'flyspell-mode
        :desc "Syntax (flycheck)"      :n "s" #'flycheck-mode
        :desc "Taglist (imenu-list)"  :nv "l" #'imenu-list-smart-toggle
        :desc "Line Numbers"           :n "L" #'doom/toggle-line-numbers
        :desc "Neotree"                :n "f" #'+neotree/open
        :desc "Frame Fullscreen"       :n "F" #'toggle-frame-fullscreen
        :desc "Indent Guides"          :n "i" #'highlight-indent-guides-mode
        :desc "Line Wrap"              :n "w" #'visual-line-mode
        :desc "Command Log"            :n "C" #'clm/toggle-command-log-buffer
        )

      (:desc "XXX" :prefix "n"
        :desc "No Highlight" :n "o" (cmd! ()
                                          (evil-ex-nohighlight)
                                          (unhighlight-regexp t)
                                          (evil-mc-undo-all-cursors))
        )

      (:after json-mode :map json-mode-map
        :n "js" #'json-mode-beautify
        :n "bu" #'json-mode-beautify
        )
      (:after javascript-mode :map javascript-mode-map
        :n "js" #'web-beautify-js
        :n "bu" #'web-beautify-js
        )
      (:after web-mode :map web-mode-map
        :n "bu" #'web-beautify-html)
      (:after css-mode :map css-mode-map
        :n "bu"  #'web-beautify-css
        :n "css" #'web-beautify-css
        )
      ) ;; end of leader
