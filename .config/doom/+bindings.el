;;;  -*- lexical-binding: t; -*-


(map! [remap newline]          #'newline-and-indent

      ;; Ensure there are no conflicts
      ;:nmvo doom-leader-key nil
      ;:nmvo doom-localleader-key nil

      ;; --- Global keybindings ---------------------------
      ;; Make M-x available everywhere
      :gnvime "M-x" #'execute-extended-command
      ;; :gnvime "M-:" #'doom/open-scratch-buffer
      :gnvime "C-x C-b" #'ibuffer
      :ne "M-r"   #'+eval/buffer
      :ne "M-f"   #'swiper
      :n  "M-s"   #'save-buffer
      :n  ";"     #'evil-ex

      ; workspace/tab related
      :ne "M-t"       #'+workspace/new
      :ne "M-T"       #'+workspace/display
      :ne "M-1"       (λ! (+workspace/switch-to 0))
      :ne "M-2"       (λ! (+workspace/switch-to 1))
      :ne "M-3"       (λ! (+workspace/switch-to 2))
      :ne "M-4"       (λ! (+workspace/switch-to 3))
      :ne "M-5"       (λ! (+workspace/switch-to 4))
      :ne "M-6"       (λ! (+workspace/switch-to 5))
      :ne "M-7"       (λ! (+workspace/switch-to 6))
      :ne "M-8"       (λ! (+workspace/switch-to 7))
      :ne "M-9"       (λ! (+workspace/switch-to 8))
      :ne "M-0"       #'+workspace/switch-to-last
      ; window management
      :en "C-h"   #'evil-window-left
      :en "C-j"   #'evil-window-down
      :en "C-k"   #'evil-window-up
      :en "C-l"   #'evil-window-right

      (:after counsel
        :n "C-p" #'counsel-projectile-find-file)


      (:after evil-surround
        :map evil-surround-mode-map
        :v "s" 'evil-surround-region)   ; originally was snipe


      (:leader
        ;; :desc "M-x"                     :nv ":"  #'execute-extended-command
        ; jumps:
        :desc "Find file in project"    :n "SPC" #'projectile-find-file
        :desc "Switch workspace buffer" :n ","   #'persp-switch-to-buffer
        :desc "Switch buffer"           :n "<"   #'switch-to-buffer
        :desc "Browse files"            :n "."   #'find-file
        :desc "Toggle last popup"       :n "`"   #'+popup/toggle
        :desc "Blink cursor line"       :n "DEL" #'+nav-flash/blink-cursor
        :desc "Jump to bookmark"        :n "RET" #'bookmark-jump

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
          :desc "Directory"              :nv "d" (λ! (+ivy/project-search t))
          :desc "Buffer"                 :nv "b" #'swiper
          :desc "Symbols"                :nv "i" #'imenu
          :desc "Symbols across buffers" :nv "I" #'imenu-anywhere)

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

        (:prefix "c"
          :desc "Commentary" :v "c" #'evil-commentary
          :n "c" #'evil-commentary-line
          )

        (:prefix "f"
          :desc "File Manager" :n "m" #'+neotree/open
          )

        (:prefix "r"
          :desc "Eval Buffer" :n "r" #'+eval/buffer
          )
        )
      )

