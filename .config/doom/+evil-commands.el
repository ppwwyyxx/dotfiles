;;;  -*- lexical-binding: t; -*-


(defalias 'ex! 'evil-ex-define-cmd)

(evil-define-command doom:cleanup-session (bang)
  (interactive "<!>")
  (doom/cleanup-session bang))

(evil-define-command doom:pwd (bang)
  (interactive "<!>")
  (if (not bang)
      (pwd)
    (kill-new default-directory)
    (message "Copied to clipboard")))

(evil-define-command doom:make (command &optional from-pwd)
  (interactive "<sh><!>")
  (let ((default-directory (if from-pwd default-directory (doom-project-root t)))
        (command (and command (evil-ex-replace-special-filenames command))))
    (compile command)))

;; editing:
(ex! "align"      #'+evil:align)
(ex! "ralign"     #'+evil:align-right)

(ex! "enhtml"     #'+web:encode-html-entities)
(ex! "dehtml"     #'+web:decode-html-entities)

(ex! "retab"      #'+evil:retab)


;; tools:
(ex! "repl"       #'+eval:repl)             ; invoke or send to repl
(ex! "sh[ell]"    #'+eshell:run)
(ex! "tcd"        #'+tmux:cd-here)          ; cd to default-directory in tmux
(ex! "er[rors]"   #'flycheck-list-errors)

;; buffers:
(ex! "cleanup"    #'doom:cleanup-session)
(ex! "kill"       #'doom/kill-this-buffer)
(ex! "msg"        #'view-echo-area-messages)
(ex! "A"          #'projectile-find-other-file)

(ex! "tabc[lose]" #'+workspace:delete)
(ex! "tabnew"     #'+workspace:new)
(ex! "tabn[ext]"  #'+workspace:switch-next)
(ex! "tabp[rev]"  #'+workspace:switch-previous)

;; search:
(ex! "ag"         #'+ivy:ag-from-cwd)
(ex! "todo"       #'+ivy:todo)

;; project:
(ex! "mak[e]"     #'doom:make)

;; vim:
;(ex! "cd"        #'+default:cd)
(ex! "pwd"        #'doom:pwd)
