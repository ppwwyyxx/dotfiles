;;; -*- lexical-binding: t; no-byte-compile: t -*-


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


(evil-define-command my/delete-window-or-workspace (bang)
  (interactive "<!>")
  (condition-case nil
      (evil-window-delete)
    (error
     (let ((is-last-window (eq (length (+workspace-list)) 1)))
       (call-interactively #'+workspace/delete)
       (when is-last-window
         (evil-quit))
       )
     ))
  )

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
;; don't close emacs with :q
(when (display-graphic-p)
  (ex! "q[uit]"     #'my/delete-window-or-workspace))
(ex! "k[ill]"     #'kill-this-buffer)
(ex! "msg"        #'view-echo-area-messages)
(ex! "A"          #'projectile-find-other-file)
(ex! "cleanup"    #'doom:cleanup-session)

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
