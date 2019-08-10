;;; -*- lexical-binding: t; no-byte-compile: t -*-

(setq tab-width 4)
(setq tab-always-indent nil)
(setq tramp-default-method "ssh")

(add-hook 'prog-mode-hook #'goto-address-prog-mode)
(add-hook 'ielm-mode-hook #'visual-line-mode)
(add-hook 'eshell-mode-hook #'visual-line-mode)
(add-hook 'markdown-mode-hook #'visual-line-mode)

(def-package! fcitx
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=10867
  :if (and IS-LINUX (string= (getenv "LC_CTYPE") "zh_CN.UTF-8"))
  :config
  (fcitx-aggressive-setup)
  (setq fcitx-use-dbus t))

;; TODO only include for Python mode?
;; (def-package! aggressive-indent
;;   :demand t
;;   :config
;;   (global-aggressive-indent-mode 1)
;;   (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
;; 	(add-to-list 'aggressive-indent-excluded-modes 'c++-mode)
;;   (add-to-list 'aggressive-indent-excluded-modes 'c-mode))


(def-package! stickyfunc-enhance
  :defer t
  :init
  (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode))

(def-package! vim-empty-lines-mode
  :hook (prog-mode . vim-empty-lines-mode))

(def-package! edit-server
  :demand t
  :when (display-graphic-p)
  :config
  (setq edit-server-default-major-mode 'markdown-mode)
  (edit-server-start))

(after! projectile
  (setq projectile-require-project-root t)
  (projectile-cleanup-known-projects)
  (setq +workspaces-switch-project-function #'find-file)
  )

(after! evil
  (global-evil-surround-mode 1)
  (setq
   evil-move-beyond-eol t
   evil-jumps-cross-buffers t)
  (evil-ex-define-cmd "vsp" 'evil-window-vsplit))

(after! company
  (setq company-quickhelp-delay nil
        company-show-numbers t)
  (setq company-box-show-single-candidate t))

(after! undo-tree
  ;; undo-tree may be buggy
  (setq undo-tree-auto-save-history t)
  (defun my/undo-tree-overwrite-advice (&optional filename noerror)
    (undo-tree-save-history nil t))
  (advice-add 'undo-tree-load-history :after-until #'my/undo-tree-overwrite-advice)
  )

(after! magit
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

(after! yasnippet
  ;; https://github.com/emacs-evil/evil/issues/254
  (add-hook 'yas-before-expand-snippet-hook
            #'(lambda ()
                (when (evil-visual-state-p)
                  (let ((p (point))
                        (m (mark)))
                    (evil-insert-state)  ;; yasnippet is happier while in insert mode
                    (goto-char p)  ;; but we need the selection
                    (set-mark m)))
                )))

(after! ivy
  (setq counsel-find-file-ignore-regexp "\\.elc\\'\\|\\.pyc\\'\\|\\.o\\'")
  (defun my/ivy-exit-new-window (side)
    (let ((current-act (ivy--get-action ivy-last))
          (current-caller (ivy-state-caller ivy-last)))
      (message "Act=%s, Caller=%s" current-act current-caller)
      (if (or
           (member current-act '(ivy--switch-buffer-action
                                 counsel-projectile-find-file-action
                                 counsel-find-file-action
                                 counsel-git-grep-action
                                 counsel--find-symbol))
           (member current-caller '(counsel-imenu
                                    counsel-recentf
                                    counsel-projectile-find-file
                                    counsel-file-jump
                                    counsel-find-library
                                    ivy-switch-buffer
                                    ivy-xref-show-xrefs
                                    +ivy/tasks)))
          (ivy-exit-with-action
           (lambda (x)
             (select-window (split-window nil nil side))
             ;; setf was wrongly expanded at load-time for unclear reason
             (eval '(progn
                      (setf (ivy-state-window ivy-last)
                            (selected-window)
                            )))
             ;; so that 'with-ivy-window' (used a lot inside predefined actions) will use the new window
             (balance-windows)
             (funcall current-act x)
             ))

        (ivy-exit-with-action current-act))
      )
    ))

(when (featurep! :emacs vc)
  (defun my/my-own-project-p()
    "Return value: 0 - unknown; 1 - yes; -1 - no"
    (let ((link (ignore-errors (+vc-git-root-url))))
      (cond
       ((not link) 0)
       ((or
         (cl-search "ppwwyyxx" link)
         (cl-search "tensorpack" link)
         (cl-search "facebook" link)
         ) 1)
       (t -1)
       )
      ))
  (add-hook 'prog-mode-hook (lambda!
                             (if (eq (my/my-own-project-p) 1)
                                 (doom|enable-delete-trailing-whitespace))))
  )

(load! "ui")
(load! "lang")
(load! "bindings")
(load! "evil-commands")
(when (file-readable-p (concat doom-private-dir "private.el"))
  (load! "private"))
