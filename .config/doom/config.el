;;; -*- lexical-binding: t; no-byte-compile: t -*-

(setq tab-width 4)
(setq tab-always-indent nil)
(setq tramp-default-method "ssh")

(add-hook 'prog-mode-hook #'doom|enable-delete-trailing-whitespace)
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

(def-package! aggressive-indent
  :demand t
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  (add-to-list
   'aggressive-indent-dont-indent-if
   '(and (derived-mode-p 'c++-mode)
         (null (string-match "\\([;{}]\\)"
                             (thing-at-point 'line)))))
  )

(def-package! stickyfunc-enhance
  :defer t
  :init
  (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode))

(def-package! vim-empty-lines-mode
  :hook (prog-mode . vim-empty-lines-mode))

(after! imenu-list
  (setq imenu-list-auto-resize nil))

(after! projectile
  (setq projectile-require-project-root t)
  (projectile-cleanup-known-projects))

(after! evil
  (global-evil-surround-mode 1)
  (setq
   evil-move-beyond-eol t
   evil-jumps-cross-buffers nil)
  (evil-ex-define-cmd "vsp" 'evil-window-vsplit))

(after! company
  (setq company-quickhelp-delay nil
        company-show-numbers t))

(after! undo-tree
  ;; may be buggy
  (setq undo-tree-auto-save-history t))

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
;; (after! helm-dash
;;   (setq helm-dash-docsets-path
;;         (substitute-in-file-name "$HOME/.local/share/Zeal/Zeal/docsets/"))

;;   (defun cpp-doc () (interactive) (setq-local helm-dash-docsets '("C++")))
;;   (defun python-doc () (interactive) (setq-local helm-dash-docsets '("Python_3")))
;;   (defun elisp-doc () (interactive) (setq-local helm-dash-docsets '("Emacs_Lisp")))
;;   (add-hook 'c++-mode-hook #'cpp-doc)
;;   (add-hook 'python-mode-hook #'python-doc)
;;   (add-hook 'elisp-mode-hook #'elisp-doc)

;;   (defun helm-dash-browse-url(search-result)
;;     (message "Result=%s" search-result)
;;     (let ((docset-name (car search-result))
;;           (identifier (nth 1 (cadr search-result))))
;;       (message "%s-%s" docset-name identifier)
;;       )
;;     )
;;   )

(after! ivy
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

(when (featurep! :feature version-control)
  (defun my/my-own-project-p()
    (let ((link (ignore-errors (+vcs-root))))
      (and (or
            (not link)
            (cl-search "ppwwyyxx" link)
            (cl-search "tensorpack" link)
            (cl-search "facebook" link)
            ) t)
      ))
  )

(load! "ui")
(load! "lang")
(load! "bindings")
(load! "evil-commands")
