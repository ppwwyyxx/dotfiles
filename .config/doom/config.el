;;; -*- lexical-binding: t -*- ;  -*- no-byte-compile: t -*-

(load! +ui)
(load! +bindings)
(load! +evil-commands)

(add-to-list 'exec-path (substitute-in-file-name "$HOME/bin"))
(add-hook 'prog-mode-hook #'doom|enable-delete-trailing-whitespace)
(add-hook 'ielm-mode-hook #'visual-line-mode)
(add-hook 'eshell-mode-hook #'visual-line-mode)
(add-hook 'markdown-mode-hook #'visual-line-mode)

(after! imenu-list
  (setq imenu-list-auto-resize nil)
  )

(after! projectile
  (setq projectile-require-project-root t)
  (projectile-cleanup-known-projects)
  )

(after! pythonic
  (setq python-shell-virtualenv-root "/usr")  ; it will otherwise look for .local/bin which may contain ipython2
)

(after! evil
  (global-evil-surround-mode 1)
  (setq
   evil-move-beyond-eol t
   evil-jumps-cross-buffers nil)
  (evil-ex-define-cmd "vsp" 'evil-window-vsplit)
  )

(after! company
  (setq company-quickhelp-delay nil
        company-show-numbers t
        ))

(after! undo-tree
  ;; may be buggy
  (setq undo-tree-auto-save-history t)
  )

(after! magit
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  )

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

  (defun my/ivy-split-action (x current-act side)
        (message "%s" (ivy-state-window ivy-last))
        (setf (ivy-state-window ivy-last)
              (select-window (split-window nil nil side))
              )      ; so that with-ivy-window (used a lot inside predefined actions) will use the new window
        (balance-windows)
        (funcall current-act x)
    )

(after! ivy
  ;(defun switch-to-buffer-new-window (buffer-or-name side)
  ;  "Switch to BUFFER-OR-NAME in a new window, based on SIDE."
  ;  (select-window (split-window nil nil side))
  ;  (balance-windows)
  ;  (switch-to-buffer buffer-or-name)
  ;  )
  ;(defun find-file-new-window (filename side)
  ;  (switch-to-buffer-new-window (find-file-noselect filename) side))
  ;(defmacro my/new-window-context (code side)
  ;  `(letf (
  ;        ;((symbol-function 'find-file-other-window) (lambda (a) (find-file-new-window a ,side)))
  ;          ((symbol-function 'find-file) (lambda (a) (find-file-new-window a ,side)))
  ;        ;((symbol-function 'switch-to-buffer-other-window) (lambda (a) (switch-to-buffer-new-window a ,side)))
  ;          )
  ;     ,code
  ;     )
  ;  )

  (defun my/ivy-exit-new-window (side)
    (let ((current-act (ivy--get-action ivy-last))
          (current-caller (ivy-state-caller ivy-last))
          )
      (message "Act=%s, Caller=%s" current-act current-caller)
      (if (or
           (member current-act '(
                                 ivy--switch-buffer-action
                                 counsel-projectile-find-file-action
                                 counsel-find-file-action
                                 counsel-git-grep-action
                                 counsel--find-symbol))
           (member current-caller '(
                                    counsel-imenu
                                    counsel-recentf
                                    counsel-find-library
                                    ivy-switch-buffer
                                    ivy-xref-show-xrefs
                                    +ivy/tasks
                                    ))
           )
          (ivy-exit-with-action
           (lambda (x) (my/ivy-split-action x current-act side))
           )
           ;(lambda (x)
           ;  (message "%s" (ivy-state-window ivy-last))
           ;  (setf (ivy-state-window ivy-last)
           ;   ;(select-window (split-window nil nil side))
           ;   (selected-window)
           ;   )      ; so that with-ivy-window (used a lot inside predefined actions) will use the new window
           ;  (balance-windows)
           ;  (funcall current-act x)
           ;  ))

        (ivy-exit-with-action current-act))
      )
    )
)

(after! ivy-posframe
  (setq ivy-posframe-parameters `((min-width . 90)
                                  (min-height . 16)
                                  (internal-border-width . 10)
                                  (internal-border-width . 10)
                                  (foreground-color . "#00afef")
                                  ))
  (set-face-attribute 'ivy-current-match nil :underline t)
  )

(def-package! lsp-mode :defer t)

(def-package! lsp-ui
  :demand t
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq
   lsp-ui-sideline-enable nil  ; too much noise
   lsp-ui-doc-include-signature t
   lsp-ui-peek-expand-function (lambda (xs) (mapcar #'car xs)))
  (when (featurep! :ui doom)
    (setq
     lsp-ui-doc-background (doom-color 'base4)
     lsp-ui-doc-border (doom-color 'fg)))
  )

(def-package! ccls
  :commands lsp-ccls-enable
  :init (add-hook 'c-mode-common-hook #'lsp-ccls-enable)
  :config
  (setq ccls-executable "/usr/bin/ccls")
  (setq ccls-sem-highlight-method 'nil)
  (setq ccls-extra-args '("--log-file=/tmp/ccls.log"))
  (setq ccls-extra-init-params
        '(:completion (:detailedLabel t)
                      :diagnostics (:frequencyMs 5000)))

  (with-eval-after-load 'projectile
    (add-to-list 'projectile-globally-ignored-directories ".ccls-cache"))

  (evil-set-initial-state 'ccls-tree-mode 'emacs)
  (set! :company-backend '(c-mode c++-mode) '(company-lsp))
  )
