;;; -*- lexical-binding: t; no-byte-compile: t -*-

(add-to-list 'exec-path (substitute-in-file-name "$HOME/bin"))

(load! "+ui")
(load! "+bindings")
(load! "+evil-commands")

(add-hook 'prog-mode-hook #'doom|enable-delete-trailing-whitespace)
(add-hook 'prog-mode-hook #'goto-address-prog-mode)
(add-hook 'ielm-mode-hook #'visual-line-mode)
(add-hook 'eshell-mode-hook #'visual-line-mode)
(add-hook 'markdown-mode-hook #'visual-line-mode)

(setq tab-width 4)
(setq tab-always-indent nil)
(setq tramp-default-method "ssh")

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
                                        ;(null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
         (null (string-match "\\([;{}]\\)"
                             (thing-at-point 'line)))))
  )

(after! imenu-list
  (setq imenu-list-auto-resize nil))

(after! projectile
  (setq projectile-require-project-root t)
  (projectile-cleanup-known-projects))

(after! quickrun
  (setq quickrun-timeout-seconds 30)
  (quickrun-add-command "c++/c1z"
    '((:command . "g++")
      (:exec    . ("%c -std=c++1z %o -o %e -Wall -Wextra %s"
                   "%e %a"))
      (:remove  . ("%e")))
    :default "c++"))

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


(defun my/apply-conf-after-save()
  (let* ((filename (buffer-file-name))
        (basename (file-name-nondirectory filename)))
    (pcase basename
      (".tmux.conf" (call-process "tmux" nil nil nil "source" filename))
      (".Xresources" (call-process "xrdb" nil nil nil filename))
      (".xbindkeysrc" (call-process-shell-command "killall -HUP xbindkeys"))
      )))
(add-hook 'after-save-hook #'my/apply-conf-after-save)
(add-to-list 'auto-mode-alist '(".xbindkeysrc" . conf-mode))

;; Programming stuff ...
(def-package! lsp-mode :defer t)

(def-package! lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq
   lsp-ui-sideline-enable nil  ; too much noise
   lsp-ui-doc-include-signature t
   lsp-ui-peek-expand-function (lambda (xs) (mapcar #'car xs)))
  (when (featurep! :ui doom)
    (setq
     lsp-ui-doc-background (doom-color 'base4)
     lsp-ui-doc-border (doom-color 'fg))))

(def-package! ccls
  :hook (c-mode-common . lsp-ccls-enable)
  :when (executable-find "ccls")
  :config
  (setq ccls-executable (executable-find "ccls"))
  (setq ccls-sem-highlight-method nil)
  (setq ccls-extra-args '("--log-file=/tmp/ccls.log"))
  (setq ccls-extra-init-params
        '(:completion (:detailedLabel t)
                      :diagnostics (:frequencyMs 5000)))

  (with-eval-after-load 'projectile
    (add-to-list 'projectile-globally-ignored-directories ".ccls-cache"))

  (evil-set-initial-state 'ccls-tree-mode 'emacs)
  (set! :company-backend '(c-mode c++-mode) '(company-lsp)))

(after! python
  (defun spacemacs/python-annotate-debug ()
    "Highlight debug lines. Copied from spacemacs."
    (interactive)
    (highlight-lines-matching-regexp "import \\(pdb\\|ipdb\\|pudb\\|wdb\\)")
    (highlight-lines-matching-regexp "\\(pdb\\|ipdb\\|pudb\\|wdb\\).set_trace()")
    (highlight-lines-matching-regexp "import IPython")
    (highlight-lines-matching-regexp "import sys; sys.exit"))
  (add-hook 'python-mode-hook #'spacemacs/python-annotate-debug)
  (add-hook 'python-mode-hook #'highlight-indent-guides-mode))


(defun display-ansi-colors ()
  (interactive)
  (require 'tty-format)
  (format-decode-buffer 'ansi-colors))
(add-to-list 'auto-mode-alist '("\\.log\\'" . display-ansi-colors))


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
