;;; -*- lexical-binding: t; no-byte-compile: t -*-

(after! quickrun
  (setq quickrun-timeout-seconds 30)
  (quickrun-add-command "c++/c1z"
    '((:command . "g++")
      (:exec    . ("%c -std=c++1z %o -o %e -Wall -Wextra %s"
                   "%e %a"))
      (:remove  . ("%e")))
    :default "c++"))

(def-package! zeal-at-point
  :when (and IS-LINUX (display-graphic-p))
  :defer t)

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
  (set-company-backend! '(c-mode c++-mode) 'company-lsp))

(defconst my/cc-style
  '("doom" (c-offsets-alist . ((innamespace . [0])))))
(c-add-style "my-cc-mode" my/cc-style)

(def-package! protobuf-mode
  :defer t
  :init
  (add-hook 'protobuf-mode-hook #'flycheck-mode))

(after! python
  (defun spacemacs/python-annotate-debug ()
    "Highlight debug lines. Modified from spacemacs."
    (interactive)
    (highlight-lines-matching-regexp "import \\(pdb\\|ipdb\\|pudb\\|wdb\\)")
    (highlight-lines-matching-regexp "\\(pdb\\|ipdb\\|pudb\\|wdb\\).set_trace()")
    (highlight-lines-matching-regexp "import IPython")
    (highlight-lines-matching-regexp "import sys; sys.exit"))
  (add-hook 'python-mode-hook #'spacemacs/python-annotate-debug)
  (add-hook 'python-mode-hook #'highlight-indent-guides-mode)
  (add-hook 'python-mode-hook
            (lambda () (setq tab-width 4 fill-column 120)))
  (setq flycheck-flake8-maximum-line-length 120)

  (defun buffer-contains-substring (string)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (search-forward string nil t))))

  (defun my/python-docset-to-use()
    (cond
     ((or (s-contains? "tensorpack" (buffer-file-name)) (buffer-contains-substring "tensorpack"))
      (setq zeal-at-point-docset '("python3" "numpy" "tensorpack" "TensorFlow")))
     ((buffer-contains-substring "tensorflow")
      (setq zeal-at-point-docset '("python3" "numpy" "TensorFlow")))
     (t (setq zeal-at-point-docset '("python3" "numpy")))
     ;; TODO complete zeal-at-po
     ))
  (add-hook 'python-mode-hook #'my/python-docset-to-use)

  (when (executable-find "ipython")
    ;; my fancy ipython prompt
    (setq python-shell-prompt-regexp "╭─.*\n.*╰─\\$ "
          python-shell-prompt-block-regexp "\\.\\.\\.: ")
    )
  )

(defun my/apply-conf-after-save()
  (let* ((filename (buffer-file-name))
         (basename (file-name-nondirectory filename)))
    (pcase basename
      (".tmux.conf" (call-process "tmux" nil nil nil "source" filename))
      (".Xresources" (call-process "xrdb" nil nil nil filename))
      (".xbindkeysrc" (call-process-shell-command "killall -HUP xbindkeys"))
      )))
(add-hook 'after-save-hook #'my/apply-conf-after-save)
(add-to-list 'auto-mode-alist '("\\..*rc\(\\.local\)?\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("/[^\\.]*rc\\'" . conf-mode))

(defun display-ansi-colors ()
  (interactive)
  (require 'tty-format)
  (format-decode-buffer 'ansi-colors))
(add-to-list 'auto-mode-alist '("\\.log\\'" . display-ansi-colors))

;; ielm history: https://emacs.stackexchange.com/questions/4221/remembering-history-between-sessions-in-inferior-emacs-lisp-mode
;; global copy of the buffer-local variable
(after! ielm
  (defvar ielm-comint-input-ring nil)

  (defun set-ielm-comint-input-ring ()
    (setq-local comint-input-ring-size 200)
    (add-hook 'kill-buffer-hook #'save-ielm-comint-input-ring nil t)
    ;; restore saved value (if available)
    (when ielm-comint-input-ring
      (message "Restoring comint-input-ring for ielm ...")
      (setq comint-input-ring ielm-comint-input-ring)))

  (defun save-ielm-comint-input-ring ()
    (message "Saving comint-input-ring for ielm ...")
    (setq ielm-comint-input-ring comint-input-ring))

  (add-hook 'inferior-emacs-lisp-mode-hook #'set-ielm-comint-input-ring)
  (add-to-list 'savehist-additional-variables 'ielm-comint-input-ring))
