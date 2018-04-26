;;; -*- lexical-binding: t -*-
;;; -*- no-byte-compile: t -*-
;(use-package evil-collection
;  :after evil
;  :custom (evil-collection-setup-minibuffer t)
;  :init (evil-collection-init))


;(def-package-hook! evil
;  :pre-init
;  (setq evil-magic nil)
;  t)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(after! evil
  (evil-ex-define-cmd "vsp" 'evil-window-vsplit)
)

;(ivy-add-actions 'ivy-switch-buffer '(("z" switch-to-buffer-other-window "test")))
;(ivy-add-actions 'counsel-find-file' (("z" switch-to-buffer-other-window "test")))
(defun switch-to-buffer-new-window (buffer-or-name side)
  "Switch to BUFFER-OR-NAME in a new window, based on SIDE."
  ;(sleep-for 5)
  (select-window (split-window nil nil side))
  (balance-windows)
  (switch-to-buffer buffer-or-name)
  )

(defun find-file-new-window (filename side)
  (switch-to-buffer-new-window (find-file-noselect filename) side))

;(load "/home/yuxinwu/test")

(after! ivy
    (defun my/projectile-find-file-new-window (filename side)
      (find-file-new-window (projectile-expand-root filename) side)
      (run-hooks 'projectile-find-file-hook)
    )

    (defvar my/ivy-new-window-action-map (make-hash-table)
    "Map from ivy actions (symbols) to actions which take an extra side argument.")
    (puthash 'counsel-projectile-find-file-action 'my/projectile-find-file-new-window my/ivy-new-window-action-map)
    ;(puthash 'counsel-projectile-find-file-action 'newfunc my/ivy-new-window-action-map)
    (puthash 'ivy--switch-buffer-action 'switch-to-buffer-new-window my/ivy-new-window-action-map)
    ;(puthash 'counsel-git-grep-action 'newfunc my/ivy-new-window-action-map)

    (defun my/ivy-exit-new-window (side)
      (let ((current-act (ivy--get-action ivy-last)))
        (message "current-act=%s" (type-of current-act))
        (message "current-act=%s" current-act)
        (cl-flet ((side-action (gethash current-act my/ivy-new-window-action-map
                                        (lambda (x _) (funcall current-act x)))))
            (ivy-exit-with-action (lambda (filename) (side-action filename side)))
        )
      )
    )

	(define-key ivy-minibuffer-map (kbd "C-v") (lambda! (my/ivy-exit-new-window 'right)))
	(define-key ivy-minibuffer-map (kbd "C-s") (lambda! (my/ivy-exit-new-window 'below)))
	(define-key ivy-minibuffer-map (kbd "C-x") (lambda! (my/ivy-exit-new-window 'below)))
)

(setq doom-font (font-spec :family "Monospace" :size 20))
(set-face-attribute 'line-number-current-line nil :inherit 'default)
;; https://github.com/hlissner/emacs-doom-themes/blob/master/doom-themes-common.el#L67-L70
