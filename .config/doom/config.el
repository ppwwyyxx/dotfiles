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
  (select-window (split-window nil nil side))
  (balance-windows)
  (switch-to-buffer buffer-or-name))

(defun find-file-new-window (filename)
  ;(switch-to-buffer-new-window (find-file-noselect (projectile-expand-root filename)) 'right)
  (switch-to-buffer-new-window (find-file-noselect filename) 'right)
)

(after! ivy
    (defun ivy-exit-buffer-with-new-window (side)
		(ivy-exit-with-action
          (lambda (buffer-or-name) (switch-to-buffer-new-window buffer-or-name side)))
	)

	(defun ivy-exit-file-with-new-window (side)
		(ivy-exit-with-action
          (lambda (filename) (switch-to-buffer-new-window (find-file-noselect filename) side))
        )
	)

    (defun ivy-exit-map-test ()
      (interactive)
      (let ((find-file-other-window 'find-file-new-window))
	    (ivy-exit-with-action 'counsel-projectile-find-file-action-other-window)
      )
      ;(ivy-set-action 'ivy-exit-file-action-test)
      ;(message (format "Act=%s" (ivy-state-action ivy-last)))
      ;(ivy-set-action 'find-file-other-window)
      ;(let ((find-file-other-window 'find-file-new-window))
      ;  (ivy-done))
    )
	(define-key ivy-switch-buffer-map (kbd "C-v") (lambda! (ivy-exit-buffer-with-new-window 'right)))
	(define-key ivy-switch-buffer-map (kbd "C-s") (lambda! (ivy-exit-buffer-with-new-window 'below)))
	(define-key ivy-switch-buffer-map (kbd "C-x") (lambda! (ivy-exit-buffer-with-new-window 'below)))
	;(define-key ivy-minibuffer-map (kbd "C-v") (lambda! (ivy-exit-file-with-new-window 'right)))
	(define-key ivy-minibuffer-map (kbd "C-v") 'ivy-exit-map-test)
	(define-key ivy-minibuffer-map (kbd "C-s") (lambda! (ivy-exit-file-with-new-window 'below)))
	(define-key ivy-minibuffer-map (kbd "C-x") (lambda! (ivy-exit-file-with-new-window 'below)))
)

(setq doom-font (font-spec :family "Monospace" :size 20))
(set-face-attribute 'line-number-current-line nil :inherit 'default)
;; https://github.com/hlissner/emacs-doom-themes/blob/master/doom-themes-common.el#L67-L70
