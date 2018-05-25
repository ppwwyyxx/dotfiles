;;; -*- lexical-binding: t; no-byte-compile: t -*-

(setq doom-font (font-spec :family "Monospace" :size 20))

(unless (display-graphic-p)
  (custom-set-faces
   '(region ((t (:background "#5CC8ED" :foreground "black"))))))

(after! neotree
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  )
(after! doom-themes
  ;; https://github.com/hlissner/emacs-doom-themes/issues/180
  (remove-hook 'doom-load-theme-hook #'doom-themes-neotree-config))

(after! evil-goggles
  (if (featurep! :ui doom)
      (custom-set-faces
       '(evil-goggles-delete-face           ((t (:inherit diff-refine-removed))))
       '(evil-goggles-change-face           ((t (:inherit diff-refine-removed))))
       '(evil-goggles-paste-face            ((t (:inherit diff-refine-added))))
       '(evil-goggles-yank-face             ((t (:inherit diff-refine-changed))))
       '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-refine-removed))))
       '(evil-goggles-undo-redo-add-face    ((t (:inherit diff-refine-added))))
       '(evil-goggles-undo-redo-change-face ((t (:inherit diff-refine-changed)))))
    (evil-goggles-use-diff-faces)
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

;; modeline
(defface my/modeline-state-normal `((t (:inherit doom-modeline-panel :background "#AFD700")))
  "" :group '+doom-modeilne)
(defface my/modeline-state-insert `((t (:inherit doom-modeline-panel :background "#00875F")))
  "" :group '+doom-modeilne)
(defface my/modeline-state-visual `((t (:inherit doom-modeline-panel :background "#F7AA52")))
  "" :group '+doom-modeilne)

(defun +doom-modeline-evil-state ()
 (let ((name-and-face
        (pcase evil-state
            ('normal '(" N " 'my/modeline-state-normal))
            ('insert '(" I " 'my/modeline-state-insert))
            ('visual '(" V " 'my/modeline-state-visual))
            (- `(,(upcase (format " %s " evil-state)) 'my/modeline-state-normal))
            )))
   (propertize (car name-and-face) 'face (cadr name-and-face))
 ))

(def-modeline-segment! my-state
  "Override doom internal modeline segments"
  (let ((meta (concat (+doom-modeline--macro-recording)
                      (+doom-modeline--anzu)
                      (+doom-modeline--evil-substitute)
                      (+doom-modeline--iedit))))
     (or (and (not (equal meta "")) meta)
         (+doom-modeline-evil-state)
         )))

(def-modeline! main
  (my-state " " buffer-info "  %l:%c %p  " selection-info)
  (buffer-encoding major-mode vcs flycheck))


(after! python
  (defun spacemacs/python-annotate-debug ()
    "Highlight debug lines. Copied from spacemacs."
    (interactive)
    (highlight-lines-matching-regexp "import \\(pdb\\|ipdb\\|pudb\\|wdb\\)")
    (highlight-lines-matching-regexp "\\(pdb\\|ipdb\\|pudb\\|wdb\\).set_trace()")
    (highlight-lines-matching-regexp "import IPython")
    (highlight-lines-matching-regexp "import sys; sys.exit"))
  (add-hook 'python-mode-hook #'spacemacs/python-annotate-debug)
	)

;; Local Variables:
;; eval: (rainbow-mode)
;; End:
