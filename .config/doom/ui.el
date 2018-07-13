;;; -*- lexical-binding: t; no-byte-compile: t -*-

(setq doom-font (font-spec :family "Monospace" :size 20))

(unless (display-graphic-p)
  (custom-set-faces
   ;; visual selection under terminal
   '(region ((t (:background "#5CC8ED" :foreground "black"))))

   ;; color similar to my vimrc
   '(default ((t (:foreground "#FFFFFF"))))
   '(font-lock-comment-face ((t (:foreground "#2277EE"))))
   '(font-lock-doc-face ((t (:foreground "#FF0117"))))
   '(font-lock-string-face ((t (:foreground "red"))))
   '(font-lock-keyword-face ((t (:foreground "yellow"))))
   '(font-lock-function-name-face ((t (:foreground "cyan"))))
   )
  )

(after! neotree
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

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
    ))

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
(when (featurep! :ui doom-modeline)
  (defface my/modeline-state-normal `((t (:inherit doom-modeline-panel :background "#AFD700")))
    "" :group '+doom-modeilne)
  (defface my/modeline-state-insert `((t (:inherit doom-modeline-panel :background "#00875F")))
    "" :group '+doom-modeilne)
  (defface my/modeline-state-visual `((t (:inherit doom-modeline-panel :background "#F7AA52")))
    "" :group '+doom-modeilne)

  (defun +doom-modeline-evil-state ()
    (let ((name-and-face
           (pcase evil-state
             ((or 'normal 'operator) '(" N " 'my/modeline-state-normal))
             ('insert '(" I " 'my/modeline-state-insert))
             ('visual '(" V " 'my/modeline-state-visual))
             (- `(,(upcase (format " %s " evil-state)) 'my/modeline-state-normal))
             )))
      (propertize (car name-and-face) 'face (cadr name-and-face))
      ))

  (def-modeline-segment! +mode-line-my-state
    "Override doom internal modeline segments"
    (let ((meta (concat (+doom-modeline--macro-recording)
                        (+doom-modeline--anzu)
                        (+doom-modeline--evil-substitute)
                        (+doom-modeline--iedit))))
      (or (and (not (equal meta "")) meta)
          (if (eq (selected-window) +doom-modeline-current-window)
              (+doom-modeline-evil-state)
            nil
            ))))

  (def-modeline! :myown
    '(+mode-line-my-state " " +mode-line-buffer-id "  %2l:%c %p  " +mode-line-selection-info)
    '(+mode-line-buffer-encoding +mode-line-major-mode +mode-line-vcs)); flycheck))
  (set-modeline! :myown t)
  )

(def-package! highlight-indent-guides
  :when (display-graphic-p)
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-responsive 'top)
  (setq highlight-indent-guides-auto-enabled nil)
  (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
  (set-face-foreground 'highlight-indent-guides-top-character-face "skyblue"))

;; copied from +spacemacs/spacemacs-editing-visual
(def-package! highlight-parentheses
  :hook (prog-mode . highlight-parentheses-mode)
  :init
  (setq hl-paren-delay 0.2)
  (setq hl-paren-colors
    '("SpringGreen3" "IndianRed1" "IndianRed3" "IndianRed4"))
  :config
  (set-face-attribute 'hl-paren-face nil :weight 'bold)
  (custom-set-faces '(show-paren-match ((t (:foreground "SpringGreen1" :underline t)))))
  )

;; Local Variables:
;; eval: (rainbow-mode)
;; End:
