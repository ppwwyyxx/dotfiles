;;; -*- lexical-binding: t; -*-
;;; -*- no-byte-compile: t -*-

(setq doom-font (font-spec :family "Monospace" :size 20))

(after! neotree
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  )
(after! doom-themes
  ;; https://github.com/hlissner/emacs-doom-themes/issues/180
  (remove-hook 'doom-load-theme-hook #'doom-themes-neotree-config))

(after! evil-goggles
  (setq
   evil-goggles-enable-change nil
   evil-goggles-enable-delete t
   evil-goggles-pulse nil
   evil-goggles-duration 0.02
   )
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


