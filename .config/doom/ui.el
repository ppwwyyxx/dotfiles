;;; -*- lexical-binding: t; no-byte-compile: t -*-

;; https://emacs.stackexchange.com/questions/28390/quickly-adjusting-text-to-dpi-changes
(defun myself/get-monitor-size ()
  ;; return: resx, sizex
  (let* ((attrs (frame-monitor-attributes (selected-frame)))
         (size (assoc 'mm-size attrs))  ; mm-size x y
         (sizex (nth 1 size))
         (res (assoc 'geometry attrs))  ; geometry x y w h
         (resx (nth 3 res)))
    (list resx sizex)))

(defun myself/preferred-font-size ()
  (let* ((res-size (myself/get-monitor-size))
         (res (car res-size))
         (size (or (cadr res-size) 100))  ;; size empty in terminal
        )
    (cond
     ((< size 500) 20) ;; small screen
     ((> size 1000) 30)  ;; large screen, unknown
     (t
      (+ 13 (/ (/ (* res res) size) 1300))
      ))
    ))

(setq doom-font (font-spec :family "Monospace" :size (myself/preferred-font-size)))

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
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-show-hidden-files nil)
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
    ))

(after! ivy-posframe
  (setq ivy-posframe-parameters `((min-height . 16)
                                  (internal-border-width . 10)
                                  (internal-border-width . 10)
                                  (foreground-color . "#00afef")
                                  ))
  (set-face-attribute 'ivy-current-match nil :underline t)

  (defun reset-posframe-size(frame)
    ;; TODO use curr-width-4, but frame can be the pos-frame itself
    (let ((curr-width (frame-width frame)))
      (setq ivy-posframe-width (min curr-width 90))
      (setq ivy-posframe-min-width (min curr-width 90))
      )
    )
  (add-to-list 'window-size-change-functions 'reset-posframe-size)
  )

;; modeline
(after! doom-modeline
  (setq +modeline-bar-at-end t)
  (defface my/modeline-state-normal `((t (:inherit doom-modeline-panel :background "#AFD700")))
    "" :group '+modeilne)
  (defface my/modeline-state-insert `((t (:inherit doom-modeline-panel :background "#00875F")))
    "" :group '+modeilne)
  (defface my/modeline-state-visual `((t (:inherit doom-modeline-panel :background "#F7AA52")))
    "" :group '+modeilne)

  (defun +modeline-evil-state ()
    (let ((name-and-face
           (pcase evil-state
             ((or 'normal 'operator) '(" N " 'my/modeline-state-normal))
             ('insert '(" I " 'my/modeline-state-insert))
             ('visual '(" V " 'my/modeline-state-visual))
             (- `(,(upcase (format " %s " evil-state)) 'my/modeline-state-normal))
             )))
      (propertize (car name-and-face) 'face (cadr name-and-face))
      ))

  (doom-modeline-def-segment my/modeline-state
    "Override doom internal modeline segments"
    (if (eq (selected-window) doom-modeline-current-window)
        (+modeline-evil-state)
      nil)
    )
;;    (let ((meta (concat (+modeline--macro-recording)
;;                        (+modeline--anzu)
;;                        (+modeline--evil-substitute)
;;                        (+modeline--iedit))))
;;      (or (and (not (equal meta "")) meta)
;;          (if (eq (selected-window) +modeline-current-window)
;;              (+modeline-evil-state)
;;            nil
;;            ))))

  (doom-modeline-def-modeline 'main
    '(my/modeline-state matches buffer-info remote-host buffer-position selection-info)
    '(misc-info persp-name irc mu4e github debug input-method lsp major-mode process vcs checker))
  )

(after! highlight-indent-guides
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

(after! ivy-posframe
  (set-face-attribute 'internal-border nil :background "light slate blue")
  (setq ivy-posframe-parameters
      '((left-fringe . 0)
        (right-fringe . 0)
        ))
)

;; Local Variables:
;; eval: (rainbow-mode)
;; End:
