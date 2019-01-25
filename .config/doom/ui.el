;;; -*- lexical-binding: t; no-byte-compile: t -*-

;; https://emacs.stackexchange.com/questions/28390/quickly-adjusting-text-to-dpi-changes
(defun myself/get-dpi ()
  (let* ((attrs (car (display-monitor-attributes-list)))
         (size (assoc 'mm-size attrs))
         (sizex (cadr size))
         (res (cdr (assoc 'geometry attrs)))
         (resx (- (caddr res) (car res)))
         dpi)
    (catch 'exit
      ;; in terminal
      (unless sizex
        (throw 'exit 10))
      ;; on big screen
      (when (> sizex 1000)
        (throw 'exit 10))
      ;; DPI
      (* (/ (float resx) sizex) 25.4))))

(defun myself/preferred-font-size ()
  (let ( (dpi (myself/get-dpi)) )
  (cond
    ((< dpi 110) 20)
    ((< dpi 130) 24)
    (t 28))))

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
(when (featurep! :ui modeline)
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

  (def-modeline-segment! +modeline-my-state
    "Override doom internal modeline segments"
    (let ((meta (concat (+modeline--macro-recording)
                        (+modeline--anzu)
                        (+modeline--evil-substitute)
                        (+modeline--iedit))))
      (or (and (not (equal meta "")) meta)
          (if (eq (selected-window) +modeline-current-window)
              (+modeline-evil-state)
            nil
            ))))

  (def-modeline-format! :myown
    '(+modeline-my-state
      " "
      +modeline-buffer-state
      +modeline-buffer-id
      "  %2l:%c %p  "
      +modeline-selection-info)
    '(+modeline-encoding
      +modeline-major-mode
      " "
      +mode-line-misc-info
      (vc-mode (" " +modeline-vcs " "))
      mode-line-process
      +modeline-flycheck))
  (set-modeline! :myown t))

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
