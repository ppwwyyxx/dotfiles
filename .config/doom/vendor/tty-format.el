;;; tty-format.el --- text file backspacing and ANSI SGR as faces

;; Copyright 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2017 Kevin Ryde

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 11
;; Keywords: wp, faces, ansi
;; URL: http://user42.tuxfamily.org/tty-format/index.html
;; EmacsWiki: TtyFormat

;; tty-format.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; tty-format.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This is two additions to `format-alist' for decoding
;;
;;    * ANSI SGR escape sequences "Esc [ ... m" colours, bold, underline,
;;      etc through ansi-color.el.
;;
;;    * Backspace overstriking for bold, underline, overline, and a bullet
;;      "+ backspace o".
;;
;; Such sequences are tty or line printer oriented output, but are sometimes
;; found in text files.  The aim is to make those files viewable and
;; hopefully have the attributes successfully copy into something like
;; `enriched-mode'.
;;
;; There's no automatic detection of these formats but you can "decode" a
;; buffer containing them with
;;
;;     M-x format-decode-buffer backspace-overstrike
;; and/or
;;     M-x format-decode-buffer ansi-colors
;;
;; `format-decode-buffer' has completion when it prompts for a format name.
;;
;; See `tty-format-guess' below for an idea to automatically notice text
;; files using these formats.
;;
;; Groff produces output like this (via grotty), and some of its manuals
;; have both ANSI and backspacing, as do various other packages with text
;; files produced from roff input.  You might think backspacing by now would
;; be as long gone as the teletypes it was made for, but grotty still uses
;; it creatively.
;;
;; Groff actually has lots of character overstrike sequences to make ink
;; resembling non-ASCII characters.  There's far too many to want in the
;; code here -- you're much better off asking groff for extended charset
;; output in the first place (utf8 or whatever), instead of decoding bizarre
;; combinations after the fact.  So the aim here is only to handle bits
;; found in real life documents.  One moderately frequent bit not yet
;; supported is | plus = for a footnote dagger.
;;
;; See also underline.el for a couple of simple functions adding or removing
;; backspace underlining.

;;; Emacsen:

;; Designed for Emacs 21 up.
;; Works in XEmacs 21 but maybe leaves buffer modified.
;;
;; Works in Emacs 20 if you have ansi-color.el.  The separately published
;; ansi-color.el 3.4.5 works if you load cl.el for `mapc'.  But note in
;; Emacs 20 faces don't display on a tty, only under X or similar GUI.

;;; Install:

;; To have M-x format-decode-buffer support the new formats put
;; tty-format.el in one of your `load-path' directories, and in your .emacs
;; add
;;
;;     (require 'tty-format)
;;
;; If you want to try automatic detection on .txt files then add
;;
;;     (add-hook 'find-file-hooks 'tty-format-guess)
;;
;; It's also possible to add the `format-alist' entries and then autoload
;; the functions so the code loads only when used.  There's ;;;###autoload
;; cookies doing this if you install via `M-x package-install' or know how
;; to use `update-file-autoloads'.

;;; History:

;; Version 1 - the first version
;; Version 2 - call the format `ansi-colors' for clarity
;; Version 3 - add unicode U+203E overline
;; Version 4 - fix for re-matching multi-backspace sequences
;; Version 5 - autoload the format-alist additions, not whole file
;; Version 6 - autoload the encode too, for an unload-feature while in use
;; Version 7 - decimal char bytes for emacs20
;; Version 8 - use ansi-color-apply-face-function when available
;; Version 9 - comments of explicit M-x format-decode-buffer
;; Version 10 - new email
;; Version 11 - compile-time decode-char for overline

;;; Code:

(require 'ansi-color)
(eval-when-compile
  (unless (fboundp 'ignore-errors)
    (require 'cl))) ;; for `ignore-errors'

;;-----------------------------------------------------------------------------
;; compatibility

;; not in xemacs, quieten its byte compiler
(defvar ansi-color-apply-face-function)


;;-----------------------------------------------------------------------------
;; faces

;; As of Emacs 23.4 there's no builtin `overline' face, unlike say `bold' or
;; `underline', so define one here.  It comes out nicely on X but dunno what
;; sort of fallback would be good on a tty.  Usually overline is just groff
;; trying to draw a box, so if it doesn't display then it doesn't matter much.
;;
;; XEmacs 21 doesn't support :overline in defface and will throw an error on
;; attempting it.  Known defface attributes are in 'custom-face-attributes',
;; which is pre-loaded in emacs but in xemacs21 must get it from
;; cus-face.el.  defface uses `custom-define-face' from cus-face.el anyway,
;; so loading it doesn't drag in anything extra.
;;
(or (ignore-errors
      ;; emacs21 and emacs22
      (defface tty-format-overline
        '((t
           (:overline t)))
        "An overline face.
Used by buffer-format `backspace-overstrike' for overlining."
        :group 'faces  ;; in absense of our own group
        :link  '(url-link :tag "tty-format.el home page"
                          "http://user42.tuxfamily.org/tty-format/index.html"))
      t)

    ;; xemacs21
    (defface tty-format-overline
      '((t))
      "An overline face.
Used by buffer-format `backspace-overstrike' for overlining.

It seems your Emacs doesn't support :overline, so the default
here is a is a do-nothing face."
      :group 'faces ;; in absense of our own group
      :link  '(url-link :tag "tty-format.el home page"
                        "http://user42.tuxfamily.org/tty-format/index.html")))

(defun tty-format-add-faces (face-list beg end)
  "Add FACE-LIST to the region between BEG and END.
FACE-LIST is a list of faces.  These faces are merged onto any
existing `face' property by adding any of FACE-LIST not already
there.  If no existing face property then FACE-LIST is stored as
the face property value.

If it happens that some of the region already has all of
FACE-LIST then those parts are not changed at all.

Faces are compared with `equal' so face names accumulate by name
even if some of them might look the same on screen."

  (when face-list
    (while (< beg end)
      (let ((part-end   (next-single-property-change beg 'face nil end))
            (part-faces (get-text-property beg 'face)))

        (cond ((not part-faces)
               ;; nothing currently, stick in our FACE-LIST directly
               (setq part-faces face-list))

              ((symbolp part-faces)
               ;; single face symbol currently, merge with FACE-LIST
               (if (memq part-faces face-list)
                   (setq part-faces face-list)
                 (setq part-faces (cons part-faces face-list))))

              (t
               ;; list of faces currently, adjoin FACE-LIST
               (dolist (face face-list)
                 (unless (member face part-faces)
                   (setq part-faces (cons face part-faces))))))

        ;; single symbol list -> symbol
        (and (symbolp (car part-faces))
             (not (cdr part-faces))
             (setq part-faces (car part-faces)))

        (put-text-property beg part-end 'face part-faces)
        (setq beg part-end)))))


;;-----------------------------------------------------------------------------
;; ansi sgr, via ansi-color.el

;;;###autoload
(add-to-list 'format-alist
             '(ansi-colors
               "ANSI SGR escape sequence colours and fonts."
               nil  ;; no automatic detection
               ansi-format-decode
               ansi-format-encode
               t
               nil))

;;;###autoload
(defun ansi-format-encode (beg end buffer)
  ;; checkdoc-params: (beg end buffer)
  "Sorry, cannot encode `ansi-colors' format.
This function is designed for use in `format-alist'.

There's no support for re-encoding to save a file in
`ansi-colors' format.  (But of course you can copy into another
document with a format that does support saving.)"
  (error "Sorry, `ansi-colors' format is read-only"))

;;;###autoload
(defun ansi-format-decode (beg end)
  "Decode ANSI SGR control sequences between BEG and END into faces.
This function is designed for use in `format-alist'.

ANSI standard \"Esc [ ... m\" terminal control sequences are
turned into corresponding Emacs faces, using `ansi-colours'.

There's no automatic detection of this format, because those
escape sequences could too easily occur in unrelated binary data.
Decode files with an explicit \\[format-decode-buffer], or see
`tty-format-guess' to try automated guessing on text files."

  (let ((inhibit-read-only t))  ;; if visiting a read-only file
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)

        ;; ansi-color.el of emacs24.3 up has
        ;; `ansi-color-apply-face-function' which can be used to apply faces
        ;; as properties instead of the default overlays.
        (if (boundp 'ansi-color-apply-face-function)
            (let ((ansi-color-apply-face-function
                   (lambda (beg end face)
                     (tty-format-add-faces (list face) beg end))))
              (setq ansi-color-context-region nil)
              (ansi-color-apply-on-region (point-min) (point-max)))

          ;; For previous ansi-color.el, this is like
          ;; `ansi-color-apply-on-region', but using text properties instead
          ;; of overlays.  And it's like `ansi-color-apply', but operating
          ;; on a buffer instead of a string.  Don't want to just put
          ;; `buffer-string' through `ansi-color-apply' because that would
          ;; lose marker positions, and also as of Emacs 22
          ;; `ansi-color-apply' is slow on big input due to a lot of string
          ;; copying.
          (goto-char (point-min))
          (let ((face-list nil)
                (start     (point-min))
                escape-sequence)
            (while (re-search-forward ansi-color-regexp nil t)
              (setq escape-sequence (match-string 1))
              (delete-region (match-beginning 0) (match-end 0))
              (tty-format-add-faces face-list start (point))
              (setq start (point))
              (setq face-list
                    (ansi-color-apply-sequence escape-sequence face-list)))
            ;; remainder of buffer in final face
            (tty-format-add-faces face-list start (point-max))))

        (point-max)))))


;;-----------------------------------------------------------------------------
;; backspace overstrike

;;;###autoload
(add-to-list 'format-alist
             '(backspace-overstrike
               "Backspace overstriking for bold and underline."
               nil  ;; no automatic detection
               backspace-overstrike-decode
               backspace-overstrike-encode
               t
               nil))

;;;###autoload
(defun backspace-overstrike-encode (beg end buffer)
  ;; checkdoc-params: (beg end buffer)
  "Sorry, cannot encode `backspace-overstrike' format.
This function is designed for use in `format-alist'.

There's no support for re-encoding to save a file in
`backspace-overstrike' format.  (But of course you can copy into
another document with a format that does support saving.)"
  (error "Sorry, `backspace-overstrike' format is read-only"))

;;;###autoload
(defun backspace-overstrike-decode (beg end)
  "Decode backspace overstrike sequences between BEG and END into faces.
This function is designed for use in `format-alist'.

The sequences recognised are:

    X backspace X       -- bold
    _ backspace X       -- underline
    U+203E backspace X  -- overline (when unicode available)
    + backspace o       -- bullet point (latin-1 middle dot,
                           as per groff \\=\\[bu]])

Character overstriking like this was used in the past on line
printers and is still sometimes found in text files.

There's no automatic detection of this format in `format-alist',
because backspace sequences could too easily occur in unrelated
binary data.  Decode with an explicit \\[format-decode-buffer] or
see `tty-format-guess' to try automated guessing on text files."

  (let* ((inhibit-read-only t)    ;; if visiting a read-only file
         (case-fold-search nil)   ;; don't match x\bX

         ;; string of U+203E overline character
         (overline (or (eval-when-compile
                         (and (fboundp 'decode-char) ;; emacs21 up
                              (string (decode-char 'ucs 8254))))
                       (and (memq 'utf-8 (coding-system-list))
                            ;; xemacs21 doesn't have `coding-system-p' so use
                            ;; `coding-system-list', and it only has utf-8
                            ;; anyway with mule-ucs
                            (decode-coding-string
                             (eval-when-compile
                               (let ((str (string 226 128 190)))
                                 (if (fboundp 'string-make-unibyte)
                                     (string-make-unibyte str) ;; emacs
                                   str)))                      ;; xemacs
                             'utf-8))))

         (overline-regexp1 (and overline (concat overline "\b")))
         (overline-regexp2 (and overline (concat "[^\b]\\(\b[^\b_]\\)*?\\(\b"
                                                 overline "\\)")))
         (end-marker       (make-marker))
         (face-idx         0))

    (save-excursion
      (save-restriction
        (narrow-to-region beg end)

        ;; If you think the approach here looks overcomplicated, well, most
        ;; of the time you'd be right.  But the idea is to cope reasonably
        ;; gracefully with backspacing that's only partly understood,
        ;; ie. recognise and remove effects we know, and leave the rest as
        ;; ^H's in the buffer.
        ;;
        ;; For speed it might be desirable to match simple runs of just bold
        ;; or just underline, to avoid crunching and propertizing every
        ;; character individually.  But that can wait unless/until the
        ;; general approach is not fast enough.

        (goto-char (point-min))
        (while (re-search-forward "[^\b]\b[^\b]\\(\b[^\b]\\)*" nil t)
          ;; each run of backspacing
          (goto-char (match-beginning 0))
          (set-marker end-marker (match-end 0))
          (setq face-idx 0)

          ;; The sequence "_\b_" is ambiguous: is it a bold underscore, or
          ;; an underlined underscore?  Both are probable, and groff gives
          ;; that output for both requests.  For now make it bold, since
          ;; that's how it'd mostly look on a line printer.  Thus crunch for
          ;; bold before crunching underline.
          ;;
          ;; "+\bo" bullet can be boldened by doubling both the + and o, so
          ;; also crunch bold before looking for that combination.

          ;; any duplicate chars in the run mean bold
          (while (looking-at "\\([^\b]\b\\)*?\\([^\b]\\)\b\\([^\b]\b\\)*?\\2")
            (delete-region (match-beginning 2) (+ 2 (match-beginning 2)))
            (setq face-idx (logior face-idx 1)))

          ;; any "_" in the run means underline
          (while (looking-at "_\b")
            (delete-region (match-beginning 0) (match-end 0))
            (setq face-idx (logior face-idx 2)))
          (while (looking-at "[^\b]\\(\b[^\b_]\\)*?\\(\b_\\)")
            (delete-region (match-beginning 2) (match-end 2))
            (setq face-idx (logior face-idx 2)))

          ;; any unicode U+203E in the run means overline
          (when overline
            (while (looking-at overline-regexp1)
              (delete-region (match-beginning 0) (match-end 0))
              (setq face-idx (logior face-idx 4)))
            (while (looking-at overline-regexp2)
              (delete-region (match-beginning 2) (match-end 2))
              (setq face-idx (logior face-idx 4))))

          ;; "+" and "o" turns into latin1 #xB7 "middle dot".
          ;;
          ;; "+\bo" is what groff gives for bullet, and a middle dot in turn
          ;; is what groff gives in latin1 (or utf8) output mode.  Suspect a
          ;; heavier mark like U+2022 would look better, but would want to
          ;; be sure that's displayable.  Also it might not degrade
          ;; gracefully if copied to another buffer and saved into a
          ;; non-unicode file.
          ;;
          (when (looking-at "\\([^\b]\b\\)*?\\(\\+\\)\\(\b[^\b]\\)*?\\(\bo\\)")
            (let* ((opos  (1- (match-end 0)))
                   (props (text-properties-at opos)))
              ;; insert before delete to keep end-marker at the right spot
              ;; xemacs21 `replace-match' doesn't take a subexpr, otherwise
              ;; it might be used here
              (save-excursion (goto-char opos) (insert "·"))
              (set-text-properties opos (1+ opos) props)
              (delete-region (1+ opos) (+ 2 opos)) ;; "o"
              (delete-region (match-beginning 2)   ;; "+\b"
                             (+ 2 (match-beginning 2)))))
          (tty-format-add-faces (aref [nil
                                       (bold)
                                       (underline)
                                       (bold underline)
                                       (tty-format-overline)
                                       (bold tty-format-overline)
                                       (underline tty-format-overline)
                                       (bold underline tty-format-overline)]
                                      face-idx)
                                (point) (marker-position end-marker))
          (goto-char end-marker))

        (set-marker end-marker nil) ;; nowhere
        (point-max)))))


;;-----------------------------------------------------------------------------
;; text file guessing

;;;###autoload
(defun tty-format-guess ()
  "Decode text files containing ANSI SGR or backspace sequences.
This is designed for use from `find-file-hook' (or
`find-file-hooks').

If the buffer filename is \".txt\" or \"README\" and there's any
ANSI SGR escapes or backspace overstriking then call
`format-decode-buffer' to decode with `ansi-colors' and/or
`backspace-overstrike' formats respectively.

It'd be too dangerous to look at every file for escape and
backspace sequences, they could too easily occur in binary data
like an image file.  The idea of this function is to check just
text files, presuming you're confident all \".txt\" files should
be ordinary text.

If you normally use this guess but found it didn't notice then
remember the formats can always be decoded explicitly with

    \\[format-decode-buffer] backspace-overstrike
and/or
    \\[format-decode-buffer] ansi-colors"

  (let ((filename (buffer-file-name)))
    (when filename
      (when (and (featurep 'jka-compr)
                 (jka-compr-installed-p))
        (setq filename (jka-compr-byte-compiler-base-file-name filename)))

      (when (let ((case-fold-search t))
              (or (string-match "\\.txt\\'"  filename)
                  (string-match "/README\\'" filename)))

        (if (save-excursion
              (goto-char (point-min))
              (re-search-forward "[^\b]\b[^\b]" nil t))
            (format-decode-buffer 'backspace-overstrike))

        (if (save-excursion
              (goto-char (point-min))
              (re-search-forward ansi-color-regexp nil t))
            (format-decode-buffer 'ansi-colors))))))

;; emacs  21 - find-file-hooks is a defvar
;; xemacs 21 - find-file-hooks is a defcustom, give custom-add-option
;; emacs  22 - find-file-hooks becomes an alias for find-file-hook, and the
;;             latter is a defcustom, give custom-add-option on that
;;
;;;###autoload
(if (eval-when-compile (boundp 'find-file-hook))
    (custom-add-option 'find-file-hook 'tty-format-guess) ;; emacs22
  (custom-add-option 'find-file-hooks 'tty-format-guess)) ;; xemacs21


;; LocalWords: Esc color overline overstrike overstriking groff Groff grotty
;; LocalWords: roff el tty unicode txt viewable charset

;; Local variables:
;; coding: latin-1
;; End:

(provide 'tty-format)

;;; tty-format.el ends here
