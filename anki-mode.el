;;; anki-mode.el --- a minor mode for emacs to ease the creation of cloze deletion notes -*- coding: utf-8 -*-

;; Copyright (C) 2013, 2014 Sylvain Girard

;; Author   : Sylvain Girard
;; URL      : https://github.com/alfred-m/anki-mode.el
;; Version  : 0.1
;; Keywords : anki, cloze deletion, import

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; TODO: fill docstring and README.

(define-minor-mode anki-mode
  "Provide functions to prepare anki notes."
  :lighter " Anki"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-&") 'anki-cloze-region)
            (define-key map (kbd "C-=") 'anki-set-counter)
            (define-key map (kbd "C-à") (lambda ()
                                          (interactive)
                                          (anki-increment-counter -1)))
            (define-key map (kbd "C-)") (lambda ()
                                          (interactive) 
                                          (anki-increment-counter)))
            (define-key map (kbd "C-c C-&") 'anki-break-at-dot)
            (define-key map (kbd "C-c C-=") 'anki-remove-no-cloze)
            map)
  ;; (make-local-variable 'anki-counter)
)

;; (make-variable-buffer-local
 (defvar anki-counter 1 "Counter for cloze deletion.")
;; )

(defcustom anki-field-separator "~" "Character used to delimit fields of anki notes.")


(defun anki-break-at-dot()
  "Break sentence at dots."
  (interactive)
  (let (beg end)
    (if (use-region-p)
        (progn
          (setq beg (region-beginning))
          (if (< (region-end) (point-max))
              (if (char-equal (char-after (region-end)) 10)
                  (setq end (+ 1 (region-end)))
                (setq end (region-end))
                )
            (setq end (point-max))
            )
          )
          (progn
            (setq beg (point))
            (setq end (point-max))
            )
      )
    (replace-regexp "\\.\n" ". " nil beg  end)
    (replace-regexp "\n" "" nil beg  end)
    (replace-regexp "\\. +" (concat "." anki-field-separator "\n") nil beg end)
    )
  )

;; TODO: replace "replace-regexp" by loops like this:
;; (while (re-search-forward REGEXP nil t)
;;   (replace-match TO-STRING nil nil))

;; TODO: deal properply with the inclusion of last line when using region. The
;; current code does not apply to the last line of the region if it is also
;; the last line of the buffer.

(defun anki-remove-no-cloze()
  "Remove lines with no cloze."
  (interactive)
  (let (beg end)
    (if (use-region-p)
        (progn
          (setq beg (region-beginning))
          (if (< (region-end) (point-max))
              (if (char-equal (char-after (region-end)) 10)
                  (setq end (+ 1 (region-end)))
                (setq end (region-end))
                )
            (setq end (point-max))
            )
          )
          (progn
            (setq beg (point))
            (setq end (point-max))
            )
      )
  (keep-lines ".+{{c.+" beg end t)
  )
)


(defun anki-cloze-region(VALUE)
  "Add '{{cX::' at region's beginning and '}}' at region's end where X is
anki-counter and increment anki-couter"
  (interactive "P")
  (cond
   ((equal current-prefix-arg nil)
    (list
     (setq VALUE anki-counter)
     (anki-increment-counter)
     )
    )
    ((equal current-prefix-arg '(4))
     (setq VALUE anki-counter))
    ((equal current-prefix-arg '(16))
     (setq VALUE (- anki-counter 1)))
    ((not (consp current-prefix-arg))
     (setq VALUE current-prefix-arg)
     )
    )
;;
  (if
 (not (use-region-p))
      (anki-select-current-word)
    )
;;
  (let (beg end clozebeg clozeend)
    (setq beg (region-beginning))
    (setq end (region-end))
    (setq clozeend "}}")
  (save-excursion
    (goto-char end)
    (insert clozeend)
    (goto-char beg)
    (setq clozebeg (concat "{{c" (number-to-string VALUE) "::"))
    (insert clozebeg)
    )
  (forward-char (length clozeend))
  )
  )

(defun anki-increment-counter(&optional DELTA)
  "Increment the anki counter of DELTA. If DELTA is unspecified,
increment of 1."
  (if (not DELTA) (setq DELTA 1))
  (setq anki-counter (+ DELTA anki-counter))
  (message (concat "anki-counter: "
                   (number-to-string (- anki-counter DELTA))
                   " → "
                   (number-to-string anki-counter)))
  )

(defun anki-set-counter(VALUE)
  "Set anki-counter to a VALUE passed in argument."
  (interactive "P")
  (cond
   ((equal current-prefix-arg nil)
    (let (prompt)
      (setq prompt (concat "anki-counter: "
                           (number-to-string anki-counter)
                           " → "))
      (setq VALUE (read-number prompt anki-counter))
      (if prompt (setq anki-counter VALUE))
      )
    )
    ((equal current-prefix-arg '(4))
     (setq anki-counter 1))
    ((not (consp current-prefix-arg))
     (setq anki-counter current-prefix-arg))
   )
  (message (concat "anki-counter: " (number-to-string anki-counter)))
)

(defun anki-select-current-word()
"Select the word under cursor.
“word” here is considered any alphanumeric sequence with “_” or “-”."
;; This function derives from http://ergoemacs.org/emacs/elisp_examples.html
 (interactive)
 (let (pt)
   (skip-chars-backward "^ \n,;:.(\[\"«„‘\?!<")
   (setq pt (point))
   (skip-chars-forward "^ \n,;:.)\]\"»”’\?!>")
   (set-mark pt)
 ))

(provide 'anki-mode)
