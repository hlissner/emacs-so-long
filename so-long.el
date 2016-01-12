;; Prevent really long lines in minified files from bringing performance to
;; a stand-still.

(defvar my-over-long-line-threshold 250
  "Number of columns after which the normal mode for a file will not be
used, unless it is specified as a local variable.

`my-over-long-line-mode' will be used instead in these circumstances.

See `my-over-long-line-detected' for details.")

(defvar my-over-long-line-max-lines 5
  "Number of non-blank, non-comment lines to test for excessive length.

See `my-over-long-line-detected' for details.")

(defun my-over-long-line-detected ()
  "Following any initial comments and blank lines, the next N lines of the
buffer will be tested for excessive length (where \"excessive\" means above
`my-over-long-line-threshold', and N is `my-over-long-line-max-lines').

Returns non-nil if any such excessive-length line is detected."
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (comment-forward)) ;; clears whitespace at minimum
      (catch 'excessive
        (while (< count my-over-long-line-max-lines)
          (if (> (- (line-end-position 1) (point))
                 my-over-long-line-threshold)
              (throw 'excessive t)
            (forward-line)
            (setq count (1+ count))))))))

(define-derived-mode my-over-long-line-mode nil "Over-long lines"
  "This mode is used if line lengths exceed `my-over-long-line-threshold'.

Many Emacs modes struggle with buffers which contain excessively long lines,
and may consequently cause unacceptable performance issues.

This is commonly on account of 'minified' code (i.e. code has been compacted
into the smallest file size possible, which often entails removing newlines
should they not be strictly necessary).

When such files are detected, we call this mode instead of the mode which
would normally have been chosen. This may happen after the initial MODE-ONLY
call to `hack-local-variables', or after `set-auto-mode' has been called.

By default this mode is essentially equivalent to `fundamental-mode', and
exists mainly to provide information to the user as to why the expected mode
was not used."
  (message "Using %s on account of line length." major-mode))

(defadvice hack-local-variables (after my-over-long-line-mode--hlv)
  "Use `my-over-long-line-mode' for files with very long lines.

This advice acts after the inital MODE-ONLY call to `hack-local-variables'(1)
and so will take effect before the real mode would have been called. This
may be ineffective on account of comments, as no comment syntax is understood
at this stage, and hence leading comments (with short lines) ahead of the very
long lines may prevent long-line detection from succeeding.

If this fails, we still try again after `set-auto-mode' has finished; at
which point we can also process comments.

 (1) Note that if the buffer in question actually has a 'mode' local variable,
we will honour that, and not attempt to change modes."
  (when (ad-get-arg 0) ; MODE-ONLY argument to `hack-local-variables'
    (unless ad-return-value ; No local var mode was found
      (when (my-over-long-line-detected)
        (setq ad-return-value 'my-over-long-line-mode)))))
(ad-activate 'hack-local-variables)

(defadvice set-auto-mode (after my-over-long-line-mode--sam)
  "Use `my-over-long-line-mode' for files with very long lines.

This advice acts after `set-auto-mode' has made its decision, and can
therefore skip any leading comments before looking for long lines."
  (unless (derived-mode-p 'my-over-long-line-mode)
    (when (my-over-long-line-detected)
      (my-over-long-line-mode))))
(ad-activate 'set-auto-mode)
