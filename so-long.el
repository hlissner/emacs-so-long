;; Prevent really long lines in minified files from bringing performance to
;; a stand-still.

(defvar my-long-line-auto-mode-threshold 500
  "Number of columns after which the mode for a file will not be set
automatically, unless it is specified as a local variable.

This is tested against the first non-blank line of the file.")

(defadvice hack-local-variables (after my-fundamental-mode-for-long-line-files)
  "Use `fundamental-mode' for files with very long lines.

Often the performance of a default mode for a given file type is extremely
poor when the file in quesiton contains very long lines.

This is sometimes the case for 'minified' code which has been compacted
into the smallest file size possible, which may entail removing newlines
if they are not strictly necessary."
  (when (ad-get-arg 0) ; MODE-ONLY argument to `hack-local-variables'
    (unless ad-return-value ; No local var mode was found
      (save-excursion
        (goto-char (point-min))
        (skip-chars-forward " \t\n")
        (end-of-line)
        (when (> (point) my-long-line-auto-mode-threshold)
          (setq ad-return-value 'fundamental-mode))))))

(ad-activate 'hack-local-variables)
