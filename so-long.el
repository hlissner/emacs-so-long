;;; so-long.el --- Say farewell to performance problems with minified code.
;;
;; Copyright (C) 2015, 2016, 2018 Free Software Foundation, Inc.

;; Author: Phil Sainty <psainty@orcon.net.nz>
;; Maintainer: Phil Sainty <psainty@orcon.net.nz>
;; URL: https://savannah.nongnu.org/projects/so-long
;; Keywords: convenience
;; Created: 23 Dec 2015
;; Package-Requires: ((emacs "24.3"))
;; Version: 0.8

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; When the lines in a buffer are so long that performance could suffer to an
;; unacceptable degree, we say "so long" to the buffer's major mode, and invoke
;; something much more basic in its place.
;;
;; Many Emacs modes struggle with buffers which contain excessively long lines.
;; This is commonly on account of 'minified' code (i.e. code that has been
;; compacted into the smallest file size possible, which often entails removing
;; newlines should they not be strictly necessary).  Most programming modes
;; simply aren't optimised (remotely) for this scenario, and so performance can
;; suffer significantly.
;;
;; When such files are detected, we invoke `so-long-mode' in place of the mode
;; that Emacs selected.  This is almost identical to `fundamental-mode', and
;; so provides optimal major mode performance in the buffer.  In addition, we
;; explicitly disable certain minor modes and set buffer-local values for
;; certain variables (all configurable), where there are performance
;; implications.
;;
;; These kinds of minified files are typically not intended to be edited; so
;; not providing the usual editing mode in such cases will rarely be an issue.
;; However, should the user wish to do so, the original mode may be reinstated
;; easily in any given buffer using `so-long-mode-revert' (the key binding for
;; which is advertised when the mode is entered).
;;
;; Alternatively, the user option `so-long-function' allows functions other
;; than `so-long-mode' to be invoked in response to long lines.
;;
;; Note that while the measures taken by this library can improve performance
;; dramatically when dealing with such files, this library does not have any
;; effect on the fundamental limitations of the Emacs redisplay code itself;
;; and so if you do need to edit the file, performance may still degrade as
;; you get deeper into the long lines.  In such circumstances you may find
;; that `longlines-mode' is the most helpful facility.

;; Installation
;; ------------
;; Put so-long.el in a directory in your load-path, and add the following to
;; your init file:
;;
;; (when (require 'so-long nil :noerror)
;;   (so-long-enable))

;; Configuration
;; -------------
;; Use M-x customize-group RET so-long RET
;;
;; Variables `so-long-target-modes', `so-long-threshold', `so-long-max-lines',
;; and `so-long-enabled' determine whether action will be taken in a given
;; buffer.  The tests are made after `set-auto-mode' has set the normal major
;; mode.  The `so-long-function' variable determines what will be done.

;; Inhibiting and disabling minor modes
;; ------------------------------------
;; The simple way to disable most buffer-local minor modes is to add the mode
;; symbol to the `so-long-minor-modes' list.  Several modes are targeted by
;; default, and it is a good idea to customize this variable to add any
;; additional buffer-local minor modes that you use which you know to have
;; performance implications.
;;
;; In the case of globalized minor modes, be sure to specify the buffer-local
;; minor mode, and not the global mode which controls it.
;;
;; Note that `so-long-minor-modes' is not useful for other global minor modes
;; (as distinguished from globalized minor modes), but in some cases it will be
;; possible to inhibit or otherwise counter-act the behaviour of a global mode
;; by overriding variables, or by employing hooks (see below).  You would need
;; to inspect the code for a given global mode (on a case by case basis) to
;; determine whether it's possible to inhibit it for a single buffer -- and if
;; so, how best to do that, as not all modes are alike.

;; Overriding variables
;; --------------------
;; `so-long-variable-overrides' is an alist mapping variable symbols to values.
;; When `so-long-mode' is invoked, the buffer-local value for each variable in
;; the list is set to the associated value in the alist.  Use this to enforce
;; values which will improve performance or otherwise avoid undesirable
;; behaviours.  If the `so-long-revert' command is called, then the original
;; values are restored.

;; Hooks
;; -----
;; `so-long-mode-hook' is the standard major mode hook, which runs between
;; `change-major-mode-after-body-hook' and `after-change-major-mode-hook'.
;;
;; `so-long-hook' runs during `after-change-major-mode-hook', but after
;; globalized minor modes have acted (because we call `add-hook' with the
;; APPEND argument, and globalized modes do not).  This hook runs immediately
;; after `so-long-minor-modes' has been processed.
;;
;; Lastly, if the `so-long-revert' command is used to restore the original
;; major mode then, once that has happened, `so-long-revert-hook' is run.
;; This could be used to undo the effects of the previous hooks.

;; Troubleshooting
;; ---------------
;; Any elisp library has the potential to cause performance problems, so
;; while the default configuration addresses some important common cases,
;; it's entirely possible that your own config introduces problem cases
;; which are unknown to this library.
;;
;; If visiting a file is still taking a very long time with so-long enabled,
;; you should test the following command:
;;
;; emacs -Q -l /path/to/so-long.el -f so-long-enable <file>
;;
;; If the file loads quickly when that command is used, you'll know that
;; something in your personal configuration is causing problems.  If this
;; turns out to be a buffer-local minor mode, or a user option, you can
;; likely alleviate the issue by customizing `so-long-minor-modes' or
;; `so-long-variable-overrides' accordingly.

;; Example configuration
;; ---------------------
;; (when (require 'so-long nil :noerror)
;;   (so-long-enable)
;;   ;; Additional target major modes to trigger for.
;;   (mapc (apply-partially 'add-to-list 'so-long-target-modes)
;;         '(sgml-mode nxml-mode))
;;   ;; Additional buffer-local minor modes to disable.
;;   (mapc (apply-partially 'add-to-list 'so-long-minor-modes)
;;         '(diff-hl-mode diff-hl-amend-mode diff-hl-flydiff-mode))
;;   ;; Additional variables to override.
;;   (mapc (apply-partially 'add-to-list 'so-long-variable-overrides)
;;         '((show-trailing-whitespace . nil)
;;           (truncate-lines . nil))))

;; Implementation notes
;; --------------------
;; This library advises `hack-local-variables' (in order that we may inhibit our
;; functionality when a file-local mode is set), and `set-auto-mode' (in order
;; to react after Emacs has chosen the major mode for a buffer).

;;; Change Log:
;;
;; 0.8   - New user option `so-long-variable-overrides'.
;;       - New user option `so-long-skip-leading-comments'.
;;       - New user option `so-long-function' supporting alternative actions.
;;       - Support `longlines-mode' as a `so-long-function' option.
;;       - Renamed `so-long-mode-enabled' to `so-long-enabled'.
;;       - Refactored the default hook values using variable overrides
;;         (and returning all the hooks to nil default values).
;;       - Performance improvements for `so-long-line-detected-p'.
;; 0.7.6 - Bug fix for `so-long-mode-hook' losing its default value.
;; 0.7.5 - Documentation.
;;       - Added sgml-mode and nxml-mode to `so-long-target-modes'.
;; 0.7.4 - Refactored the handling of `whitespace-mode'.
;; 0.7.3 - Added customize group `so-long' with user options.
;;       - Added `so-long-original-values' to generalise the storage and
;;         restoration of values from the original mode upon `so-long-revert'.
;;       - Added `so-long-revert-hook'.
;; 0.7.2 - Remember the original major mode even with M-x `so-long-mode'.
;; 0.7.1 - Clarified interaction with globalized minor modes.
;; 0.7   - Handle header 'mode' declarations.
;;       - Hack local variables after reverting to the original major mode.
;;       - Reverted `so-long-max-lines' to a default value of 5.
;; 0.6.5 - Inhibit globalized `hl-line-mode' and `whitespace-mode'.
;;       - Set `buffer-read-only' by default.
;; 0.6   - Added `so-long-minor-modes' and `so-long-hook'.
;; 0.5   - Renamed library to "so-long.el".
;;       - Added explicit `so-long-enable' command to activate our advice.
;; 0.4   - Amended/documented behaviour with file-local 'mode' variables.
;; 0.3   - Defer to a file-local 'mode' variable.
;; 0.2   - Initial release to EmacsWiki.
;; 0.1   - Experimental.

;;; Code:

(declare-function longlines-mode "longlines")

(defgroup so-long nil
  "Prevent unacceptable performance degradation with very long lines."
  :prefix "so-long"
  :group 'convenience)

(defcustom so-long-threshold 250
  "Maximum line length permitted before invoking `so-long-function'.

See `so-long-line-detected-p' for details."
  :type 'integer
  :group 'so-long)

(defcustom so-long-max-lines 5
  "Number of non-blank, non-comment lines to test for excessive length.

If `so-long-skip-leading-comments' is nil then comments and blank lines will
be counted.

See `so-long-line-detected-p' for details."
  :type 'integer
  :group 'so-long)

(defcustom so-long-skip-leading-comments t
  "Non-nil to ignore all leading comments and whitespace.

See `so-long-line-detected-p' for details."
  :type 'boolean
  :group 'so-long)

(defcustom so-long-target-modes
  '(prog-mode css-mode sgml-mode nxml-mode)
  "`so-long' affects only these modes and their derivatives.

Our primary use-case is minified programming code, so `prog-mode' covers
most cases, but there are some exceptions to this."
  :type '(repeat symbol) ;; not function, as may be unknown => mismatch.
  :group 'so-long)

(defcustom so-long-function 'so-long-mode
  "The function to call when long lines are detected.

Long lines are determined by `so-long-line-detected-p' after `set-auto-mode'.

The specified function will be called with no arguments.

The default value is `so-long-mode', which replaces the original major mode.
This is the only value for which `so-long-minor-modes',
`so-long-variable-overrides', and `so-long-hook' will be automatically
processed; but custom functions may do these things manually -- refer to
`so-long-after-change-major-mode'.

Minor mode `longlines-mode' from longlines.el (see which) is also supported as a
standard option (`so-long-function-longlines-mode')."
  :type '(radio (const :tag "Change major mode to so-long-mode"
                       so-long-mode)
                (const :tag "Enable longlines-mode"
                       so-long-function-longlines-mode)
                (function :tag "Custom function")
                (const :tag "Do nothing" nil))
  :group 'so-long)

(defcustom so-long-minor-modes
  ;; In sorted groups.
  '(font-lock-mode ;; (Generally the most important).
    ;; Other standard minor modes:
    display-line-numbers-mode
    hi-lock-mode
    highlight-changes-mode
    hl-line-mode
    linum-mode
    nlinum-mode
    prettify-symbols-mode
    visual-line-mode
    whitespace-mode
    ;; Known third-party modes-of-interest:
    diff-hl-amend-mode
    diff-hl-flydiff-mode
    diff-hl-mode
    dtrt-indent-mode
    hl-sexp-mode
    idle-highlight-mode
    rainbow-delimiters-mode
    )
  ;; It's not clear to me whether all of these would be problematic, but they
  ;; seemed like reasonable targets.  Some are certainly excessive in smaller
  ;; buffers of minified code, but we should be aiming to maximise performance
  ;; by default, so that Emacs is as responsive as we can manage in even very
  ;; large buffers of minified code.
  "List of buffer-local minor modes to explicitly disable in `so-long-mode'.

The modes are disabled by calling them with a single numeric argument of zero.

This happens during `after-change-major-mode-hook', and after any globalized
minor modes have acted, so that buffer-local modes controlled by globalized
modes can also be targeted.

`so-long-hook' can be used where more custom behaviour is desired.

See also `so-long-mode-hook'.

Please submit bug reports to recommend additional modes for this list, whether
they are in Emacs core, GNU ELPA, or elsewhere."
  :type '(repeat symbol) ;; not function, as may be unknown => mismatch.
  :group 'so-long)

(defcustom so-long-variable-overrides
  '((bidi-display-reordering . nil)
    (buffer-read-only . t)
    (global-hl-line-mode . nil)
    (line-move-visual . t)
    (truncate-lines . nil))
  "Variables to override, and the values to override them with."
  :type '(alist :key-type (variable :tag "Variable")
                :value-type (sexp :tag "Value"))
  :options '((bidi-display-reordering boolean)
             (buffer-read-only boolean)
             (global-hl-line-mode boolean)
             (line-move-visual boolean)
             (truncate-lines boolean))
  :group 'so-long)

(defcustom so-long-hook nil
  "List of functions to call after `so-long-mode'.

This hook runs during `after-change-major-mode-hook', and after any globalized
minor modes have acted.

See also `so-long-mode-hook' and `so-long-minor-modes'."
  :type 'hook
  :group 'so-long)

(defcustom so-long-revert-hook nil
  "List of functions to call after `so-long-mode-revert'."
  :type 'hook
  :group 'so-long)

(defvar so-long-enabled t
  "Set to nil to prevent `so-long-function' from being triggered.")

(defvar-local so-long--inhibited nil) ; internal use
(put 'so-long--inhibited 'permanent-local t)

(defvar-local so-long-original-values nil
  "Alist holding the buffer's original `major-mode' value, and other data.

Any values to be restored by `so-long-revert' can be stored here during
`change-major-mode-hook' and reinstated during `so-long-revert-hook'.

See also `so-long-remember' and `so-long-original'.")
(put 'so-long-original-values 'permanent-local t)

(defun so-long-original (key &optional exists)
  "Return the current value for KEY in `so-long-original-values'.

If you need to differentiate between a stored value of nil and no stored value
at all, make EXISTS non-nil.  This then returns the result of `assq' directly:
nil if no value was set, and a cons cell otherwise."
  (if exists
      (assq key so-long-original-values)
    (cadr (assq key so-long-original-values))))

(defun so-long-remember (variable)
  "Push the `symbol-value' for VARIABLE to `so-long-original-values'."
  (when (boundp variable)
    (let ((locals (buffer-local-variables)))
      (push (list variable
                  (symbol-value variable)
                  (consp (assq variable locals)))
            so-long-original-values))))

(defun so-long-change-major-mode ()
  "Ensures that `so-long-mode' knows the original `major-mode'
even when invoked interactively.

Called by default during `change-major-mode-hook'."
  (unless (eq major-mode 'so-long-mode)
    (setq so-long-original-values nil)
    (so-long-remember 'major-mode)
    (dolist (ovar so-long-variable-overrides)
      (so-long-remember (car ovar)))))

;; When the line's long
;; When the mode's slow
;; When Emacs is sad
;; We change automatically to faster code
;; And then I won't feel so mad

(defun so-long-line-detected-p ()
  "Following any initial comments and blank lines, the next N lines of the
buffer will be tested for excessive length (where \"excessive\" means above
`so-long-threshold', and N is `so-long-max-lines').

Returns non-nil if any such excessive-length line is detected.

If `so-long-skip-leading-comments' is nil then the N lines will be counted
starting from the first line of the buffer.  In this instance you will likely
want to increase `so-long-max-lines' to allow for possible comments."
  (let ((count 0) start)
    (save-excursion
      (goto-char (point-min))
      (when so-long-skip-leading-comments
        ;; Clears whitespace at minimum.
        ;; We use narrowing to limit the amount of text being processed at any
        ;; given time, where possible, as this makes things more efficient.
        (setq start (point))
        (while (save-restriction
                 (narrow-to-region start (min (+ (point) so-long-threshold)
                                              (point-max)))
                 (goto-char start)
                 ;; Possibilities for `comment-forward' are:
                 ;; 0. No comment; no movement; return nil.
                 ;; 1. Comment is <= point-max; move end of comment; return t.
                 ;; 2. Comment is truncated; move point-max; return nil.
                 ;; 3. Only whitespace; move end of WS; return nil.
                 (prog1 (or (comment-forward 1) ;; Moved past a comment.
                            (and (eobp) ;; Truncated, or WS up to point-max.
                                 (progn ;; Widen and retry.
                                   (widen)
                                   (goto-char start)
                                   (comment-forward 1))))
                   ;; Otherwise there was no comment, and we return nil.
                   ;; If there was whitespace, we moved past it.
                   (setq start (point))))))
      ;; Start looking for long lines.
      ;; `while' will ultimately return nil if we do not `throw' a result.
      (catch 'excessive
        (while (< count so-long-max-lines)
          (save-restriction
            (narrow-to-region (point) (min (+ (point) 1 so-long-threshold)
                                           (point-max)))
            (if (> (- (line-end-position 1) (point))
                   so-long-threshold)
                (throw 'excessive t)
              (when (> (forward-line) 0)
                (throw 'excessive nil))
              (setq count (1+ count)))))))))

(defun so-long-function-longlines-mode ()
  "Enable minor mode `longlines-mode'.

This is a `so-long-function' option."
  (require 'longlines)
  (longlines-mode 1))

(define-derived-mode so-long-mode nil "So long"
  "This major mode is the default `so-long-function' option.

Many Emacs modes struggle with buffers which contain excessively long lines,
and may consequently cause unacceptable performance issues.

This is commonly on account of 'minified' code (i.e. code has been compacted
into the smallest file size possible, which often entails removing newlines
should they not be strictly necessary).  These kinds of files are typically
not intended to be edited, so not providing the usual editing mode in these
cases will rarely be an issue.

When such files are detected, we invoke this mode.  This happens after
`set-auto-mode' has set the major mode, should the selected major mode be
a member (or derivative of a member) of `so-long-target-modes'.

After changing modes, any active minor modes listed in `so-long-minor-modes'
are disabled for the current buffer, and buffer-local values are assigned to
variables in accordance with `so-long-variable-overrides'.  Finally
`so-long-hook' is run.  These steps occur in `after-change-major-mode-hook',
so that minor modes controlled by globalized minor modes can also be disabled.

Some globalized minor modes may be inhibited by acting in `so-long-mode-hook'.

By default this mode is essentially equivalent to `fundamental-mode', and
exists mainly to provide information to the user as to why the expected mode
was not used, and to facilitate hooks for other so-long functionality.

To revert to the original mode despite any potential performance issues,
type \\[so-long-mode-revert], or else re-invoke it manually."
  (add-hook 'after-change-major-mode-hook
            'so-long-after-change-major-mode :append :local)
  ;; Override variables.  This is the first of two instances where we do this
  ;; (the other being `so-long-after-change-major-mode').  It is desirable to
  ;; set variables here in order to cover cases where the setting of a variable
  ;; influences how a global minor mode behaves in this buffer.
  (so-long-override-variables)
  ;; Inform the user about our major mode hijacking.
  (message (concat "Changed to %s (from %s)"
                   (unless (eq this-command 'so-long-mode)
                     " on account of line length")
                   ".  %s to revert.")
           major-mode
           (or (so-long-original 'major-mode) "<unknown>")
           (substitute-command-keys "\\[so-long-mode-revert]")))

(defcustom so-long-mode-hook nil
  "List of functions to call when `so-long-mode' is invoked.

This is the standard mode hook for `so-long-mode' which runs between
`change-major-mode-after-body-hook' and `after-change-major-mode-hook'.

Note that globalized minor modes have not yet acted.

See also `so-long-hook'."
  :type 'hook
  :group 'so-long)

(defun so-long-after-change-major-mode ()
  "Disable modes in `so-long-minor-modes' and run `so-long-hook' functions.

Also override the variables in `so-long-variable-overrides'.

This happens during `after-change-major-mode-hook'."
  ;; Disable minor modes.
  (so-long-disable-minor-modes)
  ;; Override variables (again).  We already did this in `so-long-mode' in
  ;; order that variables which affect global/globalized minor modes can have
  ;; that effect; however it's feasible that one of the minor modes disabled
  ;; above might have reverted one of these variables, so we re-enforce them.
  ;; (For example, disabling `visual-line-mode' sets `line-move-visual' to
  ;; nil, when for our purposes it is preferable for it to be non-nil).
  (so-long-override-variables)
  ;; By default we set `buffer-read-only', which can cause problems if hook
  ;; functions need to modify the buffer.  We use `inhibit-read-only' to
  ;; side-step the issue (and likewise in `so-long-mode-revert').
  (let ((inhibit-read-only t))
    (run-hooks 'so-long-hook)))

(defun so-long-disable-minor-modes ()
  "Disable any active minor modes listed in `so-long-minor-modes'."
  (dolist (mode so-long-minor-modes)
    (when (and (boundp mode) mode)
      (funcall mode 0))))

(defun so-long-override-variables ()
  "Process `so-long-variable-overrides'."
  (dolist (ovar so-long-variable-overrides)
    (set (make-local-variable (car ovar)) (cdr ovar))))

(defun so-long-restore-variables ()
  "Restore the remembered values for the overridden variables."
  (dolist (ovar so-long-variable-overrides)
    (let ((remembered (so-long-original (car ovar) :exists)))
      (when remembered
        ;; If a variable was originally buffer-local then restore it as
        ;; a buffer-local variable, even if the global value is a match.
        ;;
        ;; If the variable was originally global and the current value
        ;; matches its original value, then leave it alone.
        ;;
        ;; Otherwise set it buffer-locally to the original value.
        (unless (and (equal (symbol-value (car ovar)) (cadr remembered))
                     (not (nth 2 remembered))) ;; originally global
          (set (make-local-variable (car ovar)) (cadr remembered)))))))

(defun so-long-mode-revert ()
  "Call the `major-mode' which was selected before `so-long-mode' replaced it,
and re-process the local variables.  Lastly run `so-long-revert-hook'."
  (interactive)
  (let ((so-long-original-mode (so-long-original 'major-mode)))
    (unless so-long-original-mode
      (error "Original mode unknown."))
    (funcall so-long-original-mode)
    (hack-local-variables)
    ;; Restore overridden variables, and run hook.
    ;; `kill-all-local-variables' was already called by the original mode
    ;; function, so we may be seeing global values.
    (so-long-restore-variables)
    (let ((inhibit-read-only t))
      (run-hooks 'so-long-revert-hook))))

(define-key so-long-mode-map (kbd "C-c C-c") 'so-long-mode-revert)

(defun so-long-check-header-modes ()
  "Handles the header-comments processing in `set-auto-mode'.

`set-auto-mode' has some special-case code to handle the 'mode' pseudo-variable
when set in the header comment.  This runs outside of `hack-local-variables'
and cannot be conveniently intercepted, so we are forced to replicate it here.

This special-case code will ultimately be removed from Emacs, as it exists to
deal with a deprecated feature; but until then we need to replicate it in order
to inhibit our own behaviour in the presence of a header comment 'mode'
declaration."
  ;; The following code for processing MODE declarations in the header
  ;; comments is copied verbatim from `set-auto-mode', because we have
  ;; no way of intercepting it.
  ;;
  (let ((try-locals (not (inhibit-local-variables-p)))
        end done mode modes)
    ;; Once we drop the deprecated feature where mode: is also allowed to
    ;; specify minor-modes (ie, there can be more than one "mode:"), we can
    ;; remove this section and just let (hack-local-variables t) handle it.
    ;; Find a -*- mode tag.
    (save-excursion
      (goto-char (point-min))
      (skip-chars-forward " \t\n")
      ;; Note by design local-enable-local-variables does not matter here.
      (and enable-local-variables
           try-locals
           (setq end (set-auto-mode-1))
           (if (save-excursion (search-forward ":" end t))
               ;; Find all specifications for the `mode:' variable
               ;; and execute them left to right.
               (while (let ((case-fold-search t))
                        (or (and (looking-at "mode:")
                                 (goto-char (match-end 0)))
                            (re-search-forward "[ \t;]mode:" end t)))
                 (skip-chars-forward " \t")
                 (let ((beg (point)))
                   (if (search-forward ";" end t)
                       (forward-char -1)
                     (goto-char end))
                   (skip-chars-backward " \t")
                   (push (intern (concat (downcase (buffer-substring beg (point))) "-mode"))
                         modes)))
             ;; Simple -*-MODE-*- case.
             (push (intern (concat (downcase (buffer-substring (point) end))
                                   "-mode"))
                   modes))))

    ;; `so-long' now processes the resulting mode list.  If any modes were
    ;; listed, we assume that one of them is a major mode.  It's possible that
    ;; this isn't true, but the buffer would remain in fundamental-mode if that
    ;; were the case, so it is very unlikely.
    (setq so-long--inhibited modes)))

;; How do you solve a problem like a long line?
;; How do you stop a mode from slowing down?
;; How do you cope with processing a long line?
;; A bit of advice! A mode! A workaround!

(defadvice hack-local-variables (after so-long--file-local-mode disable)
  "Ensure that `so-long-mode' defers to file-local mode declarations.

This advice acts after any initial MODE-ONLY call to `hack-local-variables',
and ensures that we do not change to `so-long-mode' in that scenario.

File-local header comments are currently an exception (see the commentary
for details).  The file-local mode will ultimately still be used, however
`so-long-mode' still runs first, thus displaying a misleading message.
This issue will eventually be resolved in Emacs."
  (and (ad-get-arg 0) ; MODE-ONLY argument to `hack-local-variables'
       ;; Inhibit `so-long-mode' if a MODE is specified.
       (eq so-long-function 'so-long-mode)
       (setq so-long--inhibited ad-return-value)))

;; n.b. Call (so-long-enable) after changes, to re-activate the advice.

(defadvice set-auto-mode (around so-long--set-auto-mode disable)
  "Maybe trigger `so-long-function' for files with very long lines.

This advice acts after `set-auto-mode' has set the buffer's major mode.

We can't act before this point, because some major modes must be exempt
\(binary file modes, for example).  Instead, we act only when the selected
major mode is a member (or derivative of a member) of `so-long-target-modes'.

`so-long-line-detected-p' then determines whether the mode change is needed."
  (setq so-long--inhibited nil) ; is permanent-local
  (when so-long-enabled
    (so-long-check-header-modes)) ; may set `so-long--inhibited'
  ad-do-it ; `set-auto-mode'      ; may set `so-long--inhibited'
  (when so-long-enabled
    (unless so-long--inhibited
      (when (and (apply 'derived-mode-p so-long-target-modes)
                 (so-long-line-detected-p)
                 (functionp so-long-function))
        (funcall so-long-function)))))

;; n.b. Call (so-long-enable) after changes, to re-activate the advice.

;;;###autoload
(defun so-long-enable ()
  "Enable the so-long library's functionality."
  (interactive)
  (add-hook 'change-major-mode-hook 'so-long-change-major-mode)
  (ad-enable-advice 'hack-local-variables 'after 'so-long--file-local-mode)
  (ad-enable-advice 'set-auto-mode 'around 'so-long--set-auto-mode)
  (ad-activate 'hack-local-variables)
  (ad-activate 'set-auto-mode)
  (setq so-long-enabled t))

(defun so-long-disable ()
  "Disable the so-long library's functionality."
  (interactive)
  (remove-hook 'change-major-mode-hook 'so-long-change-major-mode)
  (ad-disable-advice 'hack-local-variables 'after 'so-long--file-local-mode)
  (ad-disable-advice 'set-auto-mode 'around 'so-long--set-auto-mode)
  (ad-activate 'hack-local-variables)
  (ad-activate 'set-auto-mode)
  (setq so-long-enabled nil))

(defun so-long-unload-function ()
  (so-long-disable)
  nil)

(provide 'so-long)

;; So long, farewell, auf wiedersehen, goodbye
;; You have to go, this code is minified
;; Goodbye!

;;; so-long.el ends here
