;;; so-long.el --- Say farewell to performance problems with minified code.  -*- lexical-binding:t -*-
;;
;; Copyright (C) 2015, 2016, 2018 Free Software Foundation, Inc.

;; Author: Phil Sainty <psainty@orcon.net.nz>
;; Maintainer: Phil Sainty <psainty@orcon.net.nz>
;; URL: https://savannah.nongnu.org/projects/so-long
;; Keywords: convenience
;; Created: 23 Dec 2015
;; Package-Requires: ((emacs "24.3"))
;; Version: 1.0

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; When the lines in a file are so long that performance could suffer to an
;; unacceptable degree, we say "so long" to the slow modes and options enabled
;; in that buffer, and invoke something much more basic in their place.
;;
;; Many Emacs modes struggle with buffers which contain excessively long lines.
;; This is commonly on account of 'minified' code (i.e. code that has been
;; compacted into the smallest file size possible, which often entails removing
;; newlines should they not be strictly necessary).  Most programming modes
;; simply aren't optimised (remotely) for this scenario, and so performance can
;; suffer significantly.
;;
;; When such files are detected, we automatically override certain minor modes
;; and variables with performance implications (all configurable), in order to
;; enhance performance in the buffer.
;;
;; By default we also invoke `so-long-mode' in place of the major mode that
;; Emacs selected.  This is almost identical to `fundamental-mode', and so
;; provides optimal major mode performance.  These kinds of minified files are
;; typically not intended to be edited, so not providing the usual editing mode
;; in such cases will rarely be an issue.  However, should the user wish to do
;; so, the original mode may be reinstated easily in any given buffer using
;; `so-long-revert' (the key binding for which is advertised when the major
;; mode change occurs).  If you prefer not to change the major mode, the
;; `overrides-only' action can be configured.
;;
;; The user options `so-long-action' and `so-long-action-alist' determine what
;; will happen when `so-long' and `so-long-revert' are invoked, allowing
;; alternative actions (including custom actions) to be configured.  By default
;; `longlines-mode' is supported as an alternative action.
;;
;; Note that while the measures taken by this library can improve performance
;; dramatically when dealing with such files, this library does not have any
;; effect on the fundamental limitations of the Emacs redisplay code itself;
;; and so if you do need to edit the file, performance may still degrade as
;; you get deeper into the long lines.  In such circumstances you may find
;; that `longlines-mode' is the most helpful facility.
;;
;; Note also that the mitigations are automatically triggered when visiting a
;; file.  The library does not automatically detect if long lines are inserted
;; into an existing buffer (although the `so-long' command can be invoked
;; manually in such situations).

;; Installation
;; ------------
;; And add the following to your init file to enable the library:
;;
;; (when (require 'so-long nil :noerror)
;;   (so-long-enable))
;;
;; If necessary, ensure that so-long.el is in a directory in your load-path.
;; (This step is not necessary if you are using Emacs 27+, or have installed the
;; GNU ELPA package.)

;; Configuration
;; -------------
;; Use M-x customize-group RET so-long RET
;;
;; Variables `so-long-target-modes', `so-long-threshold', `so-long-max-lines',
;; and `so-long-enabled' determine whether action will be taken in a given
;; buffer.  The tests are made after `set-auto-mode' has set the normal major
;; mode.  The `so-long-action' variable determines what will be done.
;;
;; You can also use M-x so-long to invoke the behaviour manually.
;;
;; In Emacs 26+ it is also possible to use file- or directory-local variables to
;; configure the behaviour for particular files.  (In earlier versions of Emacs
;; the local variables are processed too late, and hence have no effect.)

;; Actions and menus
;; -----------------
;; The user options `so-long-action' and `so-long-action-alist' determine what
;; will happen when `so-long' and `so-long-revert' are invoked, and you can add
;; your own custom actions if you wish.
;;
;; It is also possible to set the buffer-local `so-long-function' and
;; `so-long-revert-function' values directly -- any existing value for these
;; variables will be used in preference to the values defined by the selected
;; action.  For directory-local or file-local usage it is preferable to set
;; only `so-long-action', as all function variables are marked as 'risky',
;; meaning you would need to add to `safe-local-variable-values' in order to
;; avoid being queried about them.
;;
;; All defined actions are presented in the "So Long" menu, which is visible
;; whenever long lines have been detected.  Selecting an action from the menu
;; will first cause the current action (if any) to be reverted, before the
;; newly-selected action is invoked.
;;
;; Aside from the menu bar, the menu is also available in the mode line --
;; either via the major mode construct (when `so-long-mode' is active), or
;; in a separate mode line construct when some other major mode is active.

;; Inhibiting and disabling minor modes
;; ------------------------------------
;; The simple way to disable most buffer-local minor modes is to add the mode
;; symbol to the `so-long-minor-modes' list.  Several modes are targeted by
;; default, and it is a good idea to customize this variable to add any
;; additional buffer-local minor modes that you use which you know to have
;; performance implications.
;;
;; These minor modes are disabled if `so-long-action' is set to either
;; `so-long-mode' or `overrides-only'.  If `so-long-revert' is called, then
;; the original values are restored.
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
;; If `so-long-action' is set to either `so-long-mode' or `overrides-only', the
;; buffer-local value for each variable in the list is set to the associated
;; value in the alist.  Use this to enforce values which will improve
;; performance or otherwise avoid undesirable behaviours.  If `so-long-revert'
;; is called, then the original values are restored.

;; Hooks
;; -----
;; `so-long-mode-hook' is the standard major mode hook, which runs between
;; `change-major-mode-after-body-hook' and `after-change-major-mode-hook'
;; if `so-long-mode' is invoked.
;;
;; `so-long-hook' runs at the end of `so-long'.  Note that for the default
;; action `so-long-mode', this means globalized minor modes have finished acting
;; for the new major mode.
;;
;; Likewise, if the `so-long-revert' command is used to restore the original
;; state then, once that has happened, `so-long-revert-hook' is run.
;; This could be used to undo the effects of the previous hooks.

;; Troubleshooting
;; ---------------
;; Any elisp library has the potential to cause performance problems; so
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
;;
;; In some cases it may be useful to set a file-local `mode' variable to
;; `so-long-mode', completely bypassing the automated decision process.

;; Example configuration
;; ---------------------
;; If you prefer to configure in code rather than via the customize interface,
;; then you might use something along these lines:
;;
;; ;; Enable so-long library.
;; (when (require 'so-long nil :noerror)
;;   (so-long-enable)
;;   ;; Basic settings.
;;   (setq so-long-action 'overrides-only)
;;   (setq so-long-threshold 1000)
;;   (setq so-long-max-lines 100)
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
;; This library advises `set-auto-mode' (in order to react after Emacs has
;; chosen the major mode for a buffer), and `hack-local-variables' (so that we
;; may behave differently when a file-local mode is set).  In earlier versions
;; of Emacs (< 26.1) we also advise `hack-one-local-variable' (to prevent a
;; file-local mode from restoring the original major mode if we had changed it).
;;
;; Many variables are permanent-local because after the normal major mode has
;; been set, we potentially change the major mode to `so-long-mode', and it's
;; important that the values which were established prior to that are retained.

;; Caveats
;; -------
;; The variables affecting the automated behavior of this library (such as
;; `so-long-action') can be used as file- or dir-local values in Emacs 26+, but
;; not in previous versions of Emacs.  This is on account of improvements made
;; to `normal-mode' in 26.1, which altered the execution order with respect to
;; when local variables are processed.  It is unlikely that equivalent support
;; will be implemented for older versions of Emacs.  The exception to this
;; caveat is the `mode' pseudo-variable, which is processed early in all
;; versions of Emacs, and can be set to `so-long-mode' if desired.

;;; Change Log:
;;
;; 1.0   - Included in Emacs 27.1, and in GNU ELPA for prior versions of Emacs.
;;       - New user option `so-long-action'.
;;       - New user option `so-long-action-alist' defining alternative actions.
;;       - New user option `so-long-variable-overrides'.
;;       - New user option `so-long-skip-leading-comments'.
;;       - New user option `so-long-file-local-mode-function'.
;;       - New user option `so-long-predicate'.
;;       - New variable and function `so-long-function'.
;;       - New variable and function `so-long-revert-function'.
;;       - New command `so-long' to invoke `so-long-function' interactively.
;;       - New command `so-long-revert' to invoke `so-long-revert-function'.
;;       - Support retaining the original major mode while still disabling
;;         minor modes and overriding variables.
;;       - Support `longlines-mode' as a `so-long-action' option.
;;       - Added "So Long" menu, including all selectable actions.
;;       - Added mode-line indicator, user option `so-long-mode-line-label',
;;         and faces `so-long-mode-line-active', `so-long-mode-line-inactive'.
;;       - Renamed `so-long-mode-enabled' to `so-long-enabled'.
;;       - Refactored the default hook values using variable overrides
;;         (and returning all the hooks to nil default values).
;;       - Performance improvements for `so-long-detected-long-line-p'.
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

(add-to-list 'customize-package-emacs-version-alist
             '(so-long ("1.0" . "27.1")))

(declare-function longlines-mode "longlines")
(defvar longlines-mode)

(defvar so-long-enabled t
  "Set to nil to prevent `so-long' from being triggered automatically.")

(defvar-local so-long--active nil ; internal use
  "Non-nil when `so-long' mitigations are in effect.")

(defvar so-long--set-auto-mode nil ; internal use
  "Non-nil while `set-auto-mode' is executing.")

(defvar-local so-long--inhibited nil ; internal use
  "When non-nil, prevents the `set-auto-mode' advice from calling `so-long'.")
(put 'so-long--inhibited 'permanent-local t)

(defvar so-long--calling nil ; internal use
  ;; This prevents infinite recursion if eval:(so-long) is specified
  ;; as a file- or dir-local variable, and `so-long-action' is set to
  ;; `so-long-mode' (as that major mode would once again process the
  ;; local variables, and hence call itself).
  "Non-nil while `so-long' or `so-long-revert' is executing.")

(defvar-local so-long-detected-p nil
  "Non-nil if `so-long' has been invoked (even if subsequently reverted).")
(put 'so-long-detected-p 'permanent-local t)

(defgroup so-long nil
  "Prevent unacceptable performance degradation with very long lines."
  :prefix "so-long"
  :group 'convenience)

(defcustom so-long-threshold 250
  "Maximum line length permitted before invoking `so-long-function'.

See `so-long-detected-long-line-p' for details."
  :type 'integer
  :package-version '(so-long . "1.0")
  :group 'so-long)

(defcustom so-long-max-lines 5
  "Number of non-blank, non-comment lines to test for excessive length.

If nil then all lines will be tested, until either a long line is detected,
or the end of the buffer is reached.

If `so-long-skip-leading-comments' is nil then comments and blank lines will
be counted.

See `so-long-detected-long-line-p' for details."
  :type '(choice (integer :tag "Limit")
                 (const :tag "Unlimited" nil))
  :package-version '(so-long . "1.0")
  :group 'so-long)

(defcustom so-long-skip-leading-comments t
  "Non-nil to ignore all leading comments and whitespace.

See `so-long-detected-long-line-p' for details."
  :type 'boolean
  :package-version '(so-long . "1.0")
  :group 'so-long)

(defcustom so-long-target-modes
  '(prog-mode css-mode sgml-mode nxml-mode)
  "`so-long' affects only these modes and their derivatives.

Our primary use-case is minified programming code, so `prog-mode' covers
most cases, but there are some exceptions to this."
  :type '(repeat symbol) ;; not function, as may be unknown => mismatch.
  :package-version '(so-long . "1.0")
  :group 'so-long)

(defcustom so-long-predicate 'so-long-detected-long-line-p
  "Function, called after `set-auto-mode' to decide whether action is needed.

Only called if the major mode is a member of `so-long-target-modes'.

If the function returns non-nil, `so-long' will be invoked.

The specified function will be called with no arguments.

Defaults to `so-long-detected-long-line-p'."
  :type '(choice (const so-long-detected-long-line-p)
                 (function :tag "Custom function"))
  :package-version '(so-long . "1.0")
  :group 'so-long)

;; Silence byte-compiler warning.  `so-long-action-alist' is defined below
;; as a user option; but the definition sequence required for its setter
;; function means we also need to declare it beforehand.
(defvar so-long-action-alist)

(defun so-long-action-type ()
  "Generate a :type for `so-long-action' based on `so-long-action-alist'."
  ;; :type seemingly cannot be a form to be evaluated on demand, so we
  ;; endeavour to keep it up-to-date with `so-long-action-alist' by
  ;; calling this from `so-long-action-alist-setter'.
  `(radio ,@(mapcar (lambda (x) (list 'const :tag (cadr x) (car x)))
                    so-long-action-alist)
          (const :tag "Do nothing" nil)))

(defun so-long-action-alist-setter (option value)
  "The :set function for `so-long-action-alist'."
  ;; Set the value as normal.
  (set-default option value)
  ;; Update the :type of `so-long-action' to present the updated values.
  (put 'so-long-action 'custom-type (so-long-action-type)))

(defcustom so-long-action-alist
  '((so-long-mode
     "Change major mode to so-long-mode"
     so-long-mode
     so-long-mode-revert)
    (overrides-only
     "Disable minor modes and override variables"
     so-long-function-overrides-only
     so-long-revert-function-overrides-only)
    (longlines-mode
     "Enable longlines-mode"
     so-long-function-longlines-mode
     so-long-revert-function-longlines-mode))
  "Options for `so-long-action'.

Each element is a list comprising (KEY LABEL ACTION REVERT)

KEY is a symbol which is a valid value for `so-long-action', and LABEL is a
string which describes and represents the key in that option's customize
interface, and in the \"So Long\" menu.  ACTION and REVERT are functions:

ACTION will be the `so-long-function' value when `so-long' is called, and
REVERT will be the `so-long-revert-function' value, if `so-long-revert' is
subsequently called."
  :type '(alist :key-type (symbol :tag "Key")
                :value-type (list (string :tag "Label")
                                  (function :tag "Action")
                                  (function :tag "Revert")))
  :set #'so-long-action-alist-setter
  :package-version '(so-long . "1.0")
  :group 'so-long)
(put 'so-long-action-alist 'risky-local-variable t)

(defcustom so-long-action 'so-long-mode
  "The action taken by `so-long' when long lines are detected.

\(Long lines are determined by `so-long-predicate' after `set-auto-mode'.)

The value is a key to one of the options defined by `so-long-action-alist'.

The default action is to replace the original major mode with `so-long-mode'.
Alternatively, `overrides-only' retains the original major mode while still
disabling minor modes and overriding variables.  These are the only standard
values for which `so-long-minor-modes' and `so-long-variable-overrides' will be
automatically processed; but custom actions can also do these things.

The value `longlines-mode' causes that minor mode to be enabled.  See
longlines.el for more details.

Each action likewise determines the behaviour of `so-long-revert'."
  :type (so-long-action-type)
  :package-version '(so-long . "1.0")
  :group 'so-long)

(defvar-local so-long-function nil
  "The function called by `so-long'.

This should be set in conjunction with `so-long-revert-function'.  This usually
happens automatically, based on the value of `so-long-action'.

The specified function will be called with no arguments, after which
`so-long-hook' runs.")
(put 'so-long-function 'permanent-local t)

(defvar-local so-long-revert-function nil
  "The function called by `so-long-revert'.

This should be set in conjunction with `so-long-function'.  This usually
happens automatically, based on the value of `so-long-action'.

The specified function will be called with no arguments, after which
`so-long-revert-hook' runs.")
(put 'so-long-revert-function 'permanent-local t)

(defun so-long-function ()
  "The value of `so-long-function', else derive from `so-long-action'."
  (or so-long-function
      (let ((action (assq so-long-action so-long-action-alist)))
        (nth 2 action))))

(defun so-long-revert-function ()
  "The value of `so-long-revert-function', else derive from `so-long-action'."
  (or so-long-revert-function
      (let ((action (assq so-long-action so-long-action-alist)))
        (nth 3 action))))

(defcustom so-long-file-local-mode-function 'so-long-mode-downgrade
  "Function to call during `set-auto-mode' when a file-local mode is set.

The specified function will be called with a single argument, being the
file-local mode which was established.

This happens before `so-long' is called, and so this function can modify the
subsequent action.

The value `so-long-mode-downgrade' means that `so-long-function-overrides-only'
will be used in place of `so-long-mode' -- respecting the file-local mode, but
still overriding minor modes and variables, as if `so-long-action' had been set
to `overrides-only'.

The value `so-long-inhibit' means that so-long will not take any action at all
for this file.

If nil, then do not treat files with file-local modes any differently to other
files.

Note that this function is called if a file-local mode is set even if `so-long'
will not be called.  The exception to this is when the file-local mode is
`so-long-mode', in which case `so-long-file-local-mode-function' is ignored,
as the required action is unambiguous."
  :type '(radio (const so-long-mode-downgrade)
                (const so-long-inhibit)
                (const :tag "nil: Use so-long-function as normal" nil)
                (function :tag "Custom function"))
  :package-version '(so-long . "1.0")
  :group 'so-long)
(make-variable-buffer-local 'so-long-file-local-mode-function)

;; `provided-mode-derived-p' was added in 26.1
(unless (fboundp 'provided-mode-derived-p)
  (defun provided-mode-derived-p (mode &rest modes)
    "Non-nil if MODE is derived from one of MODES.
Uses the `derived-mode-parent' property of the symbol to trace backwards.
If you just want to check `major-mode', use `derived-mode-p'."
    (while (and (not (memq mode modes))
                (setq mode (get mode 'derived-mode-parent))))
    mode))

(defun so-long-handle-file-local-mode (mode)
  "Wrapper for calling `so-long-file-local-mode-function'.

The function is called with one argument, MODE, being the file-local mode which
was established."
  ;; Handle the special case whereby the file-local mode was `so-long-mode'.
  ;; In this instance we set `so-long--inhibited', because the file-local mode
  ;; is already going to do everything that is wanted.
  (if (provided-mode-derived-p mode 'so-long-mode)
      (setq so-long--inhibited t)
    ;; Call `so-long-file-local-mode-function'.
    (when (functionp so-long-file-local-mode-function)
      (funcall so-long-file-local-mode-function mode))))

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
  "List of buffer-local minor modes to explicitly disable.

The ones which were originally enabled in the buffer are disabled by calling
them with the numeric argument 0.  Unknown modes, and modes which were were not
enabled, are ignored.

This happens after any globalized minor modes have acted, so that buffer-local
modes controlled by globalized modes can also be targeted.

By default this happens if `so-long-action' is set to either `so-long-mode'
or `overrides-only'.  If `so-long-revert' is subsequently invoked, then the
disabled modes are re-enabled by calling them with the numeric argument 1.

`so-long-hook' can be used where more custom behaviour is desired.

Please submit bug reports to recommend additional modes for this list, whether
they are in Emacs core, GNU ELPA, or elsewhere."
  :type '(repeat symbol) ;; not function, as may be unknown => mismatch.
  :package-version '(so-long . "1.0")
  :group 'so-long)

(defcustom so-long-variable-overrides
  '((bidi-display-reordering . nil)
    (buffer-read-only . t)
    (global-hl-line-mode . nil)
    (line-move-visual . t)
    (truncate-lines . nil)
    (which-func-mode . nil))
  "Variables to override, and the values to override them with.

The variables are given buffer-local values.  By default this happens if
`so-long-action' is set to either `so-long-mode' or `overrides-only'.  If
`so-long-revert' is subsequently invoked, then the variables are restored to
their original states."
  :type '(alist :key-type (variable :tag "Variable")
                :value-type (sexp :tag "Value"))
  :options '((bidi-display-reordering boolean)
             (buffer-read-only boolean)
             (global-hl-line-mode boolean)
             (line-move-visual boolean)
             (truncate-lines boolean)
             (which-func-mode boolean))
  :package-version '(so-long . "1.0")
  :group 'so-long)

(defcustom so-long-hook nil
  "List of functions to call after `so-long' is called.

See also `so-long-revert-hook'."
  :type 'hook
  :package-version '(so-long . "1.0")
  :group 'so-long)

(defcustom so-long-revert-hook nil
  "List of functions to call after `so-long-revert' is called.

See also `so-long-hook'."
  :type 'hook
  :package-version '(so-long . "1.0")
  :group 'so-long)

(defcustom so-long-mode-line-label "So Long"
  "Text label of `so-long-mode-line-info' when long lines are detected.

If nil, no mode line indicator will be displayed."
  :type '(choice (string :tag "String")
                 (const :tag "None" nil))
  :package-version '(so-long . "1.0")
  :group 'so-long)

(defface so-long-mode-line-active
  '((t :inherit mode-line-emphasis))
  "Face for `so-long-mode-line-info' when mitigations are active."
  :package-version '(so-long . "1.0")
  :group 'so-long)

(defface so-long-mode-line-inactive
  '((t :inherit mode-line-inactive))
  "Face for `so-long-mode-line-info' when mitigations have been reverted."
  :package-version '(so-long . "1.0")
  :group 'so-long)

;; Modes that go slowly and line lengths excessive
;; Font-lock performance becoming oppressive
;; All of my CPU tied up with strings
;; These are a few of my least-favourite things

(defvar-local so-long-original-values nil
  "Alist holding the buffer's original `major-mode' value, and other data.

Any values to be restored by `so-long-revert' can be stored here by the
`so-long-function' or during `so-long-hook'.  `so-long' itself stores the
original states for `so-long-variable-overrides' and `so-long-minor-modes',
so these values are available to custom actions by default.

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
    (setq so-long-original-values
          (assq-delete-all variable so-long-original-values))
    (push (list variable
                (symbol-value variable)
                (local-variable-p variable))
          so-long-original-values)))

(defun so-long-change-major-mode ()
  "Ensures that `so-long-mode' knows the original `major-mode'
even when invoked interactively.

Called during `change-major-mode-hook'."
  (unless (or (minibufferp)
              (derived-mode-p 'so-long-mode))
    (so-long-remember 'major-mode)))

(defun so-long-menu ()
  "Dynamically generate the \"So Long\" menu."
  ;; (info "(elisp) Menu Example")
  (let ((map (make-sparse-keymap "So Long")))
    ;; `so-long-revert'.
    (define-key-after map [so-long-revert]
      '(menu-item "Revert to normal" so-long-menu-item-revert
                  :enable (and so-long-revert-function
                               so-long--active)))
    ;; `so-long-menu-item-replace-action' over `so-long-action-alist'.
    (define-key-after map [so-long-actions-separator]
      '(menu-item "--"))
    (dolist (item so-long-action-alist)
      (cl-destructuring-bind (key label actionfunc revertfunc)
          item
        (define-key-after map (vector key)
          `(menu-item
            ,label (lambda ()
                     (interactive)
                     (so-long-menu-item-replace-action ',item))
            :enable (not (and so-long--active
                              (eq ',actionfunc so-long-function)
                              (eq ',revertfunc so-long-revert-function)))))))
    map))

(defun so-long-menu-click-window ()
  "Return the window for a click in the So Long menu.

Commands in the mode-line menu may be triggered by mouse when some other window
is selected, so we need to make sure we are acting on the correct buffer."
  ;; Refer to (info "(elisp) Click Events") regarding the form of the mouse
  ;; position list for clicks in the mode line.
  (or (and (mouse-event-p last-nonmenu-event)
           (windowp (car (cadr last-nonmenu-event))) ; cXXXr only available
           (car (cadr last-nonmenu-event)))          ; since Emacs 26.1
      (selected-window)))

(defun so-long-menu-item-revert ()
  "Invoke `so-long-revert'."
  (interactive)
  (with-selected-window (so-long-menu-click-window)
    (so-long-revert)))

(defun so-long-menu-item-replace-action (replacement)
  "Revert the current action and invoke the specified replacement.

REPLACEMENT is a `so-long-action-alist' item."
  (interactive)
  (with-selected-window (so-long-menu-click-window)
    (when so-long--active
      (so-long-revert))
    (cl-destructuring-bind (_key _label actionfunc revertfunc)
        replacement
      (setq so-long-function actionfunc)
      (setq so-long-revert-function revertfunc)
      (setq this-command 'so-long)
      (so-long))))

(defvar-local so-long-mode-line-info nil
  "Mode line construct displayed when `so-long' has been triggered.

Displayed as part of `mode-line-misc-info'.

`so-long-mode-line-label' defines the text to be displayed (if any).

Face `so-long-mode-line-active' is used while mitigations are active, and
`so-long-mode-line-inactive' is used if `so-long-revert' is called.

Not displayed when `so-long-mode' is enabled, as the major mode construct
serves the same purpose.")

;; Ensure we can display text properties on this value in the mode line.
;; See (info "(elisp) Mode Line Data") or (info "(elisp) Properties in Mode").
(put 'so-long-mode-line-info 'risky-local-variable t)

(defun so-long-mode-line-info ()
  "Returns the mode line construct for variable `so-long-mode-line-info'."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<mode-line> <down-mouse-1>")
      `(menu-item "" nil
                  :filter ,(lambda (_cmd) (so-long-menu))))
    ;; Mode line construct.
    ;; n.b. It's necessary for `so-long-mode-line-info' to have a non-nil
    ;; risky-local-variable property, as otherwise the text properties won't
    ;; be rendered.
    `(so-long-mode-line-label
      ("" (:eval (propertize so-long-mode-line-label
                             'mouse-face 'highlight
                             'keymap ',map
                             'help-echo t ;; Suppress the mode-line value
                             'face (if so-long--active
                                       'so-long-mode-line-active
                                     'so-long-mode-line-inactive)))
       " "))))

;; When the line's long
;; When the mode's slow
;; When Emacs is sad
;; We change automatically to faster code
;; And then I won't feel so mad

(defun so-long-detected-long-line-p ()
  "Following any initial comments and blank lines, the next N lines of the
buffer will be tested for excessive length (where \"excessive\" means above
`so-long-threshold', and N is `so-long-max-lines').

Returns non-nil if any such excessive-length line is detected.

If `so-long-skip-leading-comments' is nil then the N lines will be counted
starting from the first line of the buffer.  In this instance you will likely
want to increase `so-long-max-lines' to allow for possible comments.

This is the default value of `so-long-predicate'."
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
                   (setq start (point)))))
        ;; We're at the first non-comment line, but we may have moved past
        ;; indentation whitespace, so move back to the beginning of the line.
        (forward-line 0))
      ;; Start looking for long lines.
      ;; `while' will ultimately return nil if we do not `throw' a result.
      (catch 'excessive
        (while (and (not (eobp))
                    (or (not so-long-max-lines)
                        (< count so-long-max-lines)))
          (setq start (point))
          (save-restriction
            (narrow-to-region start (min (+ start 1 so-long-threshold)
                                         (point-max)))
            (forward-line 1))
          ;; If point is not now at the beginning of a line, then the previous
          ;; line was long -- with the exception of when point is at the end of
          ;; the buffer (bearing in mind that we have widened again), in which
          ;; case there was a short final line with no newline.  There is an
          ;; edge case when such a final line is exactly (1+ so-long-threshold)
          ;; chars long, so if we're at (eobp) we need to verify the length in
          ;; order to be consistent.
          (unless (or (bolp)
                      (and (eobp) (<= (- (point) start)
                                      so-long-threshold)))
            (throw 'excessive t))
          (setq count (1+ count)))))))

(defun so-long-function-longlines-mode ()
  "Enable minor mode `longlines-mode'.

This is a `so-long-function' option."
  (require 'longlines)
  (so-long-remember 'longlines-mode)
  (longlines-mode 1))

(defun so-long-revert-function-longlines-mode ()
  "Restore original state of `longlines-mode'."
  (require 'longlines)
  (let ((state (so-long-original 'longlines-mode :exists)))
    (if state
        (unless (equal (cadr state) longlines-mode)
          (longlines-mode (if (cadr state) 1 0)))
      (longlines-mode 0))))

(defun so-long-function-overrides-only ()
  "Disable minor modes and override variables, but retain the major mode.

This is a `so-long-function' option."
  (so-long-disable-minor-modes)
  (so-long-override-variables))

(defun so-long-revert-function-overrides-only ()
  "Restore original state of the overridden minor modes and variables."
  (so-long-restore-minor-modes)
  (so-long-restore-variables))

;; How do you solve a problem like a long line?
;; How do you stop a mode from slowing down?
;; How do you cope with processing a long line?
;; A bit of advice! A mode! A workaround!

(defvar so-long-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'so-long-revert)
    ;; Define the major mode menu.  We have an awkward issue whereby
    ;; [menu-bar so-long] is already defined in the global map and is
    ;; :visible so-long-detected-p, but we also want this to be
    ;; available via the major mode construct in the mode line.
    ;; The following achieves the desired end result, as :visible nil
    ;; prevents this from duplicating its contents in the menu bar,
    ;; but still includes it in the mode line.
    (define-key map [menu-bar so-long]
      `(menu-item "" nil
                  :visible nil
                  :filter ,(lambda (_cmd) (so-long-menu))))
    map)
  "Major mode keymap and menu for `so-long-mode'.")

(define-derived-mode so-long-mode nil "So long"
  "This major mode is the default `so-long-action' option.

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
variables in accordance with `so-long-variable-overrides'.  These steps occur
in `after-change-major-mode-hook', so that minor modes controlled by globalized
minor modes can also be disabled.

Some globalized minor modes may be inhibited by acting in `so-long-mode-hook'.

By default this mode is essentially equivalent to `fundamental-mode', and
exists mainly to provide information to the user as to why the expected mode
was not used, and to facilitate hooks for other so-long functionality.

To revert to the original mode despite any potential performance issues,
type \\[so-long-mode-revert], or else re-invoke it manually."
  ;; Housekeeping.  `so-long-mode' might be invoked directly rather than via
  ;; `so-long', so replicate the necessary behaviours.  We could use this same
  ;; test in `so-long-after-change-major-mode' to run `so-long-hook', but that's
  ;; not so obviously the right thing to do, so I've omitted it for now.
  (unless so-long--calling
    (setq so-long--active t
          so-long-detected-p t
          so-long-function 'so-long-mode
          so-long-revert-function 'so-long-mode-revert)
    ;; Reset `so-long-original-values' with the exception of `major-mode'
    ;; which has just been stored by `so-long-change-major-mode'.
    (let ((major (so-long-original 'major-mode :exists)))
      (setq so-long-original-values (if major (list major) nil))))
  ;; Use `after-change-major-mode-hook' to disable minor modes and override
  ;; variables, so that we act after any globalized modes have acted.
  (add-hook 'after-change-major-mode-hook
            'so-long-after-change-major-mode :append :local)
  ;; Override variables.  This is the first of two instances where we do this
  ;; (the other being `so-long-after-change-major-mode').  It is desirable to
  ;; set variables here in order to cover cases where the setting of a variable
  ;; influences how a global minor mode behaves in this buffer.
  (so-long-override-variables)
  ;; Hide redundant mode-line information (our major mode info replicates this).
  (setq so-long-mode-line-info nil)
  ;; Inform the user about our major mode hijacking.
  (unless (or so-long--inhibited so-long--set-auto-mode)
    (message (concat "Changed to %s (from %s)"
                     (unless (or (eq this-command 'so-long)
                                 (and (symbolp this-command)
                                      (provided-mode-derived-p this-command
                                                               'so-long-mode)))
                       " on account of line length")
                     ".  %s to revert.")
             major-mode
             (or (so-long-original 'major-mode) "<unknown>")
             (substitute-command-keys "\\[so-long-revert]"))))

(defun so-long-after-change-major-mode ()
  "Run by `so-long-mode' in `after-change-major-mode-hook'.

Calls `so-long-disable-minor-modes' and `so-long-override-variables'."
  ;; Disable minor modes.
  (so-long-disable-minor-modes)
  ;; Override variables (again).  We already did this in `so-long-mode' in
  ;; order that variables which affect global/globalized minor modes can have
  ;; that effect; however it's feasible that one of the minor modes disabled
  ;; above might have reverted one of these variables, so we re-enforce them.
  ;; (For example, disabling `visual-line-mode' sets `line-move-visual' to
  ;; nil, when for our purposes it is preferable for it to be non-nil).
  (so-long-override-variables))

(defun so-long-disable-minor-modes ()
  "Disable any active minor modes listed in `so-long-minor-modes'."
  (dolist (mode so-long-minor-modes)
    (when (and (boundp mode) mode)
      (funcall mode 0))))

(defun so-long-restore-minor-modes ()
  "Restore the minor modes which were disabled.

The modes are enabled in accordance with what was remembered in `so-long'."
  (dolist (mode so-long-minor-modes)
    (when (and (so-long-original mode)
               (boundp mode)
               (not (symbol-value mode)))
      (funcall mode 1))))

(defun so-long-override-variables ()
  "Process `so-long-variable-overrides'."
  (dolist (ovar so-long-variable-overrides)
    (set (make-local-variable (car ovar)) (cdr ovar))))

(defun so-long-restore-variables ()
  "Restore the remembered values for the overridden variables."
  (dolist (ovar so-long-variable-overrides)
    (so-long-restore-variable (car ovar))))

(defun so-long-restore-variable (variable)
  "Restore the remembered value (if any) for VARIABLE."
  ;; In the instance where `so-long-mode-revert' has just reverted the major
  ;; mode, note that `kill-all-local-variables' was already called by the
  ;; original mode function, and so these 'overridden' variables may now have
  ;; global rather than buffer-local values.
  (let* ((remembered (so-long-original variable :exists))
         (originally-local (nth 2 remembered)))
    (if originally-local
        ;; The variable originally existed with a buffer-local value, so we
        ;; restore it as such (even if the global value is a match).
        (set (make-local-variable variable) (cadr remembered))
      ;; Either this variable did not exist initially, or it did not have a
      ;; buffer-local value at that time.  In either case we kill the current
      ;; buffer-local value (if any) in order to restore the original state.
      ;;
      ;; It's possible that the global value has *changed* in the interim; but
      ;; we can't know whether it's best to use the new global value, or retain
      ;; the old value as a buffer-local value, so we keep it simple.
      (kill-local-variable variable))))

(defun so-long-mode-revert ()
  "Call the `major-mode' which was selected before `so-long-mode' replaced it.

Re-process local variables, and restore overridden variables and minor modes."
  (interactive)
  (let ((so-long-original-mode (so-long-original 'major-mode)))
    (unless so-long-original-mode
      (error "Original mode unknown."))
    (funcall so-long-original-mode)
    ;; Emacs 26+ has already called `hack-local-variables' (during
    ;; `run-mode-hooks'), but for older versions we need to call it here.
    (when (< emacs-major-version 26)
      (hack-local-variables))
    ;; Restore minor modes.
    (so-long-restore-minor-modes)
    ;; Restore overridden variables.
    ;; `kill-all-local-variables' was already called by the original mode
    ;; function, so we may be seeing global values.
    (so-long-restore-variables)
    ;; Restore the mode line construct.
    (unless (derived-mode-p 'so-long-mode)
      (setq so-long-mode-line-info (so-long-mode-line-info)))))

(defun so-long-mode-downgrade (&optional _mode)
  "The default value for `so-long-file-local-mode-function'.

A buffer-local 'downgrade' from the `so-long-mode' action to `overrides-only'.

When `so-long-function' is set to `so-long-mode', then we change it to to
`so-long-function-overrides-only' instead -- retaining the file-local major
mode, but still doing everything else that `so-long-mode' would have done.
`so-long-revert-function' is likewise updated.

If `so-long-function' has any value other than `so-long-mode', we do nothing,
as if `so-long-file-local-mode-function' was nil."
  (when (and (symbolp (so-long-function))
             (provided-mode-derived-p (so-long-function) 'so-long-mode))
    ;; Downgrade from `so-long-mode' to the `overrides-only' behaviour.
    (setq so-long-function 'so-long-function-overrides-only
          so-long-revert-function 'so-long-revert-function-overrides-only)))

(defun so-long-inhibit (&optional _mode)
  "Prevent so-long from having any effect at all.

This is a `so-long-file-local-mode-function' option."
  (setq so-long--inhibited t))

(defun so-long-check-header-modes ()
  "Handles the header-comments processing in `set-auto-mode'.

`set-auto-mode' has some special-case code to handle the 'mode' pseudo-variable
when set in the header comment.  This runs outside of `hack-local-variables'
and cannot be conveniently intercepted, so we are forced to replicate it here.

This special-case code will ultimately be removed from Emacs, as it exists to
deal with a deprecated feature; but until then we need to replicate it in order
to inhibit our own behaviour in the presence of a header comment 'mode'
declaration.

If a file-local mode is detected in the header comment, then we call the
function defined by `so-long-file-local-mode-function'."
  ;; The following code for processing MODE declarations in the header
  ;; comments is copied verbatim from `set-auto-mode', because we have
  ;; no way of intercepting it.
  ;;
  (let ((try-locals (not (inhibit-local-variables-p)))
        end _done _mode modes)
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
    ;; were the case, so it is very unlikely.  For the purposes of passing a
    ;; value to `so-long-handle-file-local-mode' we assume the major mode was
    ;; the first mode specified (in which case it is the last in the list).
    (when modes
      (so-long-handle-file-local-mode (car (last modes))))))

;; Lisp advice, Lisp advice
;; Every calling you greet me
;; Code of mine, redefined
;; You look happy to tweak me

(defadvice hack-local-variables (after so-long--file-local-mode disable)
  "Ensure that `so-long' defers to file-local mode declarations if necessary.

This advice acts after the HANDLE-MODE:t call to `hack-local-variables'.
\(MODE-ONLY in Emacs versions < 26).

File-local header comments are currently an exception, and are processed by
`so-long-check-header-modes' (see which for details).

If a file-local mode is detected, then we call the function defined by
`so-long-file-local-mode-function'."
  ;; The first arg to `hack-local-variables' is HANDLE-MODE since Emacs 26.1,
  ;; and MODE-ONLY in earlier versions.  In either case we are interested in
  ;; whether it has the value `t'.
  (and (eq (ad-get-arg 0) t)
       ad-return-value ; A file-local mode was set.
       (so-long-handle-file-local-mode ad-return-value)))

;; n.b. Call (so-long-enable) after changes, to re-activate the advice.

(defadvice set-auto-mode (around so-long--set-auto-mode disable)
  "Maybe call `so-long' for files with very long lines.

This advice acts after `set-auto-mode' has set the buffer's major mode.

We can't act before this point, because some major modes must be exempt
\(binary file modes, for example).  Instead, we act only when the selected
major mode is a member (or derivative of a member) of `so-long-target-modes'.

`so-long-predicate' then determines whether the mode change is needed."
  (setq so-long--inhibited nil) ; is permanent-local
  (when so-long-enabled
    (so-long-check-header-modes)) ; may cause `so-long--inhibited' to be set.
  (let ((so-long--set-auto-mode t))
    ad-do-it) ; `set-auto-mode'   ; may cause `so-long--inhibited' to be set.
  ;; Test the new major mode for long lines.
  (and so-long-enabled
       (not so-long--inhibited)
       (not so-long--calling)
       (apply #'derived-mode-p so-long-target-modes)
       (setq so-long-detected-p (funcall so-long-predicate))
       (so-long)))

;; n.b. Call (so-long-enable) after changes, to re-activate the advice.

(defadvice hack-one-local-variable (around so-long--ignore-mode-dup disable)
  "Prevent the original major mode being restored after `so-long-mode'.

This advice is needed and enabled only for Emacs versions < 26.1.

If the local 'mode' pseudo-variable is used, `set-auto-mode-0' will call it
firstly, and subsequently `hack-one-local-variable' may call it again.

Usually `hack-one-local-variable' tries to avoid processing that second call,
by testing the value against `major-mode'; but as we may have changed the
major mode to `so-long-mode' by this point, that protection is insufficient
and so we need to perform our own test.

The changes to `normal-mode' in Emacs 26.1 modified the execution order, and
makes this advice unnecessary.  The relevant NEWS entry is:

** File local and directory local variables are now initialized each
time the major mode is set, not just when the file is first visited.
These local variables will thus not vanish on setting a major mode."
  (if (eq (ad-get-arg 0) 'mode)
      ;; Adapted directly from `hack-one-local-variable'
      (let ((mode (intern (concat (downcase (symbol-name (ad-get-arg 1)))
                                  "-mode"))))
        (unless (eq (indirect-function mode)
                    (indirect-function (so-long-original 'major-mode)))
          ad-do-it))
    ;; VAR is not the 'mode' pseudo-variable.
    ad-do-it))

;; n.b. Call (so-long-enable) after changes, to re-activate the advice.

;;;###autoload
(defun so-long ()
  "Invoke `so-long-action' and run `so-long-hook'."
  (interactive)
  (unless so-long--calling
    (let ((so-long--calling t))
      ;; Some of these settings need to be duplicated in `so-long-mode' to cover
      ;; the case when that mode is invoked directly.
      (setq so-long-detected-p t) ;; ensure menu is present.
      (unless so-long-function
        (setq so-long-function (so-long-function)))
      (unless so-long-revert-function
        (setq so-long-revert-function (so-long-revert-function)))
      ;; Remember original settings.
      (setq so-long-original-values nil)
      (dolist (ovar so-long-variable-overrides)
        (so-long-remember (car ovar)))
      (dolist (mode so-long-minor-modes)
        (when (and (boundp mode) mode)
          (so-long-remember mode)))
      ;; Call the configured `so-long-function'.
      (when (functionp so-long-function)
        (funcall so-long-function)
        ;; Set `so-long--active' last, as it isn't permanent-local.
        (setq so-long--active t))
      ;; Display mode line info, unless we are in `so-long-mode' (which provides
      ;; equivalent information in the mode line construct for the major mode).
      (unless (derived-mode-p 'so-long-mode)
        (setq so-long-mode-line-info (so-long-mode-line-info)))
      ;; Run `so-long-hook'.
      ;; By default we set `buffer-read-only', which can cause problems if hook
      ;; functions need to modify the buffer.  We use `inhibit-read-only' to
      ;; side-step the issue (and likewise in `so-long-revert').
      (let ((inhibit-read-only t))
        (run-hooks 'so-long-hook)))))

(defun so-long-revert ()
  "Revert `so-long-action' and run `so-long-revert-hook'."
  (interactive)
  (unless so-long--calling
    (let ((so-long--calling t))
      (when (functionp so-long-revert-function)
        (funcall so-long-revert-function)
        (setq so-long--active nil))
      (let ((inhibit-read-only t))
        (run-hooks 'so-long-revert-hook)))))

;;;###autoload
(defun so-long-enable ()
  "Enable the so-long library's functionality."
  (interactive)
  (add-hook 'change-major-mode-hook 'so-long-change-major-mode)
  (ad-enable-advice 'hack-local-variables 'after 'so-long--file-local-mode)
  (ad-enable-advice 'set-auto-mode 'around 'so-long--set-auto-mode)
  (ad-activate 'hack-local-variables)
  (ad-activate 'set-auto-mode)
  (when (< emacs-major-version 26)
    (ad-enable-advice 'hack-one-local-variable
                      'around 'so-long--ignore-mode-dup)
    (ad-activate 'hack-one-local-variable))
  (add-to-list 'mode-line-misc-info '("" so-long-mode-line-info))
  (define-key-after (current-global-map) [menu-bar so-long]
    `(menu-item "So Long" nil
                ;; See also `so-long-mode-map'.
                :visible (or so-long--active
                             so-long-detected-p
                             (derived-mode-p 'so-long-mode))
                :filter ,(lambda (_cmd) (so-long-menu))))
  (setq so-long-enabled t))

(defun so-long-disable ()
  "Disable the so-long library's functionality."
  (interactive)
  (remove-hook 'change-major-mode-hook 'so-long-change-major-mode)
  (ad-disable-advice 'hack-local-variables 'after 'so-long--file-local-mode)
  (ad-disable-advice 'set-auto-mode 'around 'so-long--set-auto-mode)
  (ad-activate 'hack-local-variables)
  (ad-activate 'set-auto-mode)
  (when (< emacs-major-version 26)
    (ad-disable-advice 'hack-one-local-variable
                       'around 'so-long--ignore-mode-dup)
    (ad-activate 'hack-one-local-variable))
  (setq mode-line-misc-info
        (delete '("" so-long-mode-line-info) mode-line-misc-info))
  (define-key (current-global-map) [menu-bar so-long] nil)
  (setq so-long-enabled nil))

(defun so-long-unload-function ()
  (so-long-disable)
  nil)

(provide 'so-long)

;; So long, farewell, auf wiedersehen, goodbye
;; You have to go, this code is minified
;; Goodbye!

;;; so-long.el ends here
