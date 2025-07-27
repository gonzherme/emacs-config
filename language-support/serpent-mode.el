;; ;;; serpent-mode.el -- Major mode for editing Serpent programs

;; ;; A slight modification to python-mode.el (credits below) to
;; ;; support "//" comments by Roger B. Dannenberg, May 2010

;; ;; To install for Aquamacs on OS X, put this file in 
;; ;;    ~/Library/Application\ Support/Emacs/site-lisp/serpent-mode.el
;; ;; and add the following lines to
;; ;;    ~/Library/Preferences/Aquamacs\ Emacs/Preferences.el
;; ;; -------------------------------------------------------------------------
;; ;; (setq auto-mode-alist (cons '("\\.srp$" . serpent-mode) auto-mode-alist))
;; ;; (setq interpreter-mode-alist (cons '("serpent" . serpent-mode) interpreter-mode-alist))
;; ;; (autoload 'serpent-mode "serpent-mode" "Serpent editing mode." t)
;; ;; -------------------------------------------------------------------------
;; ;; 
;; ;; To install on other exacs versions, comments below might help. Please send
;; ;; a note to Dannenberg if these instructions need additional information.
;; ;;
;; ;; =================== original heading and comments start here ============
;; ;;
;; ;;; python-mode.el --- Major mode for editing Python programs

;; ;; Copyright (C) 1992,1993,1994  Tim Peters

;; ;; Author: 2003-2007 http://sf.net/projects/python-mode
;; ;;         1995-2002 Barry A. Warsaw
;; ;;         1992-1994 Tim Peters
;; ;; Maintainer: python-mode@python.org
;; ;; Created:    Feb 1992
;; ;; Keywords:   python languages oop



;; (defconst srp-version "$Revision: 60621 $"
;;   "`serpent-mode' version number.")

;; ;; This software is provided as-is, without express or implied
;; ;; warranty.  Permission to use, copy, modify, distribute or sell this
;; ;; software, without fee, for any purpose and by any individual or
;; ;; organization, is hereby granted, provided that the above copyright
;; ;; notice and this paragraph appear in all copies.

;; ;;; Commentary:

;; ;; This is a major mode for editing Serpent programs.  It was developed by Tim
;; ;; Peters after an original idea by Michael A. Guravage.  Tim subsequently
;; ;; left the net and in 1995, Barry Warsaw inherited the mode.  Tim's now back
;; ;; but disavows all responsibility for the mode.  In fact, we suspect he
;; ;; doesn't even use Emacs any more.  In 2003, python-mode.el was moved to its
;; ;; own SourceForge project apart from the Python project, and now is
;; ;; maintained by the volunteers at the python-mode@python.org mailing list.

;; ;; pdbtrack support contributed by Ken Manheimer, April 2001.  Skip Montanaro
;; ;; has also contributed significantly to python-mode's development.

;; ;; Please use the SourceForge Python project to submit bugs or
;; ;; patches:
;; ;;
;; ;;     http://sourceforge.net/projects/python

;; ;; INSTALLATION:

;; ;; To install, just drop this file into a directory on your load-path and
;; ;; byte-compile it.  To set up Emacs to automatically edit files ending in
;; ;; ".py" using serpent-mode add the following to your ~/.emacs file (GNU
;; ;; Emacs) or ~/.xemacs/init.el file (XEmacs):
;; ;;    (setq auto-mode-alist (cons '("\\.py$" . serpent-mode) auto-mode-alist))
;; ;;    (setq interpreter-mode-alist (cons '("serpent" . serpent-mode)
;; ;;                                       interpreter-mode-alist))
;; ;;    (autoload 'serpent-mode "serpent-mode" "Serpent editing mode." t)
;; ;;
;; ;; In XEmacs syntax highlighting should be enabled automatically.  In GNU
;; ;; Emacs you may have to add these lines to your ~/.emacs file:
;; ;;    (global-font-lock-mode t)
;; ;;    (setq font-lock-maximum-decoration t)

;; ;; FOR MORE INFORMATION:

;; ;; There is some information on serpent-mode.el at

;; ;;     http://www.python.org/emacs/python-mode/
;; ;;
;; ;; It does contain links to other packages that you might find useful,
;; ;; such as pdb interfaces, OO-Browser links, etc.

;; ;; BUG REPORTING:

;; ;; As mentioned above, please use the SourceForge Python project for
;; ;; submitting bug reports or patches.  The old recommendation, to use
;; ;; C-c C-b will still work, but those reports have a higher chance of
;; ;; getting buried in my mailbox.  Please include a complete, but
;; ;; concise code sample and a recipe for reproducing the bug.  Send
;; ;; suggestions and other comments to python-mode@python.org.

;; ;; When in a Python mode buffer, do a C-h m for more help.  It's
;; ;; doubtful that a texinfo manual would be very useful, but if you
;; ;; want to contribute one, I'll certainly accept it!

;; ;;; Code:

;; (require 'comint)
;; (require 'custom)
;; ;;(require 'cl) ;; Deprecated package
;; (require 'compile)
;; (require 'ansi-color)

;; ;; user definable variables
;; ;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

;; (defgroup serpent nil
;;   "Support for the Serpent programming language <http://www.cs.cmu.edu/~music/aura/serpent-info.htm>"
;;   :group 'languages
;;   :prefix "srp-")

;; (defcustom srp-tab-always-indent t
;;   "*Non-nil means TAB in Serpent mode should always reindent the current line,
;; regardless of where in the line point is when the TAB command is used."
;;   :type 'boolean
;;   :group 'serpent)

;; (defcustom srp-serpent-command "serpent"
;;   "*Shell command used to start Serpent interpreter."
;;   :type 'string
;;   :group 'serpent)

;; ;; (make-obsolete-variable 'srp-jserpent-command 'srp-jython-command)
;; (defcustom srp-jython-command "jython"
;;   "*Shell command used to start the Jython interpreter."
;;   :type 'string
;;   :group 'serpent
;;   :tag "Jython Command")

;; (defcustom srp-default-interpreter 'cserpent
;;   "*Which Serpent interpreter is used by default.
;; The value for this variable can be either `cserpent' or `jython'.

;; When the value is `cserpent', the variables `srp-serpent-command' and
;; `srp-serpent-command-args' are consulted to determine the interpreter
;; and arguments to use.

;; When the value is `jython', the variables `srp-jython-command' and
;; `srp-jython-command-args' are consulted to determine the interpreter
;; and arguments to use.

;; Note that this variable is consulted only the first time that a Serpent
;; mode buffer is visited during an Emacs session.  After that, use
;; \\[srp-toggle-shells] to change the interpreter shell."
;;   :type '(choice (const :tag "Serpent (a.k.a. CSerpent)" cserpent)
;; 		 (const :tag "Jython" jython))
;;   :group 'serpent)

;; (defcustom srp-serpent-command-args '("-i")
;;   "*List of string arguments to be used when starting a Serpent shell."
;;   :type '(repeat string)
;;   :group 'serpent)

;; ;; (make-obsolete-variable 'srp-jserpent-command-args 'srp-jython-command-args)
;; (defcustom srp-jython-command-args '("-i")
;;   "*List of string arguments to be used when starting a Jython shell."
;;   :type '(repeat string)
;;   :group 'serpent
;;   :tag "Jython Command Args")

;; (defcustom srp-indent-offset 4
;;   "*Amount of offset per level of indentation.
;; `\\[srp-guess-indent-offset]' can usually guess a good value when
;; you're editing someone else's Serpent code."
;;   :type 'integer
;;   :group 'serpent)

;; (defcustom srp-continuation-offset 4
;;   "*Additional amount of offset to give for some continuation lines.
;; Continuation lines are those that immediately follow a backslash
;; terminated line.  Only those continuation lines for a block opening
;; statement are given this extra offset."
;;   :type 'integer
;;   :group 'serpent)

;; (defcustom srp-smart-indentation t
;;   "*Should `serpent-mode' try to automagically set some indentation variables?
;; When this variable is non-nil, two things happen when a buffer is set
;; to `serpent-mode':

;;     1. `srp-indent-offset' is guessed from existing code in the buffer.
;;        Only guessed values between 2 and 8 are considered.  If a valid
;;        guess can't be made (perhaps because you are visiting a new
;;        file), then the value in `srp-indent-offset' is used.

;;     2. `indent-tabs-mode' is turned off if `srp-indent-offset' does not
;;        equal `tab-width' (`indent-tabs-mode' is never turned on by
;;        Serpent mode).  This means that for newly written code, tabs are
;;        only inserted in indentation if one tab is one indentation
;;        level, otherwise only spaces are used.

;; Note that both these settings occur *after* `serpent-mode-hook' is run,
;; so if you want to defeat the automagic configuration, you must also
;; set `srp-smart-indentation' to nil in your `serpent-mode-hook'."
;;   :type 'boolean
;;   :group 'serpent)

;; (defcustom srp-align-multiline-strings-p t
;;   "*Flag describing how multi-line triple quoted strings are aligned.
;; When this flag is non-nil, continuation lines are lined up under the
;; preceding line's indentation.  When this flag is nil, continuation
;; lines are aligned to column zero."
;;   :type '(choice (const :tag "Align under preceding line" t)
;; 		 (const :tag "Align to column zero" nil))
;;   :group 'serpent)

;; (defcustom srp-block-comment-prefix "##"
;;   "*String used by \\[comment-region] to comment out a block of code.
;; This should follow the convention for non-indenting comment lines so
;; that the indentation commands won't get confused (i.e., the string
;; should be of the form `#x...' where `x' is not a blank or a tab, and
;; `...' is arbitrary).  However, this string should not end in whitespace."
;;   :type 'string
;;   :group 'serpent)

;; (defcustom srp-honor-comment-indentation t
;;   "*Controls how comment lines influence subsequent indentation.

;; When nil, all comment lines are skipped for indentation purposes, and
;; if possible, a faster algorithm is used (i.e. X/Emacs 19 and beyond).

;; When t, lines that begin with a single `#' are a hint to subsequent
;; line indentation.  If the previous line is such a comment line (as
;; opposed to one that starts with `srp-block-comment-prefix'), then its
;; indentation is used as a hint for this line's indentation.  Lines that
;; begin with `srp-block-comment-prefix' are ignored for indentation
;; purposes.

;; When not nil or t, comment lines that begin with a single `#' are used
;; as indentation hints, unless the comment character is in column zero."
;;   :type '(choice
;; 	  (const :tag "Skip all comment lines (fast)" nil)
;; 	  (const :tag "Single # `sets' indentation for next line" t)
;; 	  (const :tag "Single # `sets' indentation except at column zero"
;; 		 other)
;; 	  )
;;   :group 'serpent)

;; (defcustom srp-temp-directory
;;   (let ((ok '(lambda (x)
;; 	       (and x
;; 		    (setq x (expand-file-name x)) ; always true
;; 		    (file-directory-p x)
;; 		    (file-writable-p x)
;; 		    x))))
;;     (or (funcall ok (getenv "TMPDIR"))
;; 	(funcall ok "/usr/tmp")
;; 	(funcall ok "/tmp")
;; 	(funcall ok "/var/tmp")
;; 	(funcall ok  ".")
;; 	(error
;; 	 "Couldn't find a usable temp directory -- set `srp-temp-directory'")))
;;   "*Directory used for temporary files created by a *Serpent* process.
;; By default, the first directory from this list that exists and that you
;; can write into: the value (if any) of the environment variable TMPDIR,
;; /usr/tmp, /tmp, /var/tmp, or the current directory."
;;   :type 'string
;;   :group 'serpent)

;; (defcustom srp-beep-if-tab-change t
;;   "*Ring the bell if `tab-width' is changed.
;; If a comment of the form

;;   \t# vi:set tabsize=<number>:

;; is found before the first code line when the file is entered, and the
;; current value of (the general Emacs variable) `tab-width' does not
;; equal <number>, `tab-width' is set to <number>, a message saying so is
;; displayed in the echo area, and if `srp-beep-if-tab-change' is non-nil
;; the Emacs bell is also rung as a warning."
;;   :type 'boolean
;;   :group 'serpent)

;; (defcustom srp-jump-on-exception t
;;   "*Jump to innermost exception frame in *Serpent Output* buffer.
;; When this variable is non-nil and an exception occurs when running
;; Serpent code synchronously in a subprocess, jump immediately to the
;; source code of the innermost traceback frame."
;;   :type 'boolean
;;   :group 'serpent)

;; (defcustom srp-ask-about-save t
;;   "If not nil, ask about which buffers to save before executing some code.
;; Otherwise, all modified buffers are saved without asking."
;;   :type 'boolean
;;   :group 'serpent)

;; (defcustom srp-backspace-function 'backward-delete-char-untabify
;;   "*Function called by `srp-electric-backspace' when deleting backwards."
;;   :type 'function
;;   :group 'serpent)

;; (defcustom srp-delete-function 'delete-char
;;   "*Function called by `srp-electric-delete' when deleting forwards."
;;   :type 'function
;;   :group 'serpent)

;; (defcustom srp-imenu-show-method-args-p nil
;;   "*Controls echoing of arguments of functions & methods in the Imenu buffer.
;; When non-nil, arguments are printed."
;;   :type 'boolean
;;   :group 'serpent)
;; (make-variable-buffer-local 'srp-indent-offset)

;; (defcustom srp-pdbtrack-do-tracking-p t
;;   "*Controls whether the pdbtrack feature is enabled or not.
;; When non-nil, pdbtrack is enabled in all comint-based buffers,
;; e.g. shell buffers and the *Serpent* buffer.  When using pdb to debug a
;; Serpent program, pdbtrack notices the pdb prompt and displays the
;; source file and line that the program is stopped at, much the same way
;; as gud-mode does for debugging C programs with gdb."
;;   :type 'boolean
;;   :group 'serpent)
;; (make-variable-buffer-local 'srp-pdbtrack-do-tracking-p)

;; (defcustom srp-pdbtrack-minor-mode-string " PDB"
;;   "*String to use in the minor mode list when pdbtrack is enabled."
;;   :type 'string
;;   :group 'serpent)

;; (defcustom srp-import-check-point-max
;;   20000
;;   "Maximum number of characters to search for a Java-ish import statement.
;; When `serpent-mode' tries to calculate the shell to use (either a
;; CSerpent or a Jython shell), it looks at the so-called `shebang' line
;; -- i.e. #! line.  If that's not available, it looks at some of the
;; file heading imports to see if they look Java-like."
;;   :type 'integer
;;   :group 'serpent
;;   )

;; ;; (make-obsolete-variable 'srp-jserpent-packages 'srp-jython-packages)
;; (defcustom srp-jython-packages
;;   '("java" "javax" "org" "com")
;;   "Imported packages that imply `jython-mode'."
;;   :type '(repeat string)
;;   :group 'serpent)

;; ;; Not customizable
;; (defvar srp-master-file nil
;;   "If non-nil, execute the named file instead of the buffer's file.
;; The intent is to allow you to set this variable in the file's local
;; variable section, e.g.:

;;     # Local Variables:
;;     # srp-master-file: \"master.py\"
;;     # End:

;; so that typing \\[srp-execute-buffer] in that buffer executes the named
;; master file instead of the buffer's file.  If the file name has a
;; relative path, the value of variable `default-directory' for the
;; buffer is prepended to come up with a file name.")
;; (make-variable-buffer-local 'srp-master-file)

;; (defcustom srp-pychecker-command "pychecker"
;;   "*Shell command used to run Pychecker."
;;   :type 'string
;;   :group 'serpent
;;   :tag "Pychecker Command")

;; (defcustom srp-pychecker-command-args '("--stdlib")
;;   "*List of string arguments to be passed to pychecker."
;;   :type '(repeat string)
;;   :group 'serpent
;;   :tag "Pychecker Command Args")

;; (defvar srp-shell-alist
;;   '(("jython" . 'jython)
;;     ("serpent" . 'cserpent))
;;   "*Alist of interpreters and serpent shells. Used by `srp-choose-shell'
;; to select the appropriate serpent interpreter mode for a file.")

;; (defcustom srp-shell-input-prompt-1-regexp "^>>> "
;;   "*A regular expression to match the input prompt of the shell."
;;   :type 'string
;;   :group 'serpent)

;; (defcustom srp-shell-input-prompt-2-regexp "^[.][.][.] "
;;   "*A regular expression to match the input prompt of the shell after the
;;   first line of input."
;;   :type 'string
;;   :group 'serpent)

;; (defcustom srp-shell-switch-buffers-on-execute t
;;   "*Controls switching to the Serpent buffer where commands are
;;   executed.  When non-nil the buffer switches to the Serpent buffer, if
;;   not no switching occurs."
;;   :type 'boolean
;;   :group 'serpent)


;; ;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; ;; NO USER DEFINABLE VARIABLES BEYOND THIS POINT

;; (defvar srp-line-number-offset 0
;;   "When an exception occurs as a result of srp-execute-region, a
;; subsequent srp-up-exception needs the line number where the region
;; started, in order to jump to the correct file line.  This variable is
;; set in srp-execute-region and used in srp-jump-to-exception.")

;; (defconst srp-emacs-features
;;   (let (features)
;;    features)
;;   "A list of features extant in the Emacs you are using.
;; There are many flavors of Emacs out there, with different levels of
;; support for features needed by `serpent-mode'.")

;; ;; Face for None, True, False, self, and Ellipsis
;; (defvar srp-pseudo-keyword-face 'srp-pseudo-keyword-face
;;   "Face for pseudo keywords in Serpent mode, like self, True, False, Ellipsis.")
;; (make-face 'srp-pseudo-keyword-face)

;; ;; PEP 318 decorators
;; (defvar srp-decorators-face 'srp-decorators-face
;;   "Face method decorators.")
;; (make-face 'srp-decorators-face)

;; ;; Face for builtins
;; (defvar srp-builtins-face 'srp-builtins-face
;;   "Face for builtins like TypeError, object, open, and exec.")
;; (make-face 'srp-builtins-face)

;; ;; XXX, TODO, and FIXME comments and such
;; (defvar srp-XXX-tag-face 'srp-XXX-tag-face
;;   "Face for XXX, TODO, and FIXME tags")
;; (make-face 'srp-XXX-tag-face)

;; (defun srp-font-lock-mode-hook ()
;;   (or (face-differs-from-default-p 'srp-pseudo-keyword-face)
;;       (copy-face 'font-lock-keyword-face 'srp-pseudo-keyword-face))
;;   (or (face-differs-from-default-p 'srp-builtins-face)
;;       (copy-face 'font-lock-keyword-face 'srp-builtins-face))
;;   (or (face-differs-from-default-p 'srp-decorators-face)
;;       (copy-face 'srp-pseudo-keyword-face 'srp-decorators-face))
;;   (or (face-differs-from-default-p 'srp-XXX-tag-face)
;;       (copy-face 'font-lock-comment-face 'srp-XXX-tag-face))
;;   )
;; (add-hook 'font-lock-mode-hook 'srp-font-lock-mode-hook)

;; (defvar serpent-font-lock-keywords
;;   (let ((kw1 (mapconcat 'identity
;; 			'("and"      "assert"   "break"   "class"
;; 			  "continue" "def"      "del"     "elif"
;; 			  "else"     "except"   "exec"    "for"
;; 			  "from"     "global"   "if"      "import"
;; 			  "in"       "is"       "lambda"  "not"
;; 			  "or"       "pass"     "print"   "raise"
;; 			  "return"   "while"    "with"    "yield"
;; 			  )
;; 			"\\|"))
;; 	(kw2 (mapconcat 'identity
;; 			'("else:" "except:" "finally:" "try:")
;; 			"\\|"))
;; 	(kw3 (mapconcat 'identity
;; 			'("ArithmeticError" "AssertionError"
;; 			  "AttributeError" "DeprecationWarning" "EOFError"
;; 			  "Ellipsis" "EnvironmentError" "Exception" "False"
;; 			  "FloatingPointError" "FutureWarning" "IOError"
;; 			  "ImportError" "IndentationError" "IndexError"
;; 			  "KeyError" "KeyboardInterrupt" "LookupError"
;; 			  "MemoryError" "NameError" "None" "NotImplemented"
;; 			  "NotImplementedError" "OSError" "OverflowError"
;; 			  "OverflowWarning" "PendingDeprecationWarning"
;; 			  "ReferenceError" "RuntimeError" "RuntimeWarning"
;; 			  "StopIteration" "SyntaxError"
;; 			  "SyntaxWarning" "SystemError" "SystemExit"
;; 			  "TabError" "True" "TypeError" "UnboundLocalError"
;; 			  "UnicodeDecodeError" "UnicodeEncodeError"
;; 			  "UnicodeError" "UnicodeTranslateError"
;; 			  "UserWarning" "ValueError" "Warning"
;; 			  "ZeroDivisionError" "__debug__"
;; 			  "__import__" "__name__" "abs" "apply" "basestring"
;; 			  "bool" "buffer" "callable" "chr" "classmethod"
;; 			  "cmp" "compile" "complex" "copyright"
;; 			  "delattr" "dict" "dir" "divmod"
;; 			  "enumerate" "eval" "exit" "file"
;; 			  "filter" "float" "getattr" "globals" "hasattr"
;; 			  "hash" "hex" "id" "int"
;; 			  "isinstance" "issubclass" "iter" "len" "license"
;; 			  "list" "locals" "long" "map" "max" "min" "object"
;; 			  "oct" "open" "ord" "pow" "property" "range"
;; 			  "reload" "repr" "round"
;; 			  "setattr" "slice" "staticmethod" "str" "sum"
;; 			  "super" "tuple" "type" "unichr" "unicode" "vars"
;; 			  "zip")
;; 			"\\|"))
;; 	(kw4 (mapconcat 'identity
;; 			;; Exceptions and warnings
;; 			'("ArithmeticError" "AssertionError"
;; 			  "AttributeError" "DeprecationWarning" "EOFError"
;; 			  "EnvironmentError" "Exception"
;; 			  "FloatingPointError" "FutureWarning" "IOError"
;; 			  "ImportError" "IndentationError" "IndexError"
;; 			  "KeyError" "KeyboardInterrupt" "LookupError"
;; 			  "MemoryError" "NameError" "NotImplemented"
;; 			  "NotImplementedError" "OSError" "OverflowError"
;; 			  "OverflowWarning" "PendingDeprecationWarning"
;; 			  "ReferenceError" "RuntimeError" "RuntimeWarning"
;; 			  "StandardError" "StopIteration" "SyntaxError"
;; 			  "SyntaxWarning" "SystemError" "SystemExit"
;; 			  "TabError" "TypeError" "UnboundLocalError"
;; 			  "UnicodeDecodeError" "UnicodeEncodeError"
;; 			  "UnicodeError" "UnicodeTranslateError"
;; 			  "UserWarning" "ValueError" "Warning"
;; 			  "ZeroDivisionError")
;; 			"\\|"))
;; 	)
;;     (list
;;      '("^[ \t]*\\(@.+\\)" 1 'srp-decorators-face)
;;      ;; keywords
;;      (cons (concat "\\<\\(" kw1 "\\)\\>[ \n\t(]") 1)
;;      ;; builtins when they don't appear as object attributes
;;      (list (concat "\\([^. \t]\\|^\\)[ \t]*\\<\\(" kw3 "\\)\\>[ \n\t(]") 2
;; 	   'srp-builtins-face)
;;      ;; block introducing keywords with immediately following colons.
;;      ;; Yes "except" is in both lists.
;;      (cons (concat "\\<\\(" kw2 "\\)[ \n\t(]") 1)
;;      ;; Exceptions
;;      (list (concat "\\<\\(" kw4 "\\)[ \n\t:,(]") 1 'srp-builtins-face)
;;      ;; `as' but only in "import foo as bar" or "with foo as bar"
;;      '("[ \t]*\\(\\<from\\>.*\\)?\\<import\\>.*\\<\\(as\\)\\>" . 2)
;;      '("[ \t]*\\<with\\>.*\\<\\(as\\)\\>" . 1)
;;      ;; classes
;;      '("\\<class[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)" 1 font-lock-type-face)
;;      ;; functions
;;      '("\\<def[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"
;;        1 font-lock-function-name-face)
;;      ;; pseudo-keywords
;;      '("\\<\\(self\\|None\\|True\\|False\\|Ellipsis\\)\\>"
;;        1 srp-pseudo-keyword-face)
;;      ;; XXX, TODO, and FIXME tags
;;      '("XXX\\|TODO\\|FIXME" 0 srp-XXX-tag-face t)
;;      ))
;;   "Additional expressions to highlight in Serpent mode.")
;; (put 'serpent-mode 'font-lock-defaults '(serpent-font-lock-keywords))

;; ;; have to bind srp-file-queue before installing the kill-emacs-hook
;; (defvar srp-file-queue nil
;;   "Queue of Serpent temp files awaiting execution.
;; Currently-active file is at the head of the list.")

;; (defvar srp-pdbtrack-is-tracking-p nil)

;; (defvar srp-pychecker-history nil)



;; ;; Constants

;; (defconst srp-stringlit-re
;;   (concat
;;    ;; These fail if backslash-quote ends the string (not worth
;;    ;; fixing?).  They precede the short versions so that the first two
;;    ;; quotes don't look like an empty short string.
;;    ;;
;;    ;; (maybe raw), long single quoted triple quoted strings (SQTQ),
;;    ;; with potential embedded single quotes
;;    "[rR]?'''[^']*\\(\\('[^']\\|''[^']\\)[^']*\\)*'''"
;;    "\\|"
;;    ;; (maybe raw), long double quoted triple quoted strings (DQTQ),
;;    ;; with potential embedded double quotes
;;    "[rR]?\"\"\"[^\"]*\\(\\(\"[^\"]\\|\"\"[^\"]\\)[^\"]*\\)*\"\"\""
;;    "\\|"
;;    "[rR]?'\\([^'\n\\]\\|\\\\.\\)*'"	; single-quoted
;;    "\\|"				; or
;;    "[rR]?\"\\([^\"\n\\]\\|\\\\.\\)*\""	; double-quoted
;;    )
;;   "Regular expression matching a Serpent string literal.")

;; (defconst srp-continued-re
;;   ;; This is tricky because a trailing backslash does not mean
;;   ;; continuation if it's in a comment
;;   (concat
;;    "\\(" "[^#'\"\n\\]" "\\|" srp-stringlit-re "\\)*"
;;    "\\\\$")
;;   "Regular expression matching Serpent backslash continuation lines.")

;; (defconst srp-blank-or-comment-re "[ \t]*\\($\\|#\\|//\\)"
;;   "Regular expression matching a blank or comment line.")

;; (defconst srp-outdent-re
;;   (concat "\\(" (mapconcat 'identity
;; 			   '("else:"
;; 			     "except\\(\\s +.*\\)?:"
;; 			     "finally:"
;; 			     "elif\\s +.*:")
;; 			   "\\|")
;; 	  "\\)")
;;   "Regular expression matching statements to be dedented one level.")

;; (defconst srp-block-closing-keywords-re
;;   "\\(return\\|raise\\|break\\|continue\\|pass\\)"
;;   "Regular expression matching keywords which typically close a block.")

;; (defconst srp-no-outdent-re
;;   (concat
;;    "\\("
;;    (mapconcat 'identity
;; 	      (list "try:"
;; 		    "except\\(\\s +.*\\)?:"
;; 		    "while\\s +.*:"
;; 		    "for\\s +.*:"
;; 		    "if\\s +.*:"
;; 		    "elif\\s +.*:"
;; 		    (concat srp-block-closing-keywords-re "[ \t\n]")
;; 		    )
;; 	      "\\|")
;; 	  "\\)")
;;   "Regular expression matching lines not to dedent after.")

;; (defvar srp-traceback-line-re
;;   "[ \t]+File \"\\([^\"]+\\)\", line \\([0-9]+\\)"
;;   "Regular expression that describes tracebacks.")

;; ;; pdbtrack constants
;; (defconst srp-pdbtrack-stack-entry-regexp
;; ;  "^> \\([^(]+\\)(\\([0-9]+\\))\\([?a-zA-Z0-9_]+\\)()"
;;   "^> \\(.*\\)(\\([0-9]+\\))\\([?a-zA-Z0-9_]+\\)()"
;;   "Regular expression pdbtrack uses to find a stack trace entry.")

;; (defconst srp-pdbtrack-input-prompt "\n[(<]*[Pp]db[>)]+ "
;;   "Regular expression pdbtrack uses to recognize a pdb prompt.")

;; (defconst srp-pdbtrack-track-range 10000
;;   "Max number of characters from end of buffer to search for stack entry.")


;; ;; Major mode boilerplate

;; ;; define a mode-specific abbrev table for those who use such things
;; (defvar serpent-mode-abbrev-table nil
;;   "Abbrev table in use in `serpent-mode' buffers.")
;; (define-abbrev-table 'serpent-mode-abbrev-table nil)

;; (defvar serpent-mode-hook nil
;;   "*Hook called by `serpent-mode'.")

;; ;; (make-obsolete-variable 'jserpent-mode-hook 'jython-mode-hook)
;; (defvar jython-mode-hook nil
;;   "*Hook called by `jython-mode'. `jython-mode' also calls
;; `serpent-mode-hook'.")

;; (defvar srp-shell-hook nil
;;   "*Hook called by `srp-shell'.")

;; ;; In previous version of serpent-mode.el, the hook was incorrectly
;; ;; called srp-mode-hook, and was not defvar'd.  Deprecate its use.
;; ;; (and (fboundp 'make-obsolete-variable)
;; ;;      (make-obsolete-variable 'srp-mode-hook 'serpent-mode-hook))

;; (defvar srp-mode-map ()
;;   "Keymap used in `serpent-mode' buffers.")
;; (if srp-mode-map
;;     nil
;;   (setq srp-mode-map (make-sparse-keymap))
;;   ;; electric keys
;;   (define-key srp-mode-map ":" 'srp-electric-colon)
;;   ;; indentation level modifiers
;;   (define-key srp-mode-map "\C-c\C-l"  'srp-shift-region-left)
;;   (define-key srp-mode-map "\C-c\C-r"  'srp-shift-region-right)
;;   (define-key srp-mode-map "\C-c<"     'srp-shift-region-left)
;;   (define-key srp-mode-map "\C-c>"     'srp-shift-region-right)
;;   ;; subprocess commands
;;   (define-key srp-mode-map "\C-c\C-c"  'srp-execute-buffer)
;;   (define-key srp-mode-map "\C-c\C-m"  'srp-execute-import-or-reload)
;;   (define-key srp-mode-map "\C-c\C-s"  'srp-execute-string)
;;   (define-key srp-mode-map "\C-c|"     'srp-execute-region)
;;   (define-key srp-mode-map "\e\C-x"    'srp-execute-def-or-class)
;;   (define-key srp-mode-map "\C-c!"     'srp-shell)
;;   (define-key srp-mode-map "\C-c\C-t"  'srp-toggle-shells)
;;   ;; Caution!  Enter here at your own risk.  We are trying to support
;;   ;; several behaviors and it gets disgusting. :-( This logic ripped
;;   ;; largely from CC Mode.
;;   ;;
;;   ;; In XEmacs 19, Emacs 19, and Emacs 20, we use this to bind
;;   ;; backwards deletion behavior to DEL, which both Delete and
;;   ;; Backspace get translated to.  There's no way to separate this
;;   ;; behavior in a clean way, so deal with it!  Besides, it's been
;;   ;; this way since the dawn of time.
;;   (if (not (boundp 'delete-key-deletes-forward))
;;       (define-key srp-mode-map "\177" 'srp-electric-backspace)
;;     ;; However, XEmacs 20 actually achieved enlightenment.  It is
;;     ;; possible to sanely define both backward and forward deletion
;;     ;; behavior under X separately (TTYs are forever beyond hope, but
;;     ;; who cares?  XEmacs 20 does the right thing with these too).
;;     (define-key srp-mode-map [delete]    'srp-electric-delete)
;;     (define-key srp-mode-map [backspace] 'srp-electric-backspace))
;;   ;; Separate M-BS from C-M-h.  The former should remain
;;   ;; backward-kill-word.
;;   (define-key srp-mode-map [(control meta h)] 'srp-mark-def-or-class)
;;   (define-key srp-mode-map "\C-c\C-k"  'srp-mark-block)
;;   ;; Miscellaneous
;;   (define-key srp-mode-map "\C-c:"     'srp-guess-indent-offset)
;;   (define-key srp-mode-map "\C-c\t"    'srp-indent-region)
;;   (define-key srp-mode-map "\C-c\C-d"  'srp-pdbtrack-toggle-stack-tracking)
;;   (define-key srp-mode-map "\C-c\C-n"  'srp-next-statement)
;;   (define-key srp-mode-map "\C-c\C-p"  'srp-previous-statement)
;;   (define-key srp-mode-map "\C-c\C-u"  'srp-goto-block-up)
;;   (define-key srp-mode-map "\C-c#"     'srp-comment-region)
;;   (define-key srp-mode-map "\C-c?"     'srp-describe-mode)
;;   (define-key srp-mode-map "\C-c\C-h"  'srp-help-at-point)
;;   (define-key srp-mode-map "\e\C-a"    'srp-beginning-of-def-or-class)
;;   (define-key srp-mode-map "\e\C-e"    'srp-end-of-def-or-class)
;;   (define-key srp-mode-map "\C-c-"     'srp-up-exception)
;;   (define-key srp-mode-map "\C-c="     'srp-down-exception)
;;   ;; stuff that is `standard' but doesn't interface well with
;;   ;; serpent-mode, which forces us to rebind to special commands
;;   (define-key srp-mode-map "\C-xnd"    'srp-narrow-to-defun)
;;   ;; information
;;   (define-key srp-mode-map "\C-c\C-b" 'srp-submit-bug-report)
;;   (define-key srp-mode-map "\C-c\C-v" 'srp-version)
;;   (define-key srp-mode-map "\C-c\C-w" 'srp-pychecker-run)
;;   ;; shadow global bindings for newline-and-indent w/ the srp- version.
;;   ;; BAW - this is extremely bad form, but I'm not going to change it
;;   ;; for now.
;;   ;;;; TRY TO DISABLE FANCY REINDENT CURRENT LINE -RBD
;;   ;;;; (mapcar #'(lambda (key)
;;   ;;;;	      (define-key srp-mode-map key 'srp-newline-and-indent))
;;   ;;;;	  (where-is-internal 'newline-and-indent))
;;   ;; Force RET to be srp-newline-and-indent even if it didn't get
;;   ;; mapped by the above code.  motivation: Emacs' default binding for
;;   ;; RET is `newline' and C-j is `newline-and-indent'.  Most Serpenteers
;;   ;; expect RET to do a `srp-newline-and-indent' and any Emacsers who
;;   ;; dislike this are probably knowledgeable enough to do a rebind.
;;   ;; However, we do *not* change C-j since many Emacsers have already
;;   ;; swapped RET and C-j and they don't want C-j bound to `newline' to
;;   ;; change.
;;   ;;;; TRY TO DISABLE FANCY REINDENT CURRENT LINE -RBD
;;   ;;;; (define-key srp-mode-map "\C-m" 'srp-newline-and-indent)
;;   )

;; (defvar srp-mode-output-map nil
;;   "Keymap used in *Serpent Output* buffers.")
;; (if srp-mode-output-map
;;     nil
;;   (setq srp-mode-output-map (make-sparse-keymap))
;;   (define-key srp-mode-output-map [button2]  'srp-mouseto-exception)
;;   (define-key srp-mode-output-map "\C-c\C-c" 'srp-goto-exception)
;;   ;; TBD: Disable all self-inserting keys.  This is bogus, we should
;;   ;; really implement this as *Serpent Output* buffer being read-only
;;   (mapc #' (lambda (key)
;; 	       (define-key srp-mode-output-map key
;; 		 #'(lambda () (interactive) (beep))))
;; 	     (where-is-internal 'self-insert-command))
;;   )

;; (defvar srp-shell-map nil
;;   "Keymap used in *Serpent* shell buffers.")
;; (if srp-shell-map
;;     nil
;;   (setq srp-shell-map (copy-keymap comint-mode-map))
;;   (define-key srp-shell-map [tab]   'tab-to-tab-stop)
;;   (define-key srp-shell-map "\C-c-" 'srp-up-exception)
;;   (define-key srp-shell-map "\C-c=" 'srp-down-exception)
;;   )

;; (defvar srp-mode-syntax-table nil
;;   "Syntax table used in `serpent-mode' buffers.")
;; (when (not srp-mode-syntax-table)
;;   (setq srp-mode-syntax-table (make-syntax-table))
;;   (modify-syntax-entry ?\( "()" srp-mode-syntax-table)
;;   (modify-syntax-entry ?\) ")(" srp-mode-syntax-table)
;;   (modify-syntax-entry ?\[ "(]" srp-mode-syntax-table)
;;   (modify-syntax-entry ?\] ")[" srp-mode-syntax-table)
;;   (modify-syntax-entry ?\{ "(}" srp-mode-syntax-table)
;;   (modify-syntax-entry ?\} "){" srp-mode-syntax-table)
;;   ;; Add operator symbols misassigned in the std table
;;   (modify-syntax-entry ?\$ "."  srp-mode-syntax-table)
;;   (modify-syntax-entry ?\% "."  srp-mode-syntax-table)
;;   (modify-syntax-entry ?\& "."  srp-mode-syntax-table)
;;   (modify-syntax-entry ?\* "."  srp-mode-syntax-table)
;;   (modify-syntax-entry ?\+ "."  srp-mode-syntax-table)
;;   (modify-syntax-entry ?\- "."  srp-mode-syntax-table)
;;   ;(modify-syntax-entry ?\/ "."  srp-mode-syntax-table)
;;   (modify-syntax-entry ?\< "."  srp-mode-syntax-table)
;;   (modify-syntax-entry ?\= "."  srp-mode-syntax-table)
;;   (modify-syntax-entry ?\> "."  srp-mode-syntax-table)
;;   (modify-syntax-entry ?\| "."  srp-mode-syntax-table)
;;   ;; For historical reasons, underscore is word class instead of
;;   ;; symbol class.  GNU conventions say it should be symbol class, but
;;   ;; there's a natural conflict between what major mode authors want
;;   ;; and what users expect from `forward-word' and `backward-word'.
;;   ;; Guido and I have hashed this out and have decided to keep
;;   ;; underscore in word class.  If you're tempted to change it, try
;;   ;; binding M-f and M-b to srp-forward-into-nomenclature and
;;   ;; srp-backward-into-nomenclature instead.  This doesn't help in all
;;   ;; situations where you'd want the different behavior
;;   ;; (e.g. backward-kill-word).
;;   (modify-syntax-entry ?\_ "w"  srp-mode-syntax-table)
;;   ;; Both single quote and double quote are string delimiters
;;   (modify-syntax-entry ?\' "\"" srp-mode-syntax-table)
;;   (modify-syntax-entry ?\" "\"" srp-mode-syntax-table)
;;   ;; comment delimiters
;;   (cond
;;    ;; XEmacs
;;    ((fboundp 'get-char-table)
;;     (modify-syntax-entry ?\# "<"  srp-mode-syntax-table)
;;      (modify-syntax-entry ?/  ". 12" srp-mode-syntax-table)
;;     (modify-syntax-entry ?\n ">"  srp-mode-syntax-table)
;;     )
;;    ;; Emacs
;;    ((arrayp srp-mode-syntax-table)
;;      (modify-syntax-entry ?\# "<"  srp-mode-syntax-table)
;;      (modify-syntax-entry ?/  ". 12" srp-mode-syntax-table)
;;      (modify-syntax-entry ?\n ">"  srp-mode-syntax-table)
;;      )
;;    ;; incompatible?
;;    (t (error "CC Mode is incompatible with this version of Emacs")))
;;   )

;; ;; An auxiliary syntax table which places underscore and dot in the
;; ;; symbol class for simplicity
;; (defvar srp-dotted-expression-syntax-table nil
;;   "Syntax table used to identify Serpent dotted expressions.")
;; (when (not srp-dotted-expression-syntax-table)
;;   (setq srp-dotted-expression-syntax-table
;; 	(copy-syntax-table srp-mode-syntax-table))
;;   (modify-syntax-entry ?_ "_" srp-dotted-expression-syntax-table)
;;   (modify-syntax-entry ?. "_" srp-dotted-expression-syntax-table))



;; ;; Utilities
;; (defmacro srp-safe (&rest body)
;;   "Safely execute BODY, return nil if an error occurred."
;;   `(condition-case nil
;;        (progn ,@body)
;;      (error nil)))

;; (defsubst srp-keep-region-active ()
;;   "Keep the region active in XEmacs."
;;   ;; Ignore byte-compiler warnings you might see.  Also note that
;;   ;; FSF's Emacs 19 does it differently; its policy doesn't require us
;;   ;; to take explicit action.
;;   (and (boundp 'zmacs-region-stays)
;;        (setq zmacs-region-stays t)))

;; (defsubst srp-point (position)
;;   "Returns the value of point at certain commonly referenced POSITIONs.
;; POSITION can be one of the following symbols:

;;   bol  -- beginning of line
;;   eol  -- end of line
;;   bod  -- beginning of def or class
;;   eod  -- end of def or class
;;   bob  -- beginning of buffer
;;   eob  -- end of buffer
;;   boi  -- back to indentation
;;   bos  -- beginning of statement

;; This function does not modify point or mark."
;;   (let ((here (point)))
;;     (cond
;;      ((eq position 'bol) (beginning-of-line))
;;      ((eq position 'eol) (end-of-line))
;;      ((eq position 'bod) (srp-beginning-of-def-or-class 'either))
;;      ((eq position 'eod) (srp-end-of-def-or-class 'either))
;;      ;; Kind of funny, I know, but useful for srp-up-exception.
;;      ((eq position 'bob) (goto-char (point-min)))
;;      ((eq position 'eob) (goto-char (point-max)))
;;      ((eq position 'boi) (back-to-indentation))
;;      ((eq position 'bos) (srp-goto-initial-line))
;;      (t (error "Unknown buffer position requested: %s" position))
;;      )
;;     (prog1
;; 	(point)
;;       (goto-char here))))

;; (defsubst srp-highlight-line (from to file line)
;;   (cond
;;    ((fboundp 'make-extent)
;;     ;; XEmacs
;;     (let ((e (make-extent from to)))
;;       (set-extent-property e 'mouse-face 'highlight)
;;       (set-extent-property e 'srp-exc-info (cons file line))
;;       (set-extent-property e 'keymap srp-mode-output-map)))
;;    (t
;;     ;; Emacs -- Please port this!
;;     )
;;    ))

;; (defun srp-in-literal (&optional lim)
;;   "Return non-nil if point is in a Serpent literal (a comment or string).
;; Optional argument LIM indicates the beginning of the containing form,
;; i.e. the limit on how far back to scan."
;;   ;; This is the version used for non-XEmacs, which has a nicer
;;   ;; interface.
;;   ;;
;;   ;; WARNING: Watch out for infinite recursion.
;;   (let* ((lim (or lim (srp-point 'bod)))
;; 	 (state (parse-partial-sexp lim (point))))
;;     (cond
;;      ((nth 3 state) 'string)
;;      ((nth 4 state) 'comment)
;;      (t nil))))

;; ;; XEmacs has a built-in function that should make this much quicker.
;; ;; In this case, lim is ignored
;; (defun srp-fast-in-literal (&optional lim)
;;   "Fast version of `srp-in-literal', used only by XEmacs.
;; Optional LIM is ignored."
;;   ;; don't have to worry about context == 'block-comment
;;   (buffer-syntactic-context))

;; (if (fboundp 'buffer-syntactic-context)
;;     (defalias 'srp-in-literal 'srp-fast-in-literal))


;; ;; Menu definitions, only relevent if you have the easymenu.el package
;; ;; (standard in the latest Emacs 19 and XEmacs 19 distributions).
;; (defvar srp-menu nil
;;   "Menu for Serpent Mode.
;; This menu will get created automatically if you have the `easymenu'
;; package.  Note that the latest X/Emacs releases contain this package.")

;; (and (srp-safe (require 'easymenu) t)
;;      (easy-menu-define
;;       srp-menu srp-mode-map "Serpent Mode menu"
;;       '("Serpent"
;; 	["Comment Out Region"   srp-comment-region  (mark)]
;; 	["Uncomment Region"     (srp-comment-region (point) (mark) '(4)) (mark)]
;; 	"-"
;; 	["Mark current block"   srp-mark-block t]
;; 	["Mark current def"     srp-mark-def-or-class t]
;; 	["Mark current class"   (srp-mark-def-or-class t) t]
;; 	"-"
;; 	["Shift region left"    srp-shift-region-left (mark)]
;; 	["Shift region right"   srp-shift-region-right (mark)]
;; 	"-"
;; 	["Import/reload file"   srp-execute-import-or-reload t]
;; 	["Execute buffer"       srp-execute-buffer t]
;; 	["Execute region"       srp-execute-region (mark)]
;; 	["Execute def or class" srp-execute-def-or-class (mark)]
;; 	["Execute string"       srp-execute-string t]
;; 	["Start interpreter..." srp-shell t]
;; 	"-"
;; 	["Go to start of block" srp-goto-block-up t]
;; 	["Go to start of class" (srp-beginning-of-def-or-class t) t]
;; 	["Move to end of class" (srp-end-of-def-or-class t) t]
;; 	["Move to start of def" srp-beginning-of-def-or-class t]
;; 	["Move to end of def"   srp-end-of-def-or-class t]
;; 	"-"
;; 	["Describe mode"        srp-describe-mode t]
;; 	)))



;; ;; Imenu definitions
;; (defvar srp-imenu-class-regexp
;;   (concat				; <<classes>>
;;    "\\("				;
;;    "^[ \t]*"				; newline and maybe whitespace
;;    "\\(class[ \t]+[a-zA-Z0-9_]+\\)"	; class name
;; 					; possibly multiple superclasses
;;    "\\([ \t]*\\((\\([a-zA-Z0-9_,. \t\n]\\)*)\\)?\\)"
;;    "[ \t]*:"				; and the final :
;;    "\\)"				; >>classes<<
;;    )
;;   "Regexp for Serpent classes for use with the Imenu package."
;;   )

;; (defvar srp-imenu-method-regexp
;;   (concat                               ; <<methods and functions>>
;;    "\\("                                ;
;;    "^[ \t]*"                            ; new line and maybe whitespace
;;    "\\(def[ \t]+"                       ; function definitions start with def
;;    "\\([a-zA-Z0-9_]+\\)"                ;   name is here
;; 					;   function arguments...
;; ;;   "[ \t]*(\\([-+/a-zA-Z0-9_=,\* \t\n.()\"'#]*\\))"
;;    "[ \t]*(\\([^:#]*\\))"
;;    "\\)"                                ; end of def
;;    "[ \t]*:"                            ; and then the :
;;    "\\)"                                ; >>methods and functions<<
;;    )
;;   "Regexp for Serpent methods/functions for use with the Imenu package."
;;   )

;; (defvar srp-imenu-method-no-arg-parens '(2 8)
;;   "Indices into groups of the Serpent regexp for use with Imenu.

;; Using these values will result in smaller Imenu lists, as arguments to
;; functions are not listed.

;; See the variable `srp-imenu-show-method-args-p' for more
;; information.")

;; (defvar srp-imenu-method-arg-parens '(2 7)
;;   "Indices into groups of the Serpent regexp for use with imenu.
;; Using these values will result in large Imenu lists, as arguments to
;; functions are listed.

;; See the variable `srp-imenu-show-method-args-p' for more
;; information.")

;; ;; Note that in this format, this variable can still be used with the
;; ;; imenu--generic-function. Otherwise, there is no real reason to have
;; ;; it.
;; (defvar srp-imenu-generic-expression
;;   (cons
;;    (concat
;;     srp-imenu-class-regexp
;;     "\\|"				; or...
;;     srp-imenu-method-regexp
;;     )
;;    srp-imenu-method-no-arg-parens)
;;   "Generic Serpent expression which may be used directly with Imenu.
;; Used by setting the variable `imenu-generic-expression' to this value.
;; Also, see the function \\[srp-imenu-create-index] for a better
;; alternative for finding the index.")

;; ;; These next two variables are used when searching for the Serpent
;; ;; class/definitions. Just saving some time in accessing the
;; ;; generic-serpent-expression, really.
;; (defvar srp-imenu-generic-regexp nil)
;; (defvar srp-imenu-generic-parens nil)


;; (defun srp-imenu-create-index-function ()
;;   "Serpent interface function for the Imenu package.
;; Finds all Serpent classes and functions/methods. Calls function
;; \\[srp-imenu-create-index-engine].  See that function for the details
;; of how this works."
;;   (setq srp-imenu-generic-regexp (car srp-imenu-generic-expression)
;; 	srp-imenu-generic-parens (if srp-imenu-show-method-args-p
;; 				    srp-imenu-method-arg-parens
;; 				  srp-imenu-method-no-arg-parens))
;;   (goto-char (point-min))
;;   ;; Warning: When the buffer has no classes or functions, this will
;;   ;; return nil, which seems proper according to the Imenu API, but
;;   ;; causes an error in the XEmacs port of Imenu.  Sigh.
;;   (srp-imenu-create-index-engine nil))

;; (defun srp-imenu-create-index-engine (&optional start-indent)
;;   "Function for finding Imenu definitions in Serpent.

;; Finds all definitions (classes, methods, or functions) in a Serpent
;; file for the Imenu package.

;; Returns a possibly nested alist of the form

;; 	(INDEX-NAME . INDEX-POSITION)

;; The second element of the alist may be an alist, producing a nested
;; list as in

;; 	(INDEX-NAME . INDEX-ALIST)

;; This function should not be called directly, as it calls itself
;; recursively and requires some setup.  Rather this is the engine for
;; the function \\[srp-imenu-create-index-function].

;; It works recursively by looking for all definitions at the current
;; indention level.  When it finds one, it adds it to the alist.  If it
;; finds a definition at a greater indentation level, it removes the
;; previous definition from the alist. In its place it adds all
;; definitions found at the next indentation level.  When it finds a
;; definition that is less indented then the current level, it returns
;; the alist it has created thus far.

;; The optional argument START-INDENT indicates the starting indentation
;; at which to continue looking for Serpent classes, methods, or
;; functions.  If this is not supplied, the function uses the indentation
;; of the first definition found."
;;   (let (index-alist
;; 	sub-method-alist
;; 	looking-p
;; 	def-name prev-name
;; 	cur-indent def-pos
;; 	(class-paren (first  srp-imenu-generic-parens))
;; 	(def-paren   (second srp-imenu-generic-parens)))
;;     (setq looking-p
;; 	  (re-search-forward srp-imenu-generic-regexp (point-max) t))
;;     (while looking-p
;;       (save-excursion
;; 	;; used to set def-name to this value but generic-extract-name
;; 	;; is new to imenu-1.14. this way it still works with
;; 	;; imenu-1.11
;; 	;;(imenu--generic-extract-name srp-imenu-generic-parens))
;; 	(let ((cur-paren (if (match-beginning class-paren)
;; 			     class-paren def-paren)))
;; 	  (setq def-name
;; 		(buffer-substring-no-properties (match-beginning cur-paren)
;; 						(match-end cur-paren))))
;; 	(save-match-data
;; 	  (srp-beginning-of-def-or-class 'either))
;; 	(beginning-of-line)
;; 	(setq cur-indent (current-indentation)))
;;       ;; HACK: want to go to the next correct definition location.  We
;;       ;; explicitly list them here but it would be better to have them
;;       ;; in a list.
;;       (setq def-pos
;; 	    (or (match-beginning class-paren)
;; 		(match-beginning def-paren)))
;;       ;; if we don't have a starting indent level, take this one
;;       (or start-indent
;; 	  (setq start-indent cur-indent))
;;       ;; if we don't have class name yet, take this one
;;       (or prev-name
;; 	  (setq prev-name def-name))
;;       ;; what level is the next definition on?  must be same, deeper
;;       ;; or shallower indentation
;;       (cond
;;        ;; Skip code in comments and strings
;;        ((srp-in-literal))
;;        ;; at the same indent level, add it to the list...
;;        ((= start-indent cur-indent)
;; 	(push (cons def-name def-pos) index-alist))
;;        ;; deeper indented expression, recurse
;;        ((< start-indent cur-indent)
;; 	;; the point is currently on the expression we're supposed to
;; 	;; start on, so go back to the last expression. The recursive
;; 	;; call will find this place again and add it to the correct
;; 	;; list
;; 	(re-search-backward srp-imenu-generic-regexp (point-min) 'move)
;; 	(setq sub-method-alist (srp-imenu-create-index-engine cur-indent))
;; 	(if sub-method-alist
;; 	    ;; we put the last element on the index-alist on the start
;; 	    ;; of the submethod alist so the user can still get to it.
;; 	    (let ((save-elmt (pop index-alist)))
;; 	      (push (cons prev-name
;; 			  (cons save-elmt sub-method-alist))
;; 		    index-alist))))
;;        ;; found less indented expression, we're done.
;;        (t
;; 	(setq looking-p nil)
;; 	(re-search-backward srp-imenu-generic-regexp (point-min) t)))
;;       ;; end-cond
;;       (setq prev-name def-name)
;;       (and looking-p
;; 	   (setq looking-p
;; 		 (re-search-forward srp-imenu-generic-regexp
;; 				    (point-max) 'move))))
;;     (nreverse index-alist)))



;; (defun srp-choose-shell-by-shebang ()
;;   "Choose CSerpent or Jython mode by looking at #! on the first line.
;; Returns the appropriate mode function.
;; Used by `srp-choose-shell', and similar to but distinct from
;; `set-auto-mode', though it uses `auto-mode-interpreter-regexp' (if available)."
;;   ;; look for an interpreter specified in the first line
;;   ;; similar to set-auto-mode (files.el)
;;   (let* ((re (if (boundp 'auto-mode-interpreter-regexp)
;; 		 auto-mode-interpreter-regexp
;; 	       ;; stolen from Emacs 21.2
;; 	       "#![ \t]?\\([^ \t\n]*/bin/env[ \t]\\)?\\([^ \t\n]+\\)"))
;; 	 (interpreter (save-excursion
;; 			(goto-char (point-min))
;; 			(if (looking-at re)
;; 			    (match-string 2)
;; 			  "")))
;; 	 elt)
;;     ;; Map interpreter name to a mode.
;;     (setq elt (assoc (file-name-nondirectory interpreter)
;; 		     srp-shell-alist))
;;     (and elt (caddr elt))))


;; 
;; (defun srp-choose-shell-by-import ()
;;   "Choose CSerpent or Jython mode based imports.
;; If a file imports any packages in `srp-jython-packages', within
;; `srp-import-check-point-max' characters from the start of the file,
;; return `jython', otherwise return nil."
;;   (let (mode)
;;     (save-excursion
;;       (goto-char (point-min))
;;       (while (and (not mode)
;; 		  (search-forward-regexp
;; 		   "^\\(\\(from\\)\\|\\(import\\)\\) \\([^ \t\n.]+\\)"
;; 		   srp-import-check-point-max t))
;; 	(setq mode (and (member (match-string 4) srp-jython-packages)
;; 			'jython
;; 			))))
;;     mode))

;; 
;; (defun srp-choose-shell ()
;;   "Choose CSerpent or Jython mode. Returns the appropriate mode function.
;; This does the following:
;;  - look for an interpreter with `srp-choose-shell-by-shebang'
;;  - examine imports using `srp-choose-shell-by-import'
;;  - default to the variable `srp-default-interpreter'"
;;   (interactive)
;;   (or (srp-choose-shell-by-shebang)
;;       (srp-choose-shell-by-import)
;;       srp-default-interpreter
;; ;      'cserpent ;; don't use to srp-default-interpreter, because default
;; ;               ;; is only way to choose CSerpent
;;       ))

;; 
;; ;;;###autoload
;; (defun serpent-mode ()
;;   "Major mode for editing Serpent files.
;; To submit a problem report, enter `\\[srp-submit-bug-report]' from a
;; `serpent-mode' buffer.  Do `\\[srp-describe-mode]' for detailed
;; documentation.  To see what version of `serpent-mode' you are running,
;; enter `\\[srp-version]'.

;; This mode knows about Serpent indentation, tokens, comments and
;; continuation lines.  Paragraphs are separated by blank lines only.

;; COMMANDS
;; \\{srp-mode-map}
;; VARIABLES

;; srp-indent-offset\t\tindentation increment
;; srp-block-comment-prefix\t\tcomment string used by `comment-region'
;; srp-serpent-command\t\tshell command to invoke Serpent interpreter
;; srp-temp-directory\t\tdirectory used for temp files (if needed)
;; srp-beep-if-tab-change\t\tring the bell if `tab-width' is changed"
;;   (interactive)
;;   ;; set up local variables
;;   (kill-all-local-variables)
;;   (make-local-variable 'font-lock-defaults)
;;   (make-local-variable 'paragraph-separate)
;;   (make-local-variable 'paragraph-start)
;;   (make-local-variable 'require-final-newline)
;;   (make-local-variable 'comment-start)
;;   (make-local-variable 'comment-end)
;;   (make-local-variable 'comment-start-skip)
;;   (make-local-variable 'comment-column)
;;   (make-local-variable 'comment-indent-function)
;;   (make-local-variable 'indent-region-function)
;;   (make-local-variable 'indent-line-function)
;;   (make-local-variable 'add-log-current-defun-function)
;;   (make-local-variable 'fill-paragraph-function)
;;   ;;
;;   (set-syntax-table srp-mode-syntax-table)
;;   (setq major-mode              'serpent-mode
;; 	mode-name               "Serpent"
;; 	local-abbrev-table      serpent-mode-abbrev-table
;; 	font-lock-defaults      '(serpent-font-lock-keywords)
;; 	paragraph-separate      "^[ \t]*$"
;; 	paragraph-start         "^[ \t]*$"
;; 	require-final-newline   t
;; 	comment-start           "// "
;; 	comment-end             ""
;; 	comment-start-skip      "// *" ;"\\(# *\\)\\|\\(// *\\)"
;; 	comment-column          40
;; 	comment-indent-function 'srp-comment-indent-function
;; 	indent-region-function  'srp-indent-region
;; 	indent-line-function    'srp-indent-line
;; 	;; tell add-log.el how to find the current function/method/variable
;; 	add-log-current-defun-function 'srp-current-defun

;; 	fill-paragraph-function 'srp-fill-paragraph
;; 	)
;;   (use-local-map srp-mode-map)
;;   ;; add the menu
;;   (if srp-menu
;;       (easy-menu-add srp-menu))
;;   ;; Emacs 19 requires this
;;   (if (boundp 'comment-multi-line)
;;       (setq comment-multi-line nil))
;;   ;; Install Imenu if available
;;   (when (srp-safe (require 'imenu))
;;     (setq imenu-create-index-function #'srp-imenu-create-index-function)
;;     (setq imenu-generic-expression srp-imenu-generic-expression)
;;     (if (fboundp 'imenu-add-to-menubar)
;; 	(imenu-add-to-menubar (format "%s-%s" "IM" mode-name)))
;;     )
;;   ;; Run the mode hook.  Note that srp-mode-hook is deprecated.
;;   (if serpent-mode-hook
;;       (run-hooks 'serpent-mode-hook)
;;     (run-hooks 'srp-mode-hook))
;;   ;; Now do the automagical guessing
;;   (if srp-smart-indentation
;;     (let ((offset srp-indent-offset))
;;       ;; It's okay if this fails to guess a good value
;;       (if (and (srp-safe (srp-guess-indent-offset))
;; 	       (<= srp-indent-offset 8)
;; 	       (>= srp-indent-offset 2))
;; 	  (setq offset srp-indent-offset))
;;       (setq srp-indent-offset offset)
;;       ;; Only turn indent-tabs-mode off if tab-width !=
;;       ;; srp-indent-offset.  Never turn it on, because the user must
;;       ;; have explicitly turned it off.
;;       (if (/= tab-width srp-indent-offset)
;; 	  (setq indent-tabs-mode nil))
;;       ))
;;   ;; Set the default shell if not already set
;;   (when (null srp-which-shell)
;;     (srp-toggle-shells (srp-choose-shell))))


;; ;; (make-obsolete 'jserpent-mode 'jython-mode)
;; (defun jython-mode ()
;;   "Major mode for editing Jython/Jython files.
;; This is a simple wrapper around `serpent-mode'.
;; It runs `jython-mode-hook' then calls `serpent-mode.'
;; It is added to `interpreter-mode-alist' and `srp-choose-shell'.
;; "
;;   (interactive)
;;   (serpent-mode)
;;   (srp-toggle-shells 'jython)
;;   (when jython-mode-hook
;;       (run-hooks 'jython-mode-hook)))


;; ;; It's handy to add recognition of Serpent files to the
;; ;; interpreter-mode-alist and to auto-mode-alist.  With the former, we
;; ;; can specify different `derived-modes' based on the #! line, but
;; ;; with the latter, we can't.  So we just won't add them if they're
;; ;; already added.
;; ;;;###autoload
;; (let ((modes '(("jython" . jython-mode)
;; 	       ("serpent" . serpent-mode))))
;;   (while modes
;;     (when (not (assoc (car modes) interpreter-mode-alist))
;;       (push (car modes) interpreter-mode-alist))
;;     (setq modes (cdr modes))))
;; ;;;###autoload
;; (when (not (or (rassq 'serpent-mode auto-mode-alist)
;; 	       (rassq 'jython-mode auto-mode-alist)))
;;   (push '("\\.py$" . serpent-mode) auto-mode-alist))



;; ;; electric characters
;; (defun srp-outdent-p ()
;;   "Returns non-nil if the current line should dedent one level."
;;   (save-excursion
;;     (and (progn (back-to-indentation)
;; 		(looking-at srp-outdent-re))
;; 	 ;; short circuit infloop on illegal construct
;; 	 (not (bobp))
;; 	 (progn (forward-line -1)
;; 		(srp-goto-initial-line)
;; 		(back-to-indentation)
;; 		(while (or (looking-at srp-blank-or-comment-re)
;; 			   (bobp))
;; 		  (backward-to-indentation 1))
;; 		(not (looking-at srp-no-outdent-re)))
;; 	 )))

;; (defun srp-electric-colon (arg)
;;   "Insert a colon.
;; In certain cases the line is dedented appropriately.  If a numeric
;; argument ARG is provided, that many colons are inserted
;; non-electrically.  Electric behavior is inhibited inside a string or
;; comment."
;;   (interactive "*P")
;;   (self-insert-command (prefix-numeric-value arg))
;;   ;; are we in a string or comment?
;;   (if (save-excursion
;; 	(let ((pps (parse-partial-sexp (save-excursion
;; 					 (srp-beginning-of-def-or-class)
;; 					 (point))
;; 				       (point))))
;; 	  (not (or (nth 3 pps) (nth 4 pps)))))
;;       (save-excursion
;; 	(let ((here (point))
;; 	      (outdent 0)
;; 	      (indent (srp-compute-indentation t)))
;; 	  (if (and (not arg)
;; 		   (srp-outdent-p)
;; 		   (= indent (save-excursion
;; 			       (srp-next-statement -1)
;; 			       (srp-compute-indentation t)))
;; 		   )
;; 	      (setq outdent srp-indent-offset))
;; 	  ;; Don't indent, only dedent.  This assumes that any lines
;; 	  ;; that are already dedented relative to
;; 	  ;; srp-compute-indentation were put there on purpose.  It's
;; 	  ;; highly annoying to have `:' indent for you.  Use TAB, C-c
;; 	  ;; C-l or C-c C-r to adjust.  TBD: Is there a better way to
;; 	  ;; determine this???
;; 	  (if (< (current-indentation) indent) nil
;; 	    (goto-char here)
;; 	    (beginning-of-line)
;; 	    (delete-horizontal-space)
;; 	    (indent-to (- indent outdent))
;; 	    )))))

;; ;; Serpent subprocess utilities and filters
;; (defun srp-execute-file (proc filename)
;;   "Send to Serpent interpreter process PROC \"exec(open('FILENAME').read())\".
;; Make that process's buffer visible and force display.  Also make
;; comint believe the user typed this string so that
;; `kill-output-from-shell' does The Right Thing."
;;   (let ((curbuf (current-buffer))
;; 	(procbuf (process-buffer proc))
;; ;	(comint-scroll-to-bottom-on-output t)
;; 	(msg (format "## working on region in file %s...\n" filename))
;; 	(cmd (format "exec(open(r'%s').read())\n" filename)))
;;     (unwind-protect
;; 	(save-excursion
;; 	  (set-buffer procbuf)
;; 	  (goto-char (point-max))
;; 	  (move-marker (process-mark proc) (point))
;; 	  (funcall (process-filter proc) proc msg))
;;       (set-buffer curbuf))
;;     (process-send-string proc cmd)))

;; (defun srp-comint-output-filter-function (string)
;;   "Watch output for Serpent prompt and exec next file waiting in queue.
;; This function is appropriate for `comint-output-filter-functions'."
;;   ;;remove ansi terminal escape sequences from string, not sure why they are
;;   ;;still around...
;;   (setq string (ansi-color-filter-apply string))
;;   (when (and (string-match srp-shell-input-prompt-1-regexp string)
;;                    srp-file-queue)
;;     (if srp-shell-switch-buffers-on-execute
;;       (pop-to-buffer (current-buffer)))
;;     (srp-safe (delete-file (car srp-file-queue)))
;;     (setq srp-file-queue (cdr srp-file-queue))
;;     (if srp-file-queue
;; 	(let ((pyproc (get-buffer-process (current-buffer))))
;; 	  (srp-execute-file pyproc (car srp-file-queue))))
;;     ))

;; (defun srp-pdbtrack-overlay-arrow (activation)
;;   "Activate or de arrow at beginning-of-line in current buffer."
;;   ;; This was derived/simplified from edebug-overlay-arrow
;;   (cond (activation
;; 	 (setq overlay-arrow-position (make-marker))
;; 	 (setq overlay-arrow-string "=>")
;; 	 (set-marker overlay-arrow-position (srp-point 'bol) (current-buffer))
;; 	 (setq srp-pdbtrack-is-tracking-p t))
;; 	(overlay-arrow-position
;; 	 (setq overlay-arrow-position nil)
;; 	 (setq srp-pdbtrack-is-tracking-p nil))
;; 	))

;; (defun srp-pdbtrack-track-stack-file (text)
;;   "Show the file indicated by the pdb stack entry line, in a separate window.

;; Activity is disabled if the buffer-local variable
;; `srp-pdbtrack-do-tracking-p' is nil.

;; We depend on the pdb input prompt matching `srp-pdbtrack-input-prompt'
;; at the beginning of the line.

;; If the traceback target file path is invalid, we look for the most
;; recently visited serpent-mode buffer which either has the name of the
;; current function \(or class) or which defines the function \(or
;; class).  This is to provide for remote scripts, eg, Zope's 'Script
;; (Serpent)' - put a _copy_ of the script in a buffer named for the
;; script, and set to serpent-mode, and pdbtrack will find it.)"
;;   ;; Instead of trying to piece things together from partial text
;;   ;; (which can be almost useless depending on Emacs version), we
;;   ;; monitor to the point where we have the next pdb prompt, and then
;;   ;; check all text from comint-last-input-end to process-mark.
;;   ;;
;;   ;; Also, we're very conservative about clearing the overlay arrow,
;;   ;; to minimize residue.  This means, for instance, that executing
;;   ;; other pdb commands wipe out the highlight.  You can always do a
;;   ;; 'where' (aka 'w') command to reveal the overlay arrow.
;;   (let* ((origbuf (current-buffer))
;; 	 (currproc (get-buffer-process origbuf)))

;;     (if (not (and currproc srp-pdbtrack-do-tracking-p))
;;         (srp-pdbtrack-overlay-arrow nil)

;;       (let* ((procmark (process-mark currproc))
;;              (block (buffer-substring (max comint-last-input-end
;;                                            (- procmark
;;                                               srp-pdbtrack-track-range))
;;                                       procmark))
;;              target target_fname target_lineno target_buffer)

;;         (if (not (string-match (concat srp-pdbtrack-input-prompt "$") block))
;;             (srp-pdbtrack-overlay-arrow nil)

;;           (setq target (srp-pdbtrack-get-source-buffer block))

;;           (if (stringp target)
;;               (message "pdbtrack: %s" target)

;;             (setq target_lineno (car target))
;;             (setq target_buffer (cadr target))
;;             (setq target_fname (buffer-file-name target_buffer))
;;             (switch-to-buffer-other-window target_buffer)
;;             (forward-line target_lineno)
;;             (message "pdbtrack: line %s, file %s" target_lineno target_fname)
;;             (srp-pdbtrack-overlay-arrow t)
;;             (pop-to-buffer origbuf t)

;;             )))))
;;   )

;; (defun srp-pdbtrack-get-source-buffer (block)
;;   "Return line number and buffer of code indicated by block's traceback text.

;; We look first to visit the file indicated in the trace.

;; Failing that, we look for the most recently visited serpent-mode buffer
;; with the same name or having the named function.

;; If we're unable find the source code we return a string describing the
;; problem as best as we can determine."

;;   (if (not (string-match srp-pdbtrack-stack-entry-regexp block))

;;       "Traceback cue not found"

;;     (let* ((filename (match-string 1 block))
;;            (lineno (string-to-int (match-string 2 block)))
;;            (funcname (match-string 3 block))
;;            funcbuffer)

;;       (cond ((file-exists-p filename)
;;              (list lineno (find-file-noselect filename)))

;;             ((setq funcbuffer (srp-pdbtrack-grub-for-buffer funcname lineno))
;;              (if (string-match "/Script (Serpent)$" filename)
;;                  ;; Add in number of lines for leading '##' comments:
;;                  (setq lineno
;;                        (+ lineno
;;                           (save-excursion
;;                             (set-buffer funcbuffer)
;;                             (count-lines
;;                              (point-min)
;;                              (max (point-min)
;;                                   (string-match "^\\([^#]\\|#[^#]\\|#$\\)"
;;                                                 (buffer-substring (point-min)
;;                                                                   (point-max)))
;;                                   ))))))
;;              (list lineno funcbuffer))

;;             ((= (elt filename 0) ?\<)
;;              (format "(Non-file source: '%s')" filename))

;;             (t (format "Not found: %s(), %s" funcname filename)))
;;       )
;;     )
;;   )

;; (defun srp-pdbtrack-grub-for-buffer (funcname lineno)
;;   "Find most recent buffer itself named or having function funcname.

;; We walk the buffer-list history for serpent-mode buffers that are
;; named for funcname or define a function funcname."
;;   (let ((buffers (buffer-list))
;;         buf
;;         got)
;;     (while (and buffers (not got))
;;       (setq buf (car buffers)
;;             buffers (cdr buffers))
;;       (if (and (save-excursion (set-buffer buf)
;;                                (string= major-mode "serpent-mode"))
;;                (or (string-match funcname (buffer-name buf))
;;                    (string-match (concat "^\\s-*\\(def\\|class\\)\\s-+"
;;                                          funcname "\\s-*(")
;;                                  (save-excursion
;;                                    (set-buffer buf)
;;                                    (buffer-substring (point-min)
;;                                                      (point-max))))))
;;           (setq got buf)))
;;     got))

;; (defun srp-postprocess-output-buffer (buf)
;;   "Highlight exceptions found in BUF.
;; If an exception occurred return t, otherwise return nil.  BUF must exist."
;;   (let (line file bol err-p)
;;     (save-excursion
;;       (set-buffer buf)
;;       (beginning-of-buffer)
;;       (while (re-search-forward srp-traceback-line-re nil t)
;; 	(setq file (match-string 1)
;; 	      line (string-to-int (match-string 2))
;; 	      bol (srp-point 'bol))
;; 	(srp-highlight-line bol (srp-point 'eol) file line)))
;;     (when (and srp-jump-on-exception line)
;;       (beep)
;;       (srp-jump-to-exception file line)
;;       (setq err-p t))
;;     err-p))


;; 
;; ;;; Subprocess commands

;; ;; only used when (memq 'broken-temp-names srp-emacs-features)
;; (defvar srp-serial-number 0)
;; (defvar srp-exception-buffer nil)
;; (defconst srp-output-buffer "*Serpent Output*")
;; (make-variable-buffer-local 'srp-output-buffer)

;; ;; for toggling between CSerpent and Jython
;; (defvar srp-which-shell nil)
;; (defvar srp-which-args  srp-serpent-command-args)
;; (defvar srp-which-bufname "Serpent")
;; (make-variable-buffer-local 'srp-which-shell)
;; (make-variable-buffer-local 'srp-which-args)
;; (make-variable-buffer-local 'srp-which-bufname)

;; (defun srp-toggle-shells (arg)
;;   "Toggles between the CSerpent and Jython shells.

;; With positive argument ARG (interactively \\[universal-argument]),
;; uses the CSerpent shell, with negative ARG uses the Jython shell, and
;; with a zero argument, toggles the shell.

;; Programmatically, ARG can also be one of the symbols `cserpent' or
;; `jython', equivalent to positive arg and negative arg respectively."
;;   (interactive "P")
;;   ;; default is to toggle
;;   (if (null arg)
;;       (setq arg 0))
;;   ;; preprocess arg
;;   (cond
;;    ((equal arg 0)
;;     ;; toggle
;;     (if (string-equal srp-which-bufname "Serpent")
;; 	(setq arg -1)
;;       (setq arg 1)))
;;    ((equal arg 'cserpent) (setq arg 1))
;;    ((equal arg 'jython) (setq arg -1)))
;;   (let (msg)
;;     (cond
;;      ((< 0 arg)
;;       ;; set to CSerpent
;;       (setq srp-which-shell srp-serpent-command
;; 	    srp-which-args srp-serpent-command-args
;; 	    srp-which-bufname "Serpent"
;; 	    msg "CSerpent")
;;       (if (string-equal srp-which-bufname "Jython")
;; 	  (setq mode-name "Serpent")))
;;      ((> 0 arg)
;;       (setq srp-which-shell srp-jython-command
;; 	    srp-which-args srp-jython-command-args
;; 	    srp-which-bufname "Jython"
;; 	    msg "Jython")
;;       (if (string-equal srp-which-bufname "Serpent")
;; 	  (setq mode-name "Jython")))
;;      )
;;     (message "Using the %s shell" msg)
;;     (setq srp-output-buffer (format "*%s Output*" srp-which-bufname))))

;; ;;;###autoload
;; (defun srp-shell (&optional argprompt)
;;   "Start an interactive Serpent interpreter in another window.
;; This is like Shell mode, except that Serpent is running in the window
;; instead of a shell.  See the `Interactive Shell' and `Shell Mode'
;; sections of the Emacs manual for details, especially for the key
;; bindings active in the `*Serpent*' buffer.

;; With optional \\[universal-argument], the user is prompted for the
;; flags to pass to the Serpent interpreter.  This has no effect when this
;; command is used to switch to an existing process, only when a new
;; process is started.  If you use this, you will probably want to ensure
;; that the current arguments are retained (they will be included in the
;; prompt).  This argument is ignored when this function is called
;; programmatically, or when running in Emacs 19.34 or older.

;; Note: You can toggle between using the CSerpent interpreter and the
;; Jython interpreter by hitting \\[srp-toggle-shells].  This toggles
;; buffer local variables which control whether all your subshell
;; interactions happen to the `*Jython*' or `*Serpent*' buffers (the
;; latter is the name used for the CSerpent buffer).

;; Warning: Don't use an interactive Serpent if you change sys.ps1 or
;; sys.ps2 from their default values, or if you're running code that
;; prints `>>> ' or `... ' at the start of a line.  `serpent-mode' can't
;; distinguish your output from Serpent's output, and assumes that `>>> '
;; at the start of a line is a prompt from Serpent.  Similarly, the Emacs
;; Shell mode code assumes that both `>>> ' and `... ' at the start of a
;; line are Serpent prompts.  Bad things can happen if you fool either
;; mode.

;; Warning:  If you do any editing *in* the process buffer *while* the
;; buffer is accepting output from Serpent, do NOT attempt to `undo' the
;; changes.  Some of the output (nowhere near the parts you changed!) may
;; be lost if you do.  This appears to be an Emacs bug, an unfortunate
;; interaction between undo and process filters; the same problem exists in
;; non-Serpent process buffers using the default (Emacs-supplied) process
;; filter."
;;   (interactive "P")
;;   ;; Set the default shell if not already set
;;   (when (null srp-which-shell)
;;     (srp-toggle-shells srp-default-interpreter))
;;   (let ((args srp-which-args))
;;     (when (and argprompt
;; 	       (interactive-p)
;; 	       (fboundp 'split-string))
;;       ;; TBD: Perhaps force "-i" in the final list?
;;       (setq args (split-string
;; 		  (read-string (concat srp-which-bufname
;; 				       " arguments: ")
;; 			       (concat
;; 				(mapconcat 'identity srp-which-args " ") " ")
;; 			       ))))
;;     (if (not (equal (buffer-name) "*Serpent*"))
;;         (switch-to-buffer-other-window
;;          (apply 'make-comint srp-which-bufname srp-which-shell nil args))
;;       (apply 'make-comint srp-which-bufname srp-which-shell nil args))
;;     (make-local-variable 'comint-prompt-regexp)
;;     (setq comint-prompt-regexp (concat srp-shell-input-prompt-1-regexp "\\|"
;;                                        srp-shell-input-prompt-2-regexp "\\|"
;;                                        "^([Pp]db) "))
;;     (add-hook 'comint-output-filter-functions
;; 	      'srp-comint-output-filter-function)
;;     ;; pdbtrack
;;     (add-hook 'comint-output-filter-functions 'srp-pdbtrack-track-stack-file)
;;     (setq srp-pdbtrack-do-tracking-p t)
;;     (set-syntax-table srp-mode-syntax-table)
;;     (use-local-map srp-shell-map)
;;     (run-hooks 'srp-shell-hook)
;;     ))

;; (defun srp-clear-queue ()
;;   "Clear the queue of temporary files waiting to execute."
;;   (interactive)
;;   (let ((n (length srp-file-queue)))
;;     (mapcar 'delete-file srp-file-queue)
;;     (setq srp-file-queue nil)
;;     (message "%d pending files de-queued." n)))

;; 
;; (defun srp-execute-region (start end &optional async)
;;   "Execute the region in a Serpent interpreter.

;; The region is first copied into a temporary file (in the directory
;; `srp-temp-directory').  If there is no Serpent interpreter shell
;; running, this file is executed synchronously using
;; `shell-command-on-region'.  If the program is long running, use
;; \\[universal-argument] to run the command asynchronously in its own
;; buffer.

;; When this function is used programmatically, arguments START and END
;; specify the region to execute, and optional third argument ASYNC, if
;; non-nil, specifies to run the command asynchronously in its own
;; buffer.

;; If the Serpent interpreter shell is running, the region is exec()'d
;; in that shell.  If you try to execute regions too quickly,
;; `serpent-mode' will queue them up and execute them one at a time when
;; it sees a `>>> ' prompt from Serpent.  Each time this happens, the
;; process buffer is popped into a window (if it's not already in some
;; window) so you can see it, and a comment of the form

;;     \t## working on region in file <name>...

;; is inserted at the end.  See also the command `srp-clear-queue'."
;;   (interactive "r\nP")
;;   ;; Skip ahead to the first non-blank line
;;   (let* ((proc (get-process srp-which-bufname))
;; 	 (temp (if (memq 'broken-temp-names srp-emacs-features)
;; 		   (let
;; 		       ((sn srp-serial-number)
;; 			(pid (and (fboundp 'emacs-pid) (emacs-pid))))
;; 		     (setq srp-serial-number (1+ srp-serial-number))
;; 		     (if pid
;; 			 (format "serpent-%d-%d" sn pid)
;; 		       (format "serpent-%d" sn)))
;; 		 (make-temp-name "serpent-")))
;; 	 (file (concat (expand-file-name temp srp-temp-directory) ".py"))
;; 	 (cur (current-buffer))
;; 	 (buf (get-buffer-create file))
;; 	 shell)
;;     ;; Write the contents of the buffer, watching out for indented regions.
;;     (save-excursion
;;       (goto-char start)
;;       (beginning-of-line)
;;       (while (and (looking-at "\\s *$")
;; 		  (< (point) end))
;; 	(forward-line 1))
;;       (setq start (point))
;;       (or (< start end)
;; 	  (error "Region is empty"))
;;       (setq srp-line-number-offset (count-lines 1 start))
;;       (let ((needs-if (/= (srp-point 'bol) (srp-point 'boi))))
;; 	(set-buffer buf)
;; 	(serpent-mode)
;; 	(when needs-if
;; 	  (insert "if 1:\n")
;; 	  (setq srp-line-number-offset (- srp-line-number-offset 1)))
;; 	(insert-buffer-substring cur start end)
;; 	;; Set the shell either to the #! line command, or to the
;; 	;; srp-which-shell buffer local variable.
;; 	(setq shell (or (srp-choose-shell-by-shebang)
;; 			(srp-choose-shell-by-import)
;; 			srp-which-shell))))
;;     (cond
;;      ;; always run the code in its own asynchronous subprocess
;;      (async
;;       ;; User explicitly wants this to run in its own async subprocess
;;       (save-excursion
;; 	(set-buffer buf)
;; 	(write-region (point-min) (point-max) file nil 'nomsg))
;;       (let* ((buf (generate-new-buffer-name srp-output-buffer))
;; 	     ;; TBD: a horrible hack, but why create new Custom variables?
;; 	     (arg (if (string-equal srp-which-bufname "Serpent")
;; 		      "-u" "")))
;; 	(start-process srp-which-bufname buf shell arg file)
;; 	(pop-to-buffer buf)
;; 	(srp-postprocess-output-buffer buf)
;; 	;; TBD: clean up the temporary file!
;; 	))
;;      ;; if the Serpent interpreter shell is running, queue it up for
;;      ;; execution there.
;;      (proc
;;       ;; use the existing serpent shell
;;       (save-excursion
;; 	(set-buffer buf)
;; 	(write-region (point-min) (point-max) file nil 'nomsg))
;;       (if (not srp-file-queue)
;; 	  (srp-execute-file proc file)
;; 	(message "File %s queued for execution" file))
;;       (setq srp-file-queue (append srp-file-queue (list file)))
;;       (setq srp-exception-buffer (cons file (current-buffer))))
;;      (t
;;       ;; TBD: a horrible hack, but why create new Custom variables?
;;       (let ((cmd (concat srp-which-shell (if (string-equal srp-which-bufname
;; 							  "Jython")
;; 					    " -" ""))))
;; 	;; otherwise either run it synchronously in a subprocess
;; 	(save-excursion
;; 	  (set-buffer buf)
;; 	  (shell-command-on-region (point-min) (point-max)
;; 				   cmd srp-output-buffer))
;; 	;; shell-command-on-region kills the output buffer if it never
;; 	;; existed and there's no output from the command
;; 	(if (not (get-buffer srp-output-buffer))
;; 	    (message "No output.")
;; 	  (setq srp-exception-buffer (current-buffer))
;; 	  (let ((err-p (srp-postprocess-output-buffer srp-output-buffer)))
;; 	    (pop-to-buffer srp-output-buffer)
;; 	    (if err-p
;; 		(pop-to-buffer srp-exception-buffer)))
;; 	  ))
;;       ))
;;     ;; Clean up after ourselves.
;;     (kill-buffer buf)))

;; 
;; ;; Code execution commands
;; (defun srp-execute-buffer (&optional async)
;;   "Send the contents of the buffer to a Serpent interpreter.
;; If the file local variable `srp-master-file' is non-nil, execute the
;; named file instead of the buffer's file.

;; If there is a *Serpent* process buffer it is used.  If a clipping
;; restriction is in effect, only the accessible portion of the buffer is
;; sent.  A trailing newline will be supplied if needed.

;; See the `\\[srp-execute-region]' docs for an account of some
;; subtleties, including the use of the optional ASYNC argument."
;;   (interactive "P")
;;   (let ((old-buffer (current-buffer)))
;;     (if srp-master-file
;;         (let* ((filename (expand-file-name srp-master-file))
;;                (buffer (or (get-file-buffer filename)
;;                            (find-file-noselect filename))))
;;           (set-buffer buffer)))
;;     (srp-execute-region (point-min) (point-max) async)
;;        (pop-to-buffer old-buffer)))

;; (defun srp-execute-import-or-reload (&optional async)
;;   "Import the current buffer's file in a Serpent interpreter.

;; If the file has already been imported, then do reload instead to get
;; the latest version.

;; If the file's name does not end in \".py\", then do exec instead.

;; If the current buffer is not visiting a file, do `srp-execute-buffer'
;; instead.

;; If the file local variable `srp-master-file' is non-nil, import or
;; reload the named file instead of the buffer's file.  The file may be
;; saved based on the value of `srp-execute-import-or-reload-save-p'.

;; See the `\\[srp-execute-region]' docs for an account of some
;; subtleties, including the use of the optional ASYNC argument.

;; This may be preferable to `\\[srp-execute-buffer]' because:

;;  - Definitions stay in their module rather than appearing at top
;;    level, where they would clutter the global namespace and not affect
;;    uses of qualified names (MODULE.NAME).

;;  - The Serpent debugger gets line number information about the functions."
;;   (interactive "P")
;;   ;; Check file local variable srp-master-file
;;   (if srp-master-file
;;       (let* ((filename (expand-file-name srp-master-file))
;;              (buffer (or (get-file-buffer filename)
;;                          (find-file-noselect filename))))
;;         (set-buffer buffer)))
;;   (let ((file (buffer-file-name (current-buffer))))
;;     (if file
;;         (progn
;; 	  ;; Maybe save some buffers
;; 	  (save-some-buffers (not srp-ask-about-save) nil)
;;           (srp-execute-string
;;            (if (string-match "\\.py$" file)
;;                (let ((f (file-name-sans-extension
;; 			 (file-name-nondirectory file))))
;;                  (format "if globals().has_key('%s'):\n    reload(%s)\nelse:\n    import %s\n"
;;                          f f f))
;;              (format "exec(open(r'%s'))\n" file))
;;            async))
;;       ;; else
;;       (srp-execute-buffer async))))


;; (defun srp-execute-def-or-class (&optional async)
;;   "Send the current function or class definition to a Serpent interpreter.

;; If there is a *Serpent* process buffer it is used.

;; See the `\\[srp-execute-region]' docs for an account of some
;; subtleties, including the use of the optional ASYNC argument."
;;   (interactive "P")
;;   (save-excursion
;;     (srp-mark-def-or-class)
;;     ;; mark is before point
;;     (srp-execute-region (mark) (point) async)))


;; (defun srp-execute-string (string &optional async)
;;   "Send the argument STRING to a Serpent interpreter.

;; If there is a *Serpent* process buffer it is used.

;; See the `\\[srp-execute-region]' docs for an account of some
;; subtleties, including the use of the optional ASYNC argument."
;;   (interactive "sExecute Serpent command: ")
;;   (save-excursion
;;     (set-buffer (get-buffer-create
;;                  (generate-new-buffer-name " *Serpent Command*")))
;;     (insert string)
;;     (srp-execute-region (point-min) (point-max) async)))


;; (defun srp-jump-to-exception (file line)
;;   "Jump to the Serpent code in FILE at LINE."
;;   (let ((buffer (cond ((string-equal file "<stdin>")
;; 		       (if (consp srp-exception-buffer)
;; 			   (cdr srp-exception-buffer)
;; 			 srp-exception-buffer))
;; 		      ((and (consp srp-exception-buffer)
;; 			    (string-equal file (car srp-exception-buffer)))
;; 		       (cdr srp-exception-buffer))
;; 		      ((srp-safe (find-file-noselect file)))
;; 		      ;; could not figure out what file the exception
;; 		      ;; is pointing to, so prompt for it
;; 		      (t (find-file (read-file-name "Exception file: "
;; 						    nil
;; 						    file t))))))
;;     ;; Fiddle about with line number
;;     (setq line (+ srp-line-number-offset line))

;;     (pop-to-buffer buffer)
;;     ;; Force Serpent mode
;;     (if (not (eq major-mode 'serpent-mode))
;; 	(serpent-mode))
;;     (goto-line line)
;;     (message "Jumping to exception in file %s on line %d" file line)))

;; (defun srp-mouseto-exception (event)
;;   "Jump to the code which caused the Serpent exception at EVENT.
;; EVENT is usually a mouse click."
;;   (interactive "e")
;;   (cond
;;    ((fboundp 'event-point)
;;     ;; XEmacs
;;     (let* ((point (event-point event))
;; 	   (buffer (event-buffer event))
;; 	   (e (and point buffer (extent-at point buffer 'srp-exc-info)))
;; 	   (info (and e (extent-property e 'srp-exc-info))))
;;       (message "Event point: %d, info: %s" point info)
;;       (and info
;; 	   (srp-jump-to-exception (car info) (cdr info)))
;;       ))
;;    ;; Emacs -- Please port this!
;;    ))

;; (defun srp-goto-exception ()
;;   "Go to the line indicated by the traceback."
;;   (interactive)
;;   (let (file line)
;;     (save-excursion
;;       (beginning-of-line)
;;       (if (looking-at srp-traceback-line-re)
;; 	  (setq file (match-string 1)
;; 		line (string-to-int (match-string 2)))))
;;     (if (not file)
;; 	(error "Not on a traceback line"))
;;     (srp-jump-to-exception file line)))

;; (defun srp-find-next-exception (start buffer searchdir errwhere)
;;   "Find the next Serpent exception and jump to the code that caused it.
;; START is the buffer position in BUFFER from which to begin searching
;; for an exception.  SEARCHDIR is a function, either
;; `re-search-backward' or `re-search-forward' indicating the direction
;; to search.  ERRWHERE is used in an error message if the limit (top or
;; bottom) of the trackback stack is encountered."
;;   (let (file line)
;;     (save-excursion
;;       (set-buffer buffer)
;;       (goto-char (srp-point start))
;;       (if (funcall searchdir srp-traceback-line-re nil t)
;; 	  (setq file (match-string 1)
;; 		line (string-to-int (match-string 2)))))
;;     (if (and file line)
;; 	(srp-jump-to-exception file line)
;;       (error "%s of traceback" errwhere))))

;; (defun srp-down-exception (&optional bottom)
;;   "Go to the next line down in the traceback.
;; With \\[univeral-argument] (programmatically, optional argument
;; BOTTOM), jump to the bottom (innermost) exception in the exception
;; stack."
;;   (interactive "P")
;;   (let* ((proc (get-process "Serpent"))
;; 	 (buffer (if proc "*Serpent*" srp-output-buffer)))
;;     (if bottom
;; 	(srp-find-next-exception 'eob buffer 're-search-backward "Bottom")
;;       (srp-find-next-exception 'eol buffer 're-search-forward "Bottom"))))

;; (defun srp-up-exception (&optional top)
;;   "Go to the previous line up in the traceback.
;; With \\[universal-argument] (programmatically, optional argument TOP)
;; jump to the top (outermost) exception in the exception stack."
;;   (interactive "P")
;;   (let* ((proc (get-process "Serpent"))
;; 	 (buffer (if proc "*Serpent*" srp-output-buffer)))
;;     (if top
;; 	(srp-find-next-exception 'bob buffer 're-search-forward "Top")
;;       (srp-find-next-exception 'bol buffer 're-search-backward "Top"))))

;; 
;; ;; Electric deletion
;; (defun srp-electric-backspace (arg)
;;   "Delete preceding character or levels of indentation.
;; Deletion is performed by calling the function in `srp-backspace-function'
;; with a single argument (the number of characters to delete).

;; If point is at the leftmost column, delete the preceding newline.

;; Otherwise, if point is at the leftmost non-whitespace character of a
;; line that is neither a continuation line nor a non-indenting comment
;; line, or if point is at the end of a blank line, this command reduces
;; the indentation to match that of the line that opened the current
;; block of code.  The line that opened the block is displayed in the
;; echo area to help you keep track of where you are.  With
;; \\[universal-argument] dedents that many blocks (but not past column
;; zero).

;; Otherwise the preceding character is deleted, converting a tab to
;; spaces if needed so that only a single column position is deleted.
;; \\[universal-argument] specifies how many characters to delete;
;; default is 1.

;; When used programmatically, argument ARG specifies the number of
;; blocks to dedent, or the number of characters to delete, as indicated
;; above."
;;   (interactive "*p")
;;   (if (or (/= (current-indentation) (current-column))
;; 	  (bolp)
;; 	  (srp-continuation-line-p)
;; ;	  (not srp-honor-comment-indentation)
;; ;	  (looking-at "#[^ \t\n]")	; non-indenting #
;; 	  )
;;       (funcall srp-backspace-function arg)
;;     ;; else indent the same as the colon line that opened the block
;;     ;; force non-blank so srp-goto-block-up doesn't ignore it
;;     (insert-char ?* 1)
;;     (backward-char)
;;     (let ((base-indent 0)		; indentation of base line
;; 	  (base-text "")		; and text of base line
;; 	  (base-found-p nil))
;;       (save-excursion
;; 	(while (< 0 arg)
;; 	  (condition-case nil		; in case no enclosing block
;; 	      (progn
;; 		(srp-goto-block-up 'no-mark)
;; 		(setq base-indent (current-indentation)
;; 		      base-text   (srp-suck-up-leading-text)
;; 		      base-found-p t))
;; 	    (error nil))
;; 	  (setq arg (1- arg))))
;;       (delete-char 1)			; toss the dummy character
;;       (delete-horizontal-space)
;;       (indent-to base-indent)
;;       (if base-found-p
;; 	  (message "Closes block: %s" base-text)))))


;; (defun srp-electric-delete (arg)
;;   "Delete preceding or following character or levels of whitespace.

;; The behavior of this function depends on the variable
;; `delete-key-deletes-forward'.  If this variable is nil (or does not
;; exist, as in older Emacsen and non-XEmacs versions), then this
;; function behaves identically to \\[c-electric-backspace].

;; If `delete-key-deletes-forward' is non-nil and is supported in your
;; Emacs, then deletion occurs in the forward direction, by calling the
;; function in `srp-delete-function'.

;; \\[universal-argument] (programmatically, argument ARG) specifies the
;; number of characters to delete (default is 1)."
;;   (interactive "*p")
;;   (if (or (and (fboundp 'delete-forward-p) ;XEmacs 21
;; 	       (delete-forward-p))
;; 	  (and (boundp 'delete-key-deletes-forward) ;XEmacs 20
;; 	       delete-key-deletes-forward))
;;       (funcall srp-delete-function arg)
;;     (srp-electric-backspace arg)))

;; ;; required for pending-del and delsel modes
;; (put 'srp-electric-colon 'delete-selection t) ;delsel
;; (put 'srp-electric-colon 'pending-delete   t) ;pending-del
;; (put 'srp-electric-backspace 'delete-selection 'supersede) ;delsel
;; (put 'srp-electric-backspace 'pending-delete   'supersede) ;pending-del
;; (put 'srp-electric-delete    'delete-selection 'supersede) ;delsel
;; (put 'srp-electric-delete    'pending-delete   'supersede) ;pending-del


;; 
;; (defun srp-indent-line (&optional arg)
;;   "Fix the indentation of the current line according to Serpent rules.
;; With \\[universal-argument] (programmatically, the optional argument
;; ARG non-nil), ignore dedenting rules for block closing statements
;; (e.g. return, raise, break, continue, pass)

;; This function is normally bound to `indent-line-function' so
;; \\[indent-for-tab-command] will call it."
;;   (interactive "P")
;;   (let* ((ci (current-indentation))
;; 	 (move-to-indentation-p (<= (current-column) ci))
;; 	 (need (srp-compute-indentation (not arg)))
;;          (cc (current-column)))
;;     ;; dedent out a level if previous command was the same unless we're in
;;     ;; column 1
;;     (if (and (equal last-command this-command)
;;              (/= cc 0))
;;         (progn
;;           (beginning-of-line)
;;           (delete-horizontal-space)
;;           (indent-to (* (/ (- cc 1) srp-indent-offset) srp-indent-offset)))
;;       (progn
;; 	;; see if we need to dedent
;; 	(if (srp-outdent-p)
;; 	    (setq need (- need srp-indent-offset)))
;; 	(if (or srp-tab-always-indent
;; 		move-to-indentation-p)
;; 	    (progn (if (/= ci need)
;; 		       (save-excursion
;; 		       (beginning-of-line)
;; 		       (delete-horizontal-space)
;; 		       (indent-to need)))
;; 		   (if move-to-indentation-p (back-to-indentation)))
;; 	    (insert-tab))))))

;; (defun srp-newline-and-indent () 
;;   "Strives to act like the Emacs `newline-and-indent'.
;; This is just `strives to' because correct indentation can't be computed
;; from scratch for Serpent code.  In general, deletes the whitespace before
;; point, inserts a newline, and takes an educated guess as to how you want
;; the new line indented."
;;  (if nil (progn
;;   (interactive)
;;   (let ((ci (current-indentation)))
;;     (if (< ci (current-column))		; if point beyond indentation
;; 	(newline-and-indent)
;;       ;; else try to act like newline-and-indent "normally" acts
;;       (beginning-of-line)
;;       (insert-char ?\n 1)
;;       (move-to-column ci))))))

;; (defun srp-compute-indentation (honor-block-close-p)
;;   "Compute Serpent indentation.
;; When HONOR-BLOCK-CLOSE-P is non-nil, statements such as `return',
;; `raise', `break', `continue', and `pass' force one level of
;; dedenting."
;;   (save-excursion
;;     (beginning-of-line)
;;     (let* ((bod (srp-point 'bod))
;; 	   (pps (parse-partial-sexp bod (point)))
;; 	   (boipps (parse-partial-sexp bod (srp-point 'boi)))
;; 	   placeholder)
;;       (cond
;;        ;; are we inside a multi-line string or comment?
;;        ((or (and (nth 3 pps) (nth 3 boipps))
;; 	    (and (nth 4 pps) (nth 4 boipps)))
;; 	(save-excursion
;; 	  (if (not srp-align-multiline-strings-p) 0
;; 	    ;; skip back over blank & non-indenting comment lines
;; 	    ;; note: will skip a blank or non-indenting comment line
;; 	    ;; that happens to be a continuation line too
;; 	    (re-search-backward "^[ \t]*\\([^ \t\n#]\\|#[ \t\n]\\)" nil 'move)
;; 	    (back-to-indentation)
;; 	    (current-column))))
;;        ;; are we on a continuation line?
;;        ((srp-continuation-line-p)
;; 	(let ((startpos (point))
;; 	      (open-bracket-pos (srp-nesting-level))
;; 	      endpos searching found state cind cline)
;; 	  (if open-bracket-pos
;; 	      (progn
;; 		(setq endpos (srp-point 'bol))
;; 		(srp-goto-initial-line)
;; 		(setq cind (current-indentation))
;; 		(setq cline cind)
;; 		(dolist (bp 
;; 			 (nth 9 (save-excursion
;; 				  (parse-partial-sexp (point) endpos)))
;; 			 cind)
;; 		  (if (search-forward "\n" bp t) (setq cline cind))
;; 		  (goto-char (1+ bp))
;; 		  (skip-chars-forward " \t")
;; 		  (setq cind (if (memq (following-char) '(?\n ?# ?\\))
;; 				 (+ cline srp-indent-offset)
;; 			       (current-column)))))
;; 	    ;; else on backslash continuation line
;; 	    (forward-line -1)
;; 	    (if (srp-continuation-line-p) ; on at least 3rd line in block
;; 		(current-indentation)	; so just continue the pattern
;; 	      ;; else started on 2nd line in block, so indent more.
;; 	      ;; if base line is an assignment with a start on a RHS,
;; 	      ;; indent to 2 beyond the leftmost "="; else skip first
;; 	      ;; chunk of non-whitespace characters on base line, + 1 more
;; 	      ;; column
;; 	      (end-of-line)
;; 	      (setq endpos (point)
;; 		    searching t)
;; 	      (back-to-indentation)
;; 	      (setq startpos (point))
;; 	      ;; look at all "=" from left to right, stopping at first
;; 	      ;; one not nested in a list or string
;; 	      (while searching
;; 		(skip-chars-forward "^=" endpos)
;; 		(if (= (point) endpos)
;; 		    (setq searching nil)
;; 		  (forward-char 1)
;; 		  (setq state (parse-partial-sexp startpos (point)))
;; 		  (if (and (zerop (car state)) ; not in a bracket
;; 			   (null (nth 3 state))) ; & not in a string
;; 		      (progn
;; 			(setq searching nil) ; done searching in any case
;; 			(setq found
;; 			      (not (or
;; 				    (eq (following-char) ?=)
;; 				    (memq (char-after (- (point) 2))
;; 					  '(?< ?> ?!)))))))))
;; 	      (if (or (not found)	; not an assignment
;; 		      (looking-at "[ \t]*\\\\")) ; <=><spaces><backslash>
;; 		  (progn
;; 		    (goto-char startpos)
;; 		    (skip-chars-forward "^ \t\n")))
;; 	      ;; if this is a continuation for a block opening
;; 	      ;; statement, add some extra offset.
;; 	      (+ (current-column) (if (srp-statement-opens-block-p)
;; 				      srp-continuation-offset 0)
;; 		 1)
;; 	      ))))

;;        ;; not on a continuation line
;;        ((bobp) (current-indentation))

;;        ;; Dfn: "Indenting comment line".  A line containing only a
;;        ;; comment, but which is treated like a statement for
;;        ;; indentation calculation purposes.  Such lines are only
;;        ;; treated specially by the mode; they are not treated
;;        ;; specially by the Serpent interpreter.

;;        ;; The rules for indenting comment lines are a line where:
;;        ;;   - the first non-whitespace character is `#', and
;;        ;;   - the character following the `#' is whitespace, and
;;        ;;   - the line is dedented with respect to (i.e. to the left
;;        ;;     of) the indentation of the preceding non-blank line.

;;        ;; The first non-blank line following an indenting comment
;;        ;; line is given the same amount of indentation as the
;;        ;; indenting comment line.

;;        ;; All other comment-only lines are ignored for indentation
;;        ;; purposes.

;;        ;; Are we looking at a comment-only line which is *not* an
;;        ;; indenting comment line?  If so, we assume that it's been
;;        ;; placed at the desired indentation, so leave it alone.
;;        ;; Indenting comment lines are aligned as statements down
;;        ;; below.
;;        ((and (looking-at "[ \t]*#[^ \t\n]")
;; 	     ;; NOTE: this test will not be performed in older Emacsen
;; 	     (fboundp 'forward-comment)
;; 	     (<= (current-indentation)
;; 		 (save-excursion
;; 		   (forward-comment (- (point-max)))
;; 		   (current-indentation))))
;; 	(current-indentation))

;;        ;; else indentation based on that of the statement that
;;        ;; precedes us; use the first line of that statement to
;;        ;; establish the base, in case the user forced a non-std
;;        ;; indentation for the continuation lines (if any)
;;        (t
;; 	;; skip back over blank & non-indenting comment lines note:
;; 	;; will skip a blank or non-indenting comment line that
;; 	;; happens to be a continuation line too.  use fast Emacs 19
;; 	;; function if it's there.
;; 	(if (and (eq srp-honor-comment-indentation nil)
;; 		 (fboundp 'forward-comment))
;; 	    (forward-comment (- (point-max)))
;; 	  (let ((prefix-re (concat srp-block-comment-prefix "[ \t]*"))
;; 		done)
;; 	    (while (not done)
;; 	      (re-search-backward "^[ \t]*\\([^ \t\n#]\\|#\\)" nil 'move)
;; 	      (setq done (or (bobp)
;; 			     (and (eq srp-honor-comment-indentation t)
;; 				  (save-excursion
;; 				    (back-to-indentation)
;; 				    (not (looking-at prefix-re))
;; 				    ))
;; 			     (and (not (eq srp-honor-comment-indentation t))
;; 				  (save-excursion
;; 				    (back-to-indentation)
;; 				    (and (not (looking-at prefix-re))
;; 					 (or (looking-at "[^#]")
;; 					     (not (zerop (current-column)))
;; 					     ))
;; 				    ))
;; 			     ))
;; 	      )))
;; 	;; if we landed inside a string, go to the beginning of that
;; 	;; string. this handles triple quoted, multi-line spanning
;; 	;; strings.
;; 	(srp-goto-beginning-of-tqs (nth 3 (parse-partial-sexp bod (point))))
;; 	;; now skip backward over continued lines
;; 	(setq placeholder (point))
;; 	(srp-goto-initial-line)
;; 	;; we may *now* have landed in a TQS, so find the beginning of
;; 	;; this string.
;; 	(srp-goto-beginning-of-tqs
;; 	 (save-excursion (nth 3 (parse-partial-sexp
;; 				 placeholder (point)))))
;; 	(+ (current-indentation)
;; 	   (if (srp-statement-opens-block-p)
;; 	       srp-indent-offset
;; 	     (if (and honor-block-close-p (srp-statement-closes-block-p))
;; 		 (- srp-indent-offset)
;; 	       0)))
;; 	)))))

;; (defun srp-guess-indent-offset (&optional global)
;;   "Guess a good value for, and change, `srp-indent-offset'.

;; By default, make a buffer-local copy of `srp-indent-offset' with the
;; new value, so that other Serpent buffers are not affected.  With
;; \\[universal-argument] (programmatically, optional argument GLOBAL),
;; change the global value of `srp-indent-offset'.  This affects all
;; Serpent buffers (that don't have their own buffer-local copy), both
;; those currently existing and those created later in the Emacs session.

;; Some people use a different value for `srp-indent-offset' than you use.
;; There's no excuse for such foolishness, but sometimes you have to deal
;; with their ugly code anyway.  This function examines the file and sets
;; `srp-indent-offset' to what it thinks it was when they created the
;; mess.

;; Specifically, it searches forward from the statement containing point,
;; looking for a line that opens a block of code.  `srp-indent-offset' is
;; set to the difference in indentation between that line and the Serpent
;; statement following it.  If the search doesn't succeed going forward,
;; it's tried again going backward."
;;   (interactive "P")			; raw prefix arg
;;   (let (new-value
;; 	(start (point))
;; 	(restart (point))
;; 	(found nil)
;; 	colon-indent)
;;     (srp-goto-initial-line)
;;     (while (not (or found (eobp)))
;;       (when (and (re-search-forward ":[ \t]*\\($\\|[#\\]\\)" nil 'move)
;; 		 (not (srp-in-literal restart)))
;; 	(setq restart (point))
;; 	(srp-goto-initial-line)
;; 	(if (srp-statement-opens-block-p)
;; 	    (setq found t)
;; 	  (goto-char restart))))
;;     (unless found
;;       (goto-char start)
;;       (srp-goto-initial-line)
;;       (while (not (or found (bobp)))
;; 	(setq found (and
;; 		     (re-search-backward ":[ \t]*\\($\\|[#\\]\\)" nil 'move)
;; 		     (or (srp-goto-initial-line) t) ; always true -- side effect
;; 		     (srp-statement-opens-block-p)))))
;;     (setq colon-indent (current-indentation)
;; 	  found (and found (zerop (srp-next-statement 1)))
;; 	  new-value (- (current-indentation) colon-indent))
;;     (goto-char start)
;;     (if (not found)
;; 	(error "Sorry, couldn't guess a value for srp-indent-offset")
;;       (funcall (if global 'kill-local-variable 'make-local-variable)
;; 	       'srp-indent-offset)
;;       (setq srp-indent-offset new-value)
;;       (or noninteractive
;; 	  (message "%s value of srp-indent-offset set to %d"
;; 		   (if global "Global" "Local")
;; 		   srp-indent-offset)))
;;     ))

;; (defun srp-comment-indent-function ()
;;   "Serpent version of `comment-indent-function'."
;;   ;; This is required when filladapt is turned off.  Without it, when
;;   ;; filladapt is not used, comments which start in column zero
;;   ;; cascade one character to the right
;;   (save-excursion
;;     (beginning-of-line)
;;     (let ((eol (srp-point 'eol)))
;;       (and comment-start-skip
;; 	   (re-search-forward comment-start-skip eol t)
;; 	   (setq eol (match-beginning 0)))
;;       (goto-char eol)
;;       (skip-chars-backward " \t")
;;       (max comment-column (+ (current-column) (if (bolp) 0 1)))
;;       )))

;; (defun srp-narrow-to-defun (&optional class)
;;   "Make text outside current defun invisible.
;; The defun visible is the one that contains point or follows point.
;; Optional CLASS is passed directly to `srp-beginning-of-def-or-class'."
;;   (interactive "P")
;;   (save-excursion
;;     (widen)
;;     (srp-end-of-def-or-class class)
;;     (let ((end (point)))
;;       (srp-beginning-of-def-or-class class)
;;       (narrow-to-region (point) end))))

;; 
;; (defun srp-shift-region (start end count)
;;   "Indent lines from START to END by COUNT spaces."
;;   (save-excursion
;;     (goto-char end)
;;     (beginning-of-line)
;;     (setq end (point))
;;     (goto-char start)
;;     (beginning-of-line)
;;     (setq start (point))
;;     (indent-rigidly start end count)))

;; (defun srp-shift-region-left (start end &optional count)
;;   "Shift region of Serpent code to the left.
;; The lines from the line containing the start of the current region up
;; to (but not including) the line containing the end of the region are
;; shifted to the left, by `srp-indent-offset' columns.

;; If a prefix argument is given, the region is instead shifted by that
;; many columns.  With no active region, dedent only the current line.
;; You cannot dedent the region if any line is already at column zero."
;;   (interactive
;;    (let ((p (point))
;; 	 (m (mark))
;; 	 (arg current-prefix-arg))
;;      (if m
;; 	 (list (min p m) (max p m) arg)
;;        (list p (save-excursion (forward-line 1) (point)) arg))))
;;   ;; if any line is at column zero, don't shift the region
;;   (save-excursion
;;     (goto-char start)
;;     (while (< (point) end)
;;       (back-to-indentation)
;;       (if (and (zerop (current-column))
;; 	       (not (looking-at "\\s *$")))
;; 	  (error "Region is at left edge"))
;;       (forward-line 1)))
;;   (srp-shift-region start end (- (prefix-numeric-value
;; 				 (or count srp-indent-offset))))
;;   (srp-keep-region-active))

;; (defun srp-shift-region-right (start end &optional count)
;;   "Shift region of Serpent code to the right.
;; The lines from the line containing the start of the current region up
;; to (but not including) the line containing the end of the region are
;; shifted to the right, by `srp-indent-offset' columns.

;; If a prefix argument is given, the region is instead shifted by that
;; many columns.  With no active region, indent only the current line."
;;   (interactive
;;    (let ((p (point))
;; 	 (m (mark))
;; 	 (arg current-prefix-arg))
;;      (if m
;; 	 (list (min p m) (max p m) arg)
;;        (list p (save-excursion (forward-line 1) (point)) arg))))
;;   (srp-shift-region start end (prefix-numeric-value
;; 			      (or count srp-indent-offset)))
;;   (srp-keep-region-active))

;; (defun srp-indent-region (start end &optional indent-offset)
;;   "Reindent a region of Serpent code.

;; The lines from the line containing the start of the current region up
;; to (but not including) the line containing the end of the region are
;; reindented.  If the first line of the region has a non-whitespace
;; character in the first column, the first line is left alone and the
;; rest of the region is reindented with respect to it.  Else the entire
;; region is reindented with respect to the (closest code or indenting
;; comment) statement immediately preceding the region.

;; This is useful when code blocks are moved or yanked, when enclosing
;; control structures are introduced or removed, or to reformat code
;; using a new value for the indentation offset.

;; If a numeric prefix argument is given, it will be used as the value of
;; the indentation offset.  Else the value of `srp-indent-offset' will be
;; used.

;; Warning: The region must be consistently indented before this function
;; is called!  This function does not compute proper indentation from
;; scratch (that's impossible in Serpent), it merely adjusts the existing
;; indentation to be correct in context.

;; Warning: This function really has no idea what to do with
;; non-indenting comment lines, and shifts them as if they were indenting
;; comment lines.  Fixing this appears to require telepathy.

;; Special cases: whitespace is deleted from blank lines; continuation
;; lines are shifted by the same amount their initial line was shifted,
;; in order to preserve their relative indentation with respect to their
;; initial line; and comment lines beginning in column 1 are ignored."
;;   (interactive "*r\nP")			; region; raw prefix arg
;;   (save-excursion
;;     (goto-char end)   (beginning-of-line) (setq end (point-marker))
;;     (goto-char start) (beginning-of-line)
;;     (let ((srp-indent-offset (prefix-numeric-value
;; 			     (or indent-offset srp-indent-offset)))
;; 	  (indents '(-1))		; stack of active indent levels
;; 	  (target-column 0)		; column to which to indent
;; 	  (base-shifted-by 0)		; amount last base line was shifted
;; 	  (indent-base (if (looking-at "[ \t\n]")
;; 			   (srp-compute-indentation t)
;; 			 0))
;; 	  ci)
;;       (while (< (point) end)
;; 	(setq ci (current-indentation))
;; 	;; figure out appropriate target column
;; 	(cond
;; 	 ((or (eq (following-char) ?#)	; comment in column 1
;; 	      (looking-at "[ \t]*$"))	; entirely blank
;; 	  (setq target-column 0))
;; 	 ((srp-continuation-line-p)	; shift relative to base line
;; 	  (setq target-column (+ ci base-shifted-by)))
;; 	 (t				; new base line
;; 	  (if (> ci (car indents))	; going deeper; push it
;; 	      (setq indents (cons ci indents))
;; 	    ;; else we should have seen this indent before
;; 	    (setq indents (memq ci indents)) ; pop deeper indents
;; 	    (if (null indents)
;; 		(error "Bad indentation in region, at line %d"
;; 		       (save-restriction
;; 			 (widen)
;; 			 (1+ (count-lines 1 (point)))))))
;; 	  (setq target-column (+ indent-base
;; 				 (* srp-indent-offset
;; 				    (- (length indents) 2))))
;; 	  (setq base-shifted-by (- target-column ci))))
;; 	;; shift as needed
;; 	(if (/= ci target-column)
;; 	    (progn
;; 	      (delete-horizontal-space)
;; 	      (indent-to target-column)))
;; 	(forward-line 1))))
;;   (set-marker end nil))

;; (defun srp-comment-region (beg end &optional arg)
;;   "Like `comment-region' but uses double hash (`#') comment starter."
;;   (interactive "r\nP")
;;   (let ((comment-start srp-block-comment-prefix))
;;     (comment-region beg end arg)))

;; ;; Functions for moving point
;; (defun srp-previous-statement (count)
;;   "Go to the start of the COUNTth preceding Serpent statement.
;; By default, goes to the previous statement.  If there is no such
;; statement, goes to the first statement.  Return count of statements
;; left to move.  `Statements' do not include blank, comment, or
;; continuation lines."
;;   (interactive "p")			; numeric prefix arg
;;   (if (< count 0) (srp-next-statement (- count))
;;     (srp-goto-initial-line)
;;     (let (start)
;;       (while (and
;; 	      (setq start (point))	; always true -- side effect
;; 	      (> count 0)
;; 	      (zerop (forward-line -1))
;; 	      (srp-goto-statement-at-or-above))
;; 	(setq count (1- count)))
;;       (if (> count 0) (goto-char start)))
;;     count))

;; (defun srp-next-statement (count)
;;   "Go to the start of next Serpent statement.
;; If the statement at point is the i'th Serpent statement, goes to the
;; start of statement i+COUNT.  If there is no such statement, goes to the
;; last statement.  Returns count of statements left to move.  `Statements'
;; do not include blank, comment, or continuation lines."
;;   (interactive "p")			; numeric prefix arg
;;   (if (< count 0) (srp-previous-statement (- count))
;;     (beginning-of-line)
;;     (let (start)
;;       (while (and
;; 	      (setq start (point))	; always true -- side effect
;; 	      (> count 0)
;; 	      (srp-goto-statement-below))
;; 	(setq count (1- count)))
;;       (if (> count 0) (goto-char start)))
;;     count))

;; (defun srp-goto-block-up (&optional nomark)
;;   "Move up to start of current block.
;; Go to the statement that starts the smallest enclosing block; roughly
;; speaking, this will be the closest preceding statement that ends with a
;; colon and is indented less than the statement you started on.  If
;; successful, also sets the mark to the starting point.

;; `\\[srp-mark-block]' can be used afterward to mark the whole code
;; block, if desired.

;; If called from a program, the mark will not be set if optional argument
;; NOMARK is not nil."
;;   (interactive)
;;   (let ((start (point))
;; 	(found nil)
;; 	initial-indent)
;;     (srp-goto-initial-line)
;;     ;; if on blank or non-indenting comment line, use the preceding stmt
;;     (if (looking-at "[ \t]*\\($\\|#[^ \t\n]\\)")
;; 	(progn
;; 	  (srp-goto-statement-at-or-above)
;; 	  (setq found (srp-statement-opens-block-p))))
;;     ;; search back for colon line indented less
;;     (setq initial-indent (current-indentation))
;;     (if (zerop initial-indent)
;; 	;; force fast exit
;; 	(goto-char (point-min)))
;;     (while (not (or found (bobp)))
;;       (setq found
;; 	    (and
;; 	     (re-search-backward ":[ \t]*\\($\\|[#\\]\\)" nil 'move)
;; 	     (or (srp-goto-initial-line) t) ; always true -- side effect
;; 	     (< (current-indentation) initial-indent)
;; 	     (srp-statement-opens-block-p))))
;;     (if found
;; 	(progn
;; 	  (or nomark (push-mark start))
;; 	  (back-to-indentation))
;;       (goto-char start)
;;       (error "Enclosing block not found"))))

;; (defun srp-beginning-of-def-or-class (&optional class count)
;;   "Move point to start of `def' or `class'.

;; Searches back for the closest preceding `def'.  If you supply a prefix
;; arg, looks for a `class' instead.  The docs below assume the `def'
;; case; just substitute `class' for `def' for the other case.
;; Programmatically, if CLASS is `either', then moves to either `class'
;; or `def'.

;; When second optional argument is given programmatically, move to the
;; COUNTth start of `def'.

;; If point is in a `def' statement already, and after the `d', simply
;; moves point to the start of the statement.

;; Otherwise (i.e. when point is not in a `def' statement, or at or
;; before the `d' of a `def' statement), searches for the closest
;; preceding `def' statement, and leaves point at its start.  If no such
;; statement can be found, leaves point at the start of the buffer.

;; Returns t iff a `def' statement is found by these rules.

;; Note that doing this command repeatedly will take you closer to the
;; start of the buffer each time.

;; To mark the current `def', see `\\[srp-mark-def-or-class]'."
;;   (interactive "P")			; raw prefix arg
;;   (setq count (or count 1))
;;   (let ((at-or-before-p (<= (current-column) (current-indentation)))
;; 	(start-of-line (goto-char (srp-point 'bol)))
;; 	(start-of-stmt (goto-char (srp-point 'bos)))
;; 	(start-re (cond ((eq class 'either) "^[ \t]*\\(class\\|def\\)\\>")
;; 			(class "^[ \t]*class\\>")
;; 			(t "^[ \t]*def\\>")))
;; 	)
;;     ;; searching backward
;;     (if (and (< 0 count)
;; 	     (or (/= start-of-stmt start-of-line)
;; 		 (not at-or-before-p)))
;; 	(end-of-line))
;;     ;; search forward
;;     (if (and (> 0 count)
;; 	     (zerop (current-column))
;; 	     (looking-at start-re))
;; 	(end-of-line))
;;     (if (re-search-backward start-re nil 'move count)
;; 	(goto-char (match-beginning 0)))))

;; ;; Backwards compatibility
;; (defalias 'beginning-of-serpent-def-or-class 'srp-beginning-of-def-or-class)

;; (defun srp-end-of-def-or-class (&optional class count)
;;   "Move point beyond end of `def' or `class' body.

;; By default, looks for an appropriate `def'.  If you supply a prefix
;; arg, looks for a `class' instead.  The docs below assume the `def'
;; case; just substitute `class' for `def' for the other case.
;; Programmatically, if CLASS is `either', then moves to either `class'
;; or `def'.

;; When second optional argument is given programmatically, move to the
;; COUNTth end of `def'.

;; If point is in a `def' statement already, this is the `def' we use.

;; Else, if the `def' found by `\\[srp-beginning-of-def-or-class]'
;; contains the statement you started on, that's the `def' we use.

;; Otherwise, we search forward for the closest following `def', and use that.

;; If a `def' can be found by these rules, point is moved to the start of
;; the line immediately following the `def' block, and the position of the
;; start of the `def' is returned.

;; Else point is moved to the end of the buffer, and nil is returned.

;; Note that doing this command repeatedly will take you closer to the
;; end of the buffer each time.

;; To mark the current `def', see `\\[srp-mark-def-or-class]'."
;;   (interactive "P")			; raw prefix arg
;;   (if (and count (/= count 1))
;;       (srp-beginning-of-def-or-class (- 1 count)))
;;   (let ((start (progn (srp-goto-initial-line) (point)))
;; 	(which (cond ((eq class 'either) "\\(class\\|def\\)")
;; 		     (class "class")
;; 		     (t "def")))
;; 	(state 'not-found))
;;     ;; move point to start of appropriate def/class
;;     (if (looking-at (concat "[ \t]*" which "\\>")) ; already on one
;; 	(setq state 'at-beginning)
;;       ;; else see if srp-beginning-of-def-or-class hits container
;;       (if (and (srp-beginning-of-def-or-class class)
;; 	       (progn (srp-goto-beyond-block)
;; 		      (> (point) start)))
;; 	  (setq state 'at-end)
;; 	;; else search forward
;; 	(goto-char start)
;; 	(if (re-search-forward (concat "^[ \t]*" which "\\>") nil 'move)
;; 	    (progn (setq state 'at-beginning)
;; 		   (beginning-of-line)))))
;;     (cond
;;      ((eq state 'at-beginning) (srp-goto-beyond-block) t)
;;      ((eq state 'at-end) t)
;;      ((eq state 'not-found) nil)
;;      (t (error "Internal error in `srp-end-of-def-or-class'")))))

;; ;; Backwards compabitility
;; (defalias 'end-of-serpent-def-or-class 'srp-end-of-def-or-class)

;; 
;; ;; Functions for marking regions
;; (defun srp-mark-block (&optional extend just-move)
;;   "Mark following block of lines.  With prefix arg, mark structure.
;; Easier to use than explain.  It sets the region to an `interesting'
;; block of succeeding lines.  If point is on a blank line, it goes down to
;; the next non-blank line.  That will be the start of the region.  The end
;; of the region depends on the kind of line at the start:

;;  - If a comment, the region will include all succeeding comment lines up
;;    to (but not including) the next non-comment line (if any).

;;  - Else if a prefix arg is given, and the line begins one of these
;;    structures:

;;      if elif else try except finally for while def class

;;    the region will be set to the body of the structure, including
;;    following blocks that `belong' to it, but excluding trailing blank
;;    and comment lines.  E.g., if on a `try' statement, the `try' block
;;    and all (if any) of the following `except' and `finally' blocks
;;    that belong to the `try' structure will be in the region.  Ditto
;;    for if/elif/else, for/else and while/else structures, and (a bit
;;    degenerate, since they're always one-block structures) def and
;;    class blocks.

;;  - Else if no prefix argument is given, and the line begins a Serpent
;;    block (see list above), and the block is not a `one-liner' (i.e.,
;;    the statement ends with a colon, not with code), the region will
;;    include all succeeding lines up to (but not including) the next
;;    code statement (if any) that's indented no more than the starting
;;    line, except that trailing blank and comment lines are excluded.
;;    E.g., if the starting line begins a multi-statement `def'
;;    structure, the region will be set to the full function definition,
;;    but without any trailing `noise' lines.

;;  - Else the region will include all succeeding lines up to (but not
;;    including) the next blank line, or code or indenting-comment line
;;    indented strictly less than the starting line.  Trailing indenting
;;    comment lines are included in this case, but not trailing blank
;;    lines.

;; A msg identifying the location of the mark is displayed in the echo
;; area; or do `\\[exchange-point-and-mark]' to flip down to the end.

;; If called from a program, optional argument EXTEND plays the role of
;; the prefix arg, and if optional argument JUST-MOVE is not nil, just
;; moves to the end of the block (& does not set mark or display a msg)."
;;   (interactive "P")			; raw prefix arg
;;   (srp-goto-initial-line)
;;   ;; skip over blank lines
;;   (while (and
;; 	  (looking-at "[ \t]*$")	; while blank line
;; 	  (not (eobp)))			; & somewhere to go
;;     (forward-line 1))
;;   (if (eobp)
;;       (error "Hit end of buffer without finding a non-blank stmt"))
;;   (let ((initial-pos (point))
;; 	(initial-indent (current-indentation))
;; 	last-pos			; position of last stmt in region
;; 	(followers
;; 	 '((if elif else) (elif elif else) (else)
;; 	   (try except finally) (except except) (finally)
;; 	   (for else) (while else)
;; 	   (def) (class) ) )
;; 	first-symbol next-symbol)

;;     (cond
;;      ;; if comment line, suck up the following comment lines
;;      ((looking-at "[ \t]*#")
;;       (re-search-forward "^[ \t]*[^ \t#]" nil 'move) ; look for non-comment
;;       (re-search-backward "^[ \t]*#")	; and back to last comment in block
;;       (setq last-pos (point)))

;;      ;; else if line is a block line and EXTEND given, suck up
;;      ;; the whole structure
;;      ((and extend
;; 	   (setq first-symbol (srp-suck-up-first-keyword) )
;; 	   (assq first-symbol followers))
;;       (while (and
;; 	      (or (srp-goto-beyond-block) t) ; side effect
;; 	      (forward-line -1)		; side effect
;; 	      (setq last-pos (point))	; side effect
;; 	      (srp-goto-statement-below)
;; 	      (= (current-indentation) initial-indent)
;; 	      (setq next-symbol (srp-suck-up-first-keyword))
;; 	      (memq next-symbol (cdr (assq first-symbol followers))))
;; 	(setq first-symbol next-symbol)))

;;      ;; else if line *opens* a block, search for next stmt indented <=
;;      ((srp-statement-opens-block-p)
;;       (while (and
;; 	      (setq last-pos (point))	; always true -- side effect
;; 	      (srp-goto-statement-below)
;; 	      (> (current-indentation) initial-indent)
;; 	      )))

;;      ;; else plain code line; stop at next blank line, or stmt or
;;      ;; indenting comment line indented <
;;      (t
;;       (while (and
;; 	      (setq last-pos (point))	; always true -- side effect
;; 	      (or (srp-goto-beyond-final-line) t)
;; 	      (not (looking-at "[ \t]*$")) ; stop at blank line
;; 	      (or
;; 	       (>= (current-indentation) initial-indent)
;; 	       (looking-at "[ \t]*#[^ \t\n]"))) ; ignore non-indenting #
;; 	nil)))

;;     ;; skip to end of last stmt
;;     (goto-char last-pos)
;;     (srp-goto-beyond-final-line)

;;     ;; set mark & display
;;     (if just-move
;; 	()				; just return
;;       (push-mark (point) 'no-msg)
;;       (forward-line -1)
;;       (message "Mark set after: %s" (srp-suck-up-leading-text))
;;       (goto-char initial-pos))))

;; (defun srp-mark-def-or-class (&optional class)
;;   "Set region to body of def (or class, with prefix arg) enclosing point.
;; Pushes the current mark, then point, on the mark ring (all language
;; modes do this, but although it's handy it's never documented ...).

;; In most Emacs language modes, this function bears at least a
;; hallucinogenic resemblance to `\\[srp-end-of-def-or-class]' and
;; `\\[srp-beginning-of-def-or-class]'.

;; And in earlier versions of Serpent mode, all 3 were tightly connected.
;; Turned out that was more confusing than useful: the `goto start' and
;; `goto end' commands are usually used to search through a file, and
;; people expect them to act a lot like `search backward' and `search
;; forward' string-search commands.  But because Serpent `def' and `class'
;; can nest to arbitrary levels, finding the smallest def containing
;; point cannot be done via a simple backward search: the def containing
;; point may not be the closest preceding def, or even the closest
;; preceding def that's indented less.  The fancy algorithm required is
;; appropriate for the usual uses of this `mark' command, but not for the
;; `goto' variations.

;; So the def marked by this command may not be the one either of the
;; `goto' commands find: If point is on a blank or non-indenting comment
;; line, moves back to start of the closest preceding code statement or
;; indenting comment line.  If this is a `def' statement, that's the def
;; we use.  Else searches for the smallest enclosing `def' block and uses
;; that.  Else signals an error.

;; When an enclosing def is found: The mark is left immediately beyond
;; the last line of the def block.  Point is left at the start of the
;; def, except that: if the def is preceded by a number of comment lines
;; followed by (at most) one optional blank line, point is left at the
;; start of the comments; else if the def is preceded by a blank line,
;; point is left at its start.

;; The intent is to mark the containing def/class and its associated
;; documentation, to make moving and duplicating functions and classes
;; pleasant."
;;   (interactive "P")			; raw prefix arg
;;   (let ((start (point))
;; 	(which (cond ((eq class 'either) "\\(class\\|def\\)")
;; 		     (class "class")
;; 		     (t "def"))))
;;     (push-mark start)
;;     (if (not (srp-go-up-tree-to-keyword which))
;; 	(progn (goto-char start)
;; 	       (error "Enclosing %s not found"
;; 		      (if (eq class 'either)
;; 			  "def or class"
;; 			which)))
;;       ;; else enclosing def/class found
;;       (setq start (point))
;;       (srp-goto-beyond-block)
;;       (push-mark (point))
;;       (goto-char start)
;;       (if (zerop (forward-line -1))	; if there is a preceding line
;; 	  (progn
;; 	    (if (looking-at "[ \t]*$")	; it's blank
;; 		(setq start (point))	; so reset start point
;; 	      (goto-char start))	; else try again
;; 	    (if (zerop (forward-line -1))
;; 		(if (looking-at "[ \t]*#") ; a comment
;; 		    ;; look back for non-comment line
;; 		    ;; tricky: note that the regexp matches a blank
;; 		    ;; line, cuz \n is in the 2nd character class
;; 		    (and
;; 		     (re-search-backward "^[ \t]*[^ \t#]" nil 'move)
;; 		     (forward-line 1))
;; 		  ;; no comment, so go back
;; 		  (goto-char start)))))))
;;   (exchange-point-and-mark)
;;   (srp-keep-region-active))

;; ;; ripped from cc-mode
;; (defun srp-forward-into-nomenclature (&optional arg)
;;   "Move forward to end of a nomenclature section or word.
;; With \\[universal-argument] (programmatically, optional argument ARG),
;; do it that many times.

;; A `nomenclature' is a fancy way of saying AWordWithMixedCaseNotUnderscores."
;;   (interactive "p")
;;   (let ((case-fold-search nil))
;;     (if (> arg 0)
;; 	(re-search-forward
;; 	 "\\(\\W\\|[_]\\)*\\([A-Z]*[a-z0-9]*\\)"
;; 	 (point-max) t arg)
;;       (while (and (< arg 0)
;; 		  (re-search-backward
;; 		   "\\(\\W\\|[a-z0-9]\\)[A-Z]+\\|\\(\\W\\|[_]\\)\\w+"
;; 		   (point-min) 0))
;; 	(forward-char 1)
;; 	(setq arg (1+ arg)))))
;;   (srp-keep-region-active))

;; (defun srp-backward-into-nomenclature (&optional arg)
;;   "Move backward to beginning of a nomenclature section or word.
;; With optional ARG, move that many times.  If ARG is negative, move
;; forward.

;; A `nomenclature' is a fancy way of saying AWordWithMixedCaseNotUnderscores."
;;   (interactive "p")
;;   (srp-forward-into-nomenclature (- arg))
;;   (srp-keep-region-active))


;; 
;; ;; pdbtrack functions
;; (defun srp-pdbtrack-toggle-stack-tracking (arg)
;;   (interactive "P")
;;   (if (not (get-buffer-process (current-buffer)))
;;       (error "No process associated with buffer '%s'" (current-buffer)))
;;   ;; missing or 0 is toggle, >0 turn on, <0 turn off
;;   (if (or (not arg)
;; 	  (zerop (setq arg (prefix-numeric-value arg))))
;;       (setq srp-pdbtrack-do-tracking-p (not srp-pdbtrack-do-tracking-p))
;;     (setq srp-pdbtrack-do-tracking-p (> arg 0)))
;;   (message "%sabled Serpent's pdbtrack"
;;            (if srp-pdbtrack-do-tracking-p "En" "Dis")))

;; (defun turn-on-pdbtrack ()
;;   (interactive)
;;   (srp-pdbtrack-toggle-stack-tracking 1))

;; (defun turn-off-pdbtrack ()
;;   (interactive)
;;   (srp-pdbtrack-toggle-stack-tracking 0))


;; 
;; ;; Pychecker

;; ;; hack for FSF Emacs
;; (unless (fboundp 'read-shell-command)
;;   (defalias 'read-shell-command 'read-string))

;; (defun srp-pychecker-run (command)
;;   "*Run pychecker (default on the file currently visited)."
;;   (interactive
;;    (let ((default
;;            (format "%s %s %s" srp-pychecker-command
;; 		   (mapconcat 'identity srp-pychecker-command-args " ")
;; 		   (buffer-file-name)))
;; 	 (last (when srp-pychecker-history
;; 		 (let* ((lastcmd (car srp-pychecker-history))
;; 			(cmd (cdr (reverse (split-string lastcmd))))
;; 			(newcmd (reverse (cons (buffer-file-name) cmd))))
;; 		   (mapconcat 'identity newcmd " ")))))

;;      (list
;;       (if (fboundp 'read-shell-command)
;; 	  (read-shell-command "Run pychecker like this: "
;; 			      (if last
;; 				  last
;; 				default)
;; 			      'srp-pychecker-history)
;; 	(read-string "Run pychecker like this: "
;; 		     (if last
;; 			 last
;; 		       default)
;; 		     'srp-pychecker-history))
;; 	)))
;;   (save-some-buffers (not srp-ask-about-save) nil)
;;   (compile-internal command "No more errors"))


;; 
;; ;; pydoc commands. The guts of this function is stolen from XEmacs's
;; ;; symbol-near-point, but without the useless regexp-quote call on the
;; ;; results, nor the interactive bit.  Also, we've added the temporary
;; ;; syntax table setting, which Skip originally had broken out into a
;; ;; separate function.  Note that Emacs doesn't have the original
;; ;; function.
;; (defun srp-symbol-near-point ()
;;   "Return the first textual item to the nearest point."
;;   ;; alg stolen from etag.el
;;   (save-excursion
;;     (with-syntax-table srp-dotted-expression-syntax-table
;;       (if (or (bobp) (not (memq (char-syntax (char-before)) '(?w ?_))))
;; 	  (while (not (looking-at "\\sw\\|\\s_\\|\\'"))
;; 	    (forward-char 1)))
;;       (while (looking-at "\\sw\\|\\s_")
;; 	(forward-char 1))
;;       (if (re-search-backward "\\sw\\|\\s_" nil t)
;; 	  (progn (forward-char 1)
;; 		 (buffer-substring (point)
;; 				   (progn (forward-sexp -1)
;; 					  (while (looking-at "\\s'")
;; 					    (forward-char 1))
;; 					  (point))))
;; 	nil))))

;; (defun srp-help-at-point ()
;;   "Get help from Serpent based on the symbol nearest point."
;;   (interactive)
;;   (let* ((sym (srp-symbol-near-point))
;; 	 (base (substring sym 0 (or (search "." sym :from-end t) 0)))
;; 	 cmd)
;;     (if (not (equal base ""))
;;         (setq cmd (concat "import " base "\n")))
;;     (setq cmd (concat "import pydoc\n"
;;                       cmd
;; 		      "try: pydoc.help('" sym "')\n"
;; 		      "except: print 'No help available on:', \"" sym "\""))
;;     (message cmd)
;;     (srp-execute-string cmd)
;;     (set-buffer "*Serpent Output*")
;;     ;; BAW: Should we really be leaving the output buffer in help-mode?
;;     (help-mode)))


;; 
;; ;; Documentation functions

;; ;; dump the long form of the mode blurb; does the usual doc escapes,
;; ;; plus lines of the form ^[vc]:name$ to suck variable & command docs
;; ;; out of the right places, along with the keys they're on & current
;; ;; values
;; (defun srp-dump-help-string (str)
;;   (with-output-to-temp-buffer "*Help*"
;;     (let ((locals (buffer-local-variables))
;; 	  funckind funcname func funcdoc
;; 	  (start 0) mstart end
;; 	  keys )
;;       (while (string-match "^%\\([vc]\\):\\(.+\\)\n" str start)
;; 	(setq mstart (match-beginning 0)  end (match-end 0)
;; 	      funckind (substring str (match-beginning 1) (match-end 1))
;; 	      funcname (substring str (match-beginning 2) (match-end 2))
;; 	      func (intern funcname))
;; 	(princ (substitute-command-keys (substring str start mstart)))
;; 	(cond
;; 	 ((equal funckind "c")		; command
;; 	  (setq funcdoc (documentation func)
;; 		keys (concat
;; 		      "Key(s): "
;; 		      (mapconcat 'key-description
;; 				 (where-is-internal func srp-mode-map)
;; 				 ", "))))
;; 	 ((equal funckind "v")		; variable
;; 	  (setq funcdoc (documentation-property func 'variable-documentation)
;; 		keys (if (assq func locals)
;; 			 (concat
;; 			  "Local/Global values: "
;; 			  (prin1-to-string (symbol-value func))
;; 			  " / "
;; 			  (prin1-to-string (default-value func)))
;; 		       (concat
;; 			"Value: "
;; 			(prin1-to-string (symbol-value func))))))
;; 	 (t				; unexpected
;; 	  (error "Error in srp-dump-help-string, tag `%s'" funckind)))
;; 	(princ (format "\n-> %s:\t%s\t%s\n\n"
;; 		       (if (equal funckind "c") "Command" "Variable")
;; 		       funcname keys))
;; 	(princ funcdoc)
;; 	(terpri)
;; 	(setq start end))
;;       (princ (substitute-command-keys (substring str start))))
;;     (print-help-return-message)))

;; (defun srp-describe-mode ()
;;   "Dump long form of Serpent-mode docs."
;;   (interactive)
;;   (srp-dump-help-string "Major mode for editing Serpent files.
;; Knows about Serpent indentation, tokens, comments and continuation lines.
;; Paragraphs are separated by blank lines only.

;; Major sections below begin with the string `@'; specific function and
;; variable docs begin with `->'.

;; @EXECUTING SERPENT CODE

;; \\[srp-execute-import-or-reload]\timports or reloads the file in the Serpent interpreter
;; \\[srp-execute-buffer]\tsends the entire buffer to the Serpent interpreter
;; \\[srp-execute-region]\tsends the current region
;; \\[srp-execute-def-or-class]\tsends the current function or class definition
;; \\[srp-execute-string]\tsends an arbitrary string
;; \\[srp-shell]\tstarts a Serpent interpreter window; this will be used by
;; \tsubsequent Serpent execution commands
;; %c:srp-execute-import-or-reload
;; %c:srp-execute-buffer
;; %c:srp-execute-region
;; %c:srp-execute-def-or-class
;; %c:srp-execute-string
;; %c:srp-shell

;; @VARIABLES

;; srp-indent-offset\tindentation increment
;; srp-block-comment-prefix\tcomment string used by comment-region

;; srp-serpent-command\tshell command to invoke Serpent interpreter
;; srp-temp-directory\tdirectory used for temp files (if needed)

;; srp-beep-if-tab-change\tring the bell if tab-width is changed
;; %v:srp-indent-offset
;; %v:srp-block-comment-prefix
;; %v:srp-serpent-command
;; %v:srp-temp-directory
;; %v:srp-beep-if-tab-change

;; @KINDS OF LINES

;; Each physical line in the file is either a `continuation line' (the
;; preceding line ends with a backslash that's not part of a comment, or
;; the paren/bracket/brace nesting level at the start of the line is
;; non-zero, or both) or an `initial line' (everything else).

;; An initial line is in turn a `blank line' (contains nothing except
;; possibly blanks or tabs), a `comment line' (leftmost non-blank
;; character is `#'), or a `code line' (everything else).

;; Comment Lines

;; Although all comment lines are treated alike by Serpent, Serpent mode
;; recognizes two kinds that act differently with respect to indentation.

;; An `indenting comment line' is a comment line with a blank, tab or
;; nothing after the initial `#'.  The indentation commands (see below)
;; treat these exactly as if they were code lines: a line following an
;; indenting comment line will be indented like the comment line.  All
;; other comment lines (those with a non-whitespace character immediately
;; following the initial `#') are `non-indenting comment lines', and
;; their indentation is ignored by the indentation commands.

;; Indenting comment lines are by far the usual case, and should be used
;; whenever possible.  Non-indenting comment lines are useful in cases
;; like these:

;; \ta = b   # a very wordy single-line comment that ends up being
;; \t        #... continued onto another line

;; \tif a == b:
;; ##\t\tprint 'panic!' # old code we've `commented out'
;; \t\treturn a

;; Since the `#...' and `##' comment lines have a non-whitespace
;; character following the initial `#', Serpent mode ignores them when
;; computing the proper indentation for the next line.

;; Continuation Lines and Statements

;; The Serpent-mode commands generally work on statements instead of on
;; individual lines, where a `statement' is a comment or blank line, or a
;; code line and all of its following continuation lines (if any)
;; considered as a single logical unit.  The commands in this mode
;; generally (when it makes sense) automatically move to the start of the
;; statement containing point, even if point happens to be in the middle
;; of some continuation line.


;; @INDENTATION

;; Primarily for entering new code:
;; \t\\[indent-for-tab-command]\t indent line appropriately
;; \t\\[srp-newline-and-indent]\t insert newline, then indent
;; \t\\[srp-electric-backspace]\t reduce indentation, or delete single character

;; Primarily for reindenting existing code:
;; \t\\[srp-guess-indent-offset]\t guess srp-indent-offset from file content; change locally
;; \t\\[universal-argument] \\[srp-guess-indent-offset]\t ditto, but change globally

;; \t\\[srp-indent-region]\t reindent region to match its context
;; \t\\[srp-shift-region-left]\t shift region left by srp-indent-offset
;; \t\\[srp-shift-region-right]\t shift region right by srp-indent-offset

;; Unlike most programming languages, Serpent uses indentation, and only
;; indentation, to specify block structure.  Hence the indentation supplied
;; automatically by Serpent-mode is just an educated guess:  only you know
;; the block structure you intend, so only you can supply correct
;; indentation.

;; The \\[indent-for-tab-command] and \\[srp-newline-and-indent] keys try to suggest plausible indentation, based on
;; the indentation of preceding statements.  E.g., assuming
;; srp-indent-offset is 4, after you enter
;; \tif a > 0: \\[srp-newline-and-indent]
;; the cursor will be moved to the position of the `_' (_ is not a
;; character in the file, it's just used here to indicate the location of
;; the cursor):
;; \tif a > 0:
;; \t    _
;; If you then enter `c = d' \\[srp-newline-and-indent], the cursor will move
;; to
;; \tif a > 0:
;; \t    c = d
;; \t    _
;; Serpent-mode cannot know whether that's what you intended, or whether
;; \tif a > 0:
;; \t    c = d
;; \t_
;; was your intent.  In general, Serpent-mode either reproduces the
;; indentation of the (closest code or indenting-comment) preceding
;; statement, or adds an extra srp-indent-offset blanks if the preceding
;; statement has `:' as its last significant (non-whitespace and non-
;; comment) character.  If the suggested indentation is too much, use
;; \\[srp-electric-backspace] to reduce it.

;; Continuation lines are given extra indentation.  If you don't like the
;; suggested indentation, change it to something you do like, and Serpent-
;; mode will strive to indent later lines of the statement in the same way.

;; If a line is a continuation line by virtue of being in an unclosed
;; paren/bracket/brace structure (`list', for short), the suggested
;; indentation depends on whether the current line contains the first item
;; in the list.  If it does, it's indented srp-indent-offset columns beyond
;; the indentation of the line containing the open bracket.  If you don't
;; like that, change it by hand.  The remaining items in the list will mimic
;; whatever indentation you give to the first item.

;; If a line is a continuation line because the line preceding it ends with
;; a backslash, the third and following lines of the statement inherit their
;; indentation from the line preceding them.  The indentation of the second
;; line in the statement depends on the form of the first (base) line:  if
;; the base line is an assignment statement with anything more interesting
;; than the backslash following the leftmost assigning `=', the second line
;; is indented two columns beyond that `='.  Else it's indented to two
;; columns beyond the leftmost solid chunk of non-whitespace characters on
;; the base line.

;; Warning:  indent-region should not normally be used!  It calls \\[indent-for-tab-command]
;; repeatedly, and as explained above, \\[indent-for-tab-command] can't guess the block
;; structure you intend.
;; %c:indent-for-tab-command
;; %c:srp-newline-and-indent
;; %c:srp-electric-backspace


;; The next function may be handy when editing code you didn't write:
;; %c:srp-guess-indent-offset


;; The remaining `indent' functions apply to a region of Serpent code.  They
;; assume the block structure (equals indentation, in Serpent) of the region
;; is correct, and alter the indentation in various ways while preserving
;; the block structure:
;; %c:srp-indent-region
;; %c:srp-shift-region-left
;; %c:srp-shift-region-right

;; @MARKING & MANIPULATING REGIONS OF CODE

;; \\[srp-mark-block]\t mark block of lines
;; \\[srp-mark-def-or-class]\t mark smallest enclosing def
;; \\[universal-argument] \\[srp-mark-def-or-class]\t mark smallest enclosing class
;; \\[comment-region]\t comment out region of code
;; \\[universal-argument] \\[comment-region]\t uncomment region of code
;; %c:srp-mark-block
;; %c:srp-mark-def-or-class
;; %c:comment-region

;; @MOVING POINT

;; \\[srp-previous-statement]\t move to statement preceding point
;; \\[srp-next-statement]\t move to statement following point
;; \\[srp-goto-block-up]\t move up to start of current block
;; \\[srp-beginning-of-def-or-class]\t move to start of def
;; \\[universal-argument] \\[srp-beginning-of-def-or-class]\t move to start of class
;; \\[srp-end-of-def-or-class]\t move to end of def
;; \\[universal-argument] \\[srp-end-of-def-or-class]\t move to end of class

;; The first two move to one statement beyond the statement that contains
;; point.  A numeric prefix argument tells them to move that many
;; statements instead.  Blank lines, comment lines, and continuation lines
;; do not count as `statements' for these commands.  So, e.g., you can go
;; to the first code statement in a file by entering
;; \t\\[beginning-of-buffer]\t to move to the top of the file
;; \t\\[srp-next-statement]\t to skip over initial comments and blank lines
;; Or do `\\[srp-previous-statement]' with a huge prefix argument.
;; %c:srp-previous-statement
;; %c:srp-next-statement
;; %c:srp-goto-block-up
;; %c:srp-beginning-of-def-or-class
;; %c:srp-end-of-def-or-class

;; @LITTLE-KNOWN EMACS COMMANDS PARTICULARLY USEFUL IN SERPENT MODE

;; `\\[indent-new-comment-line]' is handy for entering a multi-line comment.

;; `\\[set-selective-display]' with a `small' prefix arg is ideally suited for viewing the
;; overall class and def structure of a module.

;; `\\[back-to-indentation]' moves point to a line's first non-blank character.

;; `\\[indent-relative]' is handy for creating odd indentation.

;; @OTHER EMACS HINTS

;; If you don't like the default value of a variable, change its value to
;; whatever you do like by putting a `setq' line in your .emacs file.
;; E.g., to set the indentation increment to 4, put this line in your
;; .emacs:
;; \t(setq  srp-indent-offset  4)
;; To see the value of a variable, do `\\[describe-variable]' and enter the variable
;; name at the prompt.

;; When entering a key sequence like `C-c C-n', it is not necessary to
;; release the CONTROL key after doing the `C-c' part -- it suffices to
;; press the CONTROL key, press and release `c' (while still holding down
;; CONTROL), press and release `n' (while still holding down CONTROL), &
;; then release CONTROL.

;; Entering Serpent mode calls with no arguments the value of the variable
;; `serpent-mode-hook', if that value exists and is not nil; for backward
;; compatibility it also tries `srp-mode-hook'; see the `Hooks' section of
;; the Elisp manual for details.

;; Obscure:  When serpent-mode is first loaded, it looks for all bindings
;; to newline-and-indent in the global keymap, and shadows them with
;; local bindings to srp-newline-and-indent."))

;; (require 'info-look)
;; ;; The info-look package does not always provide this function (it
;; ;; appears this is the case with XEmacs 21.1)
;; (when (fboundp 'info-lookup-maybe-add-help)
;;   (info-lookup-maybe-add-help
;;    :mode 'serpent-mode
;;    :regexp "[a-zA-Z0-9_]+"
;;    :doc-spec '(("(serpent-lib)Module Index")
;; 	       ("(serpent-lib)Class-Exception-Object Index")
;; 	       ("(serpent-lib)Function-Method-Variable Index")
;; 	       ("(serpent-lib)Miscellaneous Index")))
;;   )

;; 
;; ;; Helper functions
;; (defvar srp-parse-state-re
;;   (concat
;;    "^[ \t]*\\(elif\\|else\\|while\\|def\\|class\\)\\>"
;;    "\\|"
;;    "^[^ #\t\n]"))

;; (defun srp-parse-state ()
;;   "Return the parse state at point (see `parse-partial-sexp' docs)."
;;   (save-excursion
;;     (let ((here (point))
;; 	  pps done)
;;       (while (not done)
;; 	;; back up to the first preceding line (if any; else start of
;; 	;; buffer) that begins with a popular Serpent keyword, or a
;; 	;; non- whitespace and non-comment character.  These are good
;; 	;; places to start parsing to see whether where we started is
;; 	;; at a non-zero nesting level.  It may be slow for people who
;; 	;; write huge code blocks or huge lists ... tough beans.
;; 	(re-search-backward srp-parse-state-re nil 'move)
;; 	(beginning-of-line)
;; 	;; In XEmacs, we have a much better way to test for whether
;; 	;; we're in a triple-quoted string or not.  Emacs does not
;; 	;; have this built-in function, which is its loss because
;; 	;; without scanning from the beginning of the buffer, there's
;; 	;; no accurate way to determine this otherwise.
;; 	(save-excursion (setq pps (parse-partial-sexp (point) here)))
;; 	;; make sure we don't land inside a triple-quoted string
;; 	(setq done (or (not (nth 3 pps))
;; 		       (bobp)))
;; 	;; Just go ahead and short circuit the test back to the
;; 	;; beginning of the buffer.  This will be slow, but not
;; 	;; nearly as slow as looping through many
;; 	;; re-search-backwards.
;; 	(if (not done)
;; 	    (goto-char (point-min))))
;;       pps)))

;; (defun srp-nesting-level ()
;;   "Return the buffer position of the last unclosed enclosing list.
;; If nesting level is zero, return nil."
;;   (let ((status (srp-parse-state)))
;;     (if (zerop (car status))
;; 	nil				; not in a nest
;;       (car (cdr status)))))		; char# of open bracket

;; (defun srp-backslash-continuation-line-p ()
;;   "Return t iff preceding line ends with backslash that is not in a comment."
;;   (save-excursion
;;     (beginning-of-line)
;;     (and
;;      ;; use a cheap test first to avoid the regexp if possible
;;      ;; use 'eq' because char-after may return nil
;;      (eq (char-after (- (point) 2)) ?\\ )
;;      ;; make sure; since eq test passed, there is a preceding line
;;      (forward-line -1)			; always true -- side effect
;;      (looking-at srp-continued-re))))

;; (defun srp-continuation-line-p ()
;;   "Return t iff current line is a continuation line."
;;   (save-excursion
;;     (beginning-of-line)
;;     (or (srp-backslash-continuation-line-p)
;; 	(srp-nesting-level))))

;; (defun srp-goto-beginning-of-tqs (delim)
;;   "Go to the beginning of the triple quoted string we find ourselves in.
;; DELIM is the TQS string delimiter character we're searching backwards
;; for."
;;   (let ((skip (and delim (make-string 1 delim)))
;; 	(continue t))
;;     (when skip
;;       (save-excursion
;; 	(while continue
;; 	  (srp-safe (search-backward skip))
;; 	  (setq continue (and (not (bobp))
;; 			      (= (char-before) ?\\))))
;; 	(if (and (= (char-before) delim)
;; 		 (= (char-before (1- (point))) delim))
;; 	    (setq skip (make-string 3 delim))))
;;       ;; we're looking at a triple-quoted string
;;       (srp-safe (search-backward skip)))))

;; (defun srp-goto-initial-line ()
;;   "Go to the initial line of the current statement.
;; Usually this is the line we're on, but if we're on the 2nd or
;; following lines of a continuation block, we need to go up to the first
;; line of the block."
;;   ;; Tricky: We want to avoid quadratic-time behavior for long
;;   ;; continued blocks, whether of the backslash or open-bracket
;;   ;; varieties, or a mix of the two.  The following manages to do that
;;   ;; in the usual cases.
;;   ;;
;;   ;; Also, if we're sitting inside a triple quoted string, this will
;;   ;; drop us at the line that begins the string.
;;   (let (open-bracket-pos)
;;     (while (srp-continuation-line-p)
;;       (beginning-of-line)
;;       (if (srp-backslash-continuation-line-p)
;; 	  (while (srp-backslash-continuation-line-p)
;; 	    (forward-line -1))
;; 	;; else zip out of nested brackets/braces/parens
;; 	(while (setq open-bracket-pos (srp-nesting-level))
;; 	  (goto-char open-bracket-pos)))))
;;   (beginning-of-line))

;; (defun srp-goto-beyond-final-line ()
;;   "Go to the point just beyond the fine line of the current statement.
;; Usually this is the start of the next line, but if this is a
;; multi-line statement we need to skip over the continuation lines."
;;   ;; Tricky: Again we need to be clever to avoid quadratic time
;;   ;; behavior.
;;   ;;
;;   ;; XXX: Not quite the right solution, but deals with multi-line doc
;;   ;; strings
;;   (if (looking-at (concat "[ \t]*\\(" srp-stringlit-re "\\)"))
;;       (goto-char (match-end 0)))
;;   ;;
;;   (forward-line 1)
;;   (let (state)
;;     (while (and (srp-continuation-line-p)
;; 		(not (eobp)))
;;       ;; skip over the backslash flavor
;;       (while (and (srp-backslash-continuation-line-p)
;; 		  (not (eobp)))
;; 	(forward-line 1))
;;       ;; if in nest, zip to the end of the nest
;;       (setq state (srp-parse-state))
;;       (if (and (not (zerop (car state)))
;; 	       (not (eobp)))
;; 	  (progn
;; 	    (parse-partial-sexp (point) (point-max) 0 nil state)
;; 	    (forward-line 1))))))

;; (defun srp-statement-opens-block-p ()
;;   "Return t iff the current statement opens a block.
;; I.e., iff it ends with a colon that is not in a comment.  Point should
;; be at the start of a statement."
;;   (save-excursion
;;     (let ((start (point))
;; 	  (finish (progn (srp-goto-beyond-final-line) (1- (point))))
;; 	  (searching t)
;; 	  (answer nil)
;; 	  state)
;;       (goto-char start)
;;       (while searching
;; 	;; look for a colon with nothing after it except whitespace, and
;; 	;; maybe a comment
;; 	(if (re-search-forward ":\\([ \t]\\|\\\\\n\\)*\\(#.*\\)?$"
;; 			       finish t)
;; 	    (if (eq (point) finish)	; note: no `else' clause; just
;; 					; keep searching if we're not at
;; 					; the end yet
;; 		;; sure looks like it opens a block -- but it might
;; 		;; be in a comment
;; 		(progn
;; 		  (setq searching nil)	; search is done either way
;; 		  (setq state (parse-partial-sexp start
;; 						  (match-beginning 0)))
;; 		  (setq answer (not (nth 4 state)))))
;; 	  ;; search failed: couldn't find another interesting colon
;; 	  (setq searching nil)))
;;       answer)))

;; (defun srp-statement-closes-block-p ()
;;   "Return t iff the current statement closes a block.
;; I.e., if the line starts with `return', `raise', `break', `continue',
;; and `pass'.  This doesn't catch embedded statements."
;;   (let ((here (point)))
;;     (srp-goto-initial-line)
;;     (back-to-indentation)
;;     (prog1
;; 	(looking-at (concat srp-block-closing-keywords-re "\\>"))
;;       (goto-char here))))

;; (defun srp-goto-beyond-block ()
;;   "Go to point just beyond the final line of block begun by the current line.
;; This is the same as where `srp-goto-beyond-final-line' goes unless
;; we're on colon line, in which case we go to the end of the block.
;; Assumes point is at the beginning of the line."
;;   (if (srp-statement-opens-block-p)
;;       (srp-mark-block nil 'just-move)
;;     (srp-goto-beyond-final-line)))

;; (defun srp-goto-statement-at-or-above ()
;;   "Go to the start of the first statement at or preceding point.
;; Return t if there is such a statement, otherwise nil.  `Statement'
;; does not include blank lines, comments, or continuation lines."
;;   (srp-goto-initial-line)
;;   (if (looking-at srp-blank-or-comment-re)
;;       ;; skip back over blank & comment lines
;;       ;; note:  will skip a blank or comment line that happens to be
;;       ;; a continuation line too
;;       (if (re-search-backward "^[ \t]*[^ \t#\n]" nil t)
;; 	  (progn (srp-goto-initial-line) t)
;; 	nil)
;;     t))

;; (defun srp-goto-statement-below ()
;;   "Go to start of the first statement following the statement containing point.
;; Return t if there is such a statement, otherwise nil.  `Statement'
;; does not include blank lines, comments, or continuation lines."
;;   (beginning-of-line)
;;   (let ((start (point)))
;;     (srp-goto-beyond-final-line)
;;     (while (and
;; 	    (or (looking-at srp-blank-or-comment-re)
;; 		(srp-in-literal))
;; 	    (not (eobp)))
;;       (forward-line 1))
;;     (if (eobp)
;; 	(progn (goto-char start) nil)
;;       t)))

;; (defun srp-go-up-tree-to-keyword (key)
;;   "Go to begining of statement starting with KEY, at or preceding point.

;; KEY is a regular expression describing a Serpent keyword.  Skip blank
;; lines and non-indenting comments.  If the statement found starts with
;; KEY, then stop, otherwise go back to first enclosing block starting
;; with KEY.  If successful, leave point at the start of the KEY line and
;; return t.  Otherwise, leave point at an undefined place and return nil."
;;   ;; skip blanks and non-indenting #
;;   (srp-goto-initial-line)
;;   (while (and
;; 	  (looking-at "[ \t]*\\($\\|#[^ \t\n]\\)")
;; 	  (zerop (forward-line -1)))	; go back
;;     nil)
;;   (srp-goto-initial-line)
;;   (let* ((re (concat "[ \t]*" key "\\>"))
;; 	 (case-fold-search nil)		; let* so looking-at sees this
;; 	 (found (looking-at re))
;; 	 (dead nil))
;;     (while (not (or found dead))
;;       (condition-case nil		; in case no enclosing block
;; 	  (srp-goto-block-up 'no-mark)
;; 	(error (setq dead t)))
;;       (or dead (setq found (looking-at re))))
;;     (beginning-of-line)
;;     found))

;; (defun srp-suck-up-leading-text ()
;;   "Return string in buffer from start of indentation to end of line.
;; Prefix with \"...\" if leading whitespace was skipped."
;;   (save-excursion
;;     (back-to-indentation)
;;     (concat
;;      (if (bolp) "" "...")
;;      (buffer-substring (point) (progn (end-of-line) (point))))))

;; (defun srp-suck-up-first-keyword ()
;;   "Return first keyword on the line as a Lisp symbol.
;; `Keyword' is defined (essentially) as the regular expression
;; ([a-z]+).  Returns nil if none was found."
;;   (let ((case-fold-search nil))
;;     (if (looking-at "[ \t]*\\([a-z]+\\)\\>")
;; 	(intern (buffer-substring (match-beginning 1) (match-end 1)))
;;       nil)))

;; (defun srp-current-defun ()
;;   "Serpent value for `add-log-current-defun-function'.
;; This tells add-log.el how to find the current function/method/variable."
;;   (save-excursion

;;     ;; Move back to start of the current statement.

;;     (srp-goto-initial-line)
;;     (back-to-indentation)
;;     (while (and (or (looking-at srp-blank-or-comment-re)
;; 		    (srp-in-literal))
;; 		(not (bobp)))
;;       (backward-to-indentation 1))
;;     (srp-goto-initial-line)

;;     (let ((scopes "")
;; 	  (sep "")
;; 	  dead assignment)

;;       ;; Check for an assignment.  If this assignment exists inside a
;;       ;; def, it will be overwritten inside the while loop.  If it
;;       ;; exists at top lever or inside a class, it will be preserved.

;;       (when (looking-at "[ \t]*\\([a-zA-Z0-9_]+\\)[ \t]*=")
;; 	(setq scopes (buffer-substring (match-beginning 1) (match-end 1)))
;; 	(setq assignment t)
;; 	(setq sep "."))

;;       ;; Prepend the name of each outer socpe (def or class).

;;       (while (not dead)
;; 	(if (and (srp-go-up-tree-to-keyword "\\(class\\|def\\)")
;; 		 (looking-at
;; 		  "[ \t]*\\(class\\|def\\)[ \t]*\\([a-zA-Z0-9_]+\\)[ \t]*"))
;; 	    (let ((name (buffer-substring (match-beginning 2) (match-end 2))))
;; 	      (if (and assignment (looking-at "[ \t]*def"))
;; 		  (setq scopes name)
;; 		(setq scopes (concat name sep scopes))
;; 		(setq sep "."))))
;; 	(setq assignment nil)
;; 	(condition-case nil		; Terminate nicely at top level.
;; 	    (srp-goto-block-up 'no-mark)
;; 	  (error (setq dead t))))
;;       (if (string= scopes "")
;; 	  nil
;; 	scopes))))


;; 
;; (defconst srp-help-address "serpent-mode@serpent.org"
;;   "Address accepting submission of bug reports.")

;; (defun srp-version ()
;;   "Echo the current version of `serpent-mode' in the minibuffer."
;;   (interactive)
;;   (message "Using `serpent-mode' version %s" srp-version)
;;   (srp-keep-region-active))

;; ;; only works under Emacs 19
;; ;(eval-when-compile
;; ;  (require 'reporter))

;; (defun srp-submit-bug-report (enhancement-p)
;;   "Submit via mail a bug report on `serpent-mode'.
;; With \\[universal-argument] (programmatically, argument ENHANCEMENT-P
;; non-nil) just submit an enhancement request."
;;   (interactive
;;    (list (not (y-or-n-p
;; 	       "Is this a bug report (hit `n' to send other comments)? "))))
;;   (let ((reporter-prompt-for-summary-p (if enhancement-p
;; 					   "(Very) brief summary: "
;; 					 t)))
;;     (require 'reporter)
;;     (reporter-submit-bug-report
;;      srp-help-address			;address
;;      (concat "serpent-mode " srp-version)	;pkgname
;;      ;; varlist
;;      (if enhancement-p nil
;;        '(srp-serpent-command
;; 	 srp-indent-offset
;; 	 srp-block-comment-prefix
;; 	 srp-temp-directory
;; 	 srp-beep-if-tab-change))
;;      nil				;pre-hooks
;;      nil				;post-hooks
;;      "Dear Barry,")			;salutation
;;     (if enhancement-p nil
;;       (set-mark (point))
;;       (insert
;; "Please replace this text with a sufficiently large code sample\n\
;; and an exact recipe so that I can reproduce your problem.  Failure\n\
;; to do so may mean a greater delay in fixing your bug.\n\n")
;;       (exchange-point-and-mark)
;;       (srp-keep-region-active))))

;; 
;; (defun srp-kill-emacs-hook ()
;;   "Delete files in `srp-file-queue'.
;; These are Serpent temporary files awaiting execution."
;;   (mapcar #'(lambda (filename)
;; 	      (srp-safe (delete-file filename)))
;; 	  srp-file-queue))

;; ;; arrange to kill temp files when Emacs exists
;; (add-hook 'kill-emacs-hook 'srp-kill-emacs-hook)
;; (add-hook 'comint-output-filter-functions 'srp-pdbtrack-track-stack-file)

;; ;; Add a designator to the minor mode strings
;; (or (assq 'srp-pdbtrack-is-tracking-p minor-mode-alist)
;;     (push '(srp-pdbtrack-is-tracking-p srp-pdbtrack-minor-mode-string)
;; 	  minor-mode-alist))


;; 
;; ;;; paragraph and string filling code from Bernhard Herzog
;; ;;; see http://mail.serpent.org/pipermail/serpent-list/2002-May/103189.html

;; (defun srp-fill-comment (&optional justify)
;;   "Fill the comment paragraph around point"
;;   (let (;; Non-nil if the current line contains a comment.
;; 	has-comment

;; 	;; If has-comment, the appropriate fill-prefix for the comment.
;; 	comment-fill-prefix)

;;     ;; Figure out what kind of comment we are looking at.
;;     (save-excursion
;;       (beginning-of-line)
;;       (cond
;;        ;; A line with nothing but a comment on it?
;;        ((looking-at "[ \t]*#[# \t]*")
;; 	(setq has-comment t
;; 	      comment-fill-prefix (buffer-substring (match-beginning 0)
;; 						    (match-end 0))))

;;        ;; A line with some code, followed by a comment? Remember that the hash
;;        ;; which starts the comment shouldn't be part of a string or character.
;;        ((progn
;; 	  (while (not (looking-at "#\\|$"))
;; 	    (skip-chars-forward "^#\n\"'\\")
;; 	    (cond
;; 	     ((eq (char-after (point)) ?\\) (forward-char 2))
;; 	     ((memq (char-after (point)) '(?\" ?')) (forward-sexp 1))))
;; 	  (looking-at "#+[\t ]*"))
;; 	(setq has-comment t)
;; 	(setq comment-fill-prefix
;; 	      (concat (make-string (current-column) ? )
;; 		      (buffer-substring (match-beginning 0) (match-end 0)))))))

;;     (if (not has-comment)
;; 	(fill-paragraph justify)

;;       ;; Narrow to include only the comment, and then fill the region.
;;       (save-restriction
;; 	(narrow-to-region

;; 	 ;; Find the first line we should include in the region to fill.
;; 	 (save-excursion
;; 	   (while (and (zerop (forward-line -1))
;; 		       (looking-at "^[ \t]*#")))

;; 	   ;; We may have gone to far.  Go forward again.
;; 	   (or (looking-at "^[ \t]*#")
;; 	       (forward-line 1))
;; 	   (point))

;; 	 ;; Find the beginning of the first line past the region to fill.
;; 	 (save-excursion
;; 	   (while (progn (forward-line 1)
;; 			 (looking-at "^[ \t]*#")))
;; 	   (point)))

;; 	;; Lines with only hashes on them can be paragraph boundaries.
;; 	(let ((paragraph-start (concat paragraph-start "\\|[ \t#]*$"))
;; 	      (paragraph-separate (concat paragraph-separate "\\|[ \t#]*$"))
;; 	      (fill-prefix comment-fill-prefix))
;; 	  ;;(message "paragraph-start %S paragraph-separate %S"
;; 	  ;;paragraph-start paragraph-separate)
;; 	  (fill-paragraph justify))))
;;     t))


;; (defun srp-fill-string (start &optional justify)
;;   "Fill the paragraph around (point) in the string starting at start"
;;   ;; basic strategy: narrow to the string and call the default
;;   ;; implementation
;;   (let (;; the start of the string's contents
;; 	string-start
;; 	;; the end of the string's contents
;; 	string-end
;; 	;; length of the string's delimiter
;; 	delim-length
;; 	;; The string delimiter
;; 	delim
;; 	)

;;     (save-excursion
;;       (goto-char start)
;;       (if (looking-at "\\('''\\|\"\"\"\\|'\\|\"\\)\\\\?\n?")
;; 	  (setq string-start (match-end 0)
;; 		delim-length (- (match-end 1) (match-beginning 1))
;; 		delim (buffer-substring-no-properties (match-beginning 1)
;; 						      (match-end 1)))
;; 	(error "The parameter start is not the beginning of a serpent string"))

;;       ;; if the string is the first token on a line and doesn't start with
;;       ;; a newline, fill as if the string starts at the beginning of the
;;       ;; line. this helps with one line docstrings
;;       (save-excursion
;; 	(beginning-of-line)
;; 	(and (/= (char-before string-start) ?\n)
;; 	     (looking-at (concat "[ \t]*" delim))
;; 	     (setq string-start (point))))

;;       (forward-sexp (if (= delim-length 3) 2 1))

;;       ;; with both triple quoted strings and single/double quoted strings
;;       ;; we're now directly behind the first char of the end delimiter
;;       ;; (this doesn't work correctly when the triple quoted string
;;       ;; contains the quote mark itself). The end of the string's contents
;;       ;; is one less than point
;;       (setq string-end (1- (point))))

;;     ;; Narrow to the string's contents and fill the current paragraph
;;     (save-restriction
;;       (narrow-to-region string-start string-end)
;;       (let ((ends-with-newline (= (char-before (point-max)) ?\n)))
;; 	(fill-paragraph justify)
;; 	(if (and (not ends-with-newline)
;; 		 (= (char-before (point-max)) ?\n))
;; 	    ;; the default fill-paragraph implementation has inserted a
;; 	    ;; newline at the end. Remove it again.
;; 	    (save-excursion
;; 	      (goto-char (point-max))
;; 	      (delete-char -1)))))

;;     ;; return t to indicate that we've done our work
;;     t))

;; (defun srp-fill-paragraph (&optional justify)
;;   "Like \\[fill-paragraph], but handle Serpent comments and strings.
;; If any of the current line is a comment, fill the comment or the
;; paragraph of it that point is in, preserving the comment's indentation
;; and initial `#'s.
;; If point is inside a string, narrow to that string and fill.
;; "
;;   (interactive "P")
;;   ;; fill-paragraph will narrow incorrectly
;;   (save-restriction
;;     (widen)
;;     (let* ((bod (srp-point 'bod))
;; 	   (pps (parse-partial-sexp bod (point))))
;;       (cond
;;        ;; are we inside a comment or on a line with only whitespace before
;;        ;; the comment start?
;;        ((or (nth 4 pps)
;; 	    (save-excursion (beginning-of-line) (looking-at "[ \t]*#")))
;; 	(srp-fill-comment justify))
;;        ;; are we inside a string?
;;        ((nth 3 pps)
;; 	(srp-fill-string (nth 8 pps)))
;;        ;; are we at the opening quote of a string, or in the indentation?
;;        ((save-excursion
;; 	  (forward-word 1)
;; 	  (eq (srp-in-literal) 'string))
;; 	(save-excursion
;; 	  (srp-fill-string (srp-point 'boi))))
;;        ;; are we at or after the closing quote of a string?
;;        ((save-excursion
;; 	  (backward-word 1)
;; 	  (eq (srp-in-literal) 'string))
;; 	(save-excursion
;; 	  (srp-fill-string (srp-point 'boi))))
;;        ;; otherwise use the default
;;        (t
;; 	(fill-paragraph justify))))))


;; (provide 'serpent-mode)

;; ;;; serpent-mode.el ends here
