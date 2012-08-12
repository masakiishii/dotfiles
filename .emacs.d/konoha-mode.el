;;; konoha-mode.el --- Konoha mode derived mode

;; Author:     2009 Shinpei Nakata
;; Created:    2009 July the 17th
;; Modified:   July 2009
;; Version:    0.0.1
;; Keywords:   konoha languages oop

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;;    See http://konoha.sourceforge.jp for Konoha scripting language details
;;
;;    Note: The interface used in this file requires CC Mode 5.30 or
;;    later.

;;; .emacs (don't put in (require 'konoha-mode))
;; (autoload 'konoha-mode "konoha-mode" "Major mode for editing Konoha code." t)
;; (setq auto-mode-alist
;;    (append '(("\\.k$" . konoha-mode)) auto-mode-alist))

;;; Versions:
;;
;;	0.0.1	: Initial version based on csharp-mode
;;

;; This is a copy of the function in cc-mode which is used to handle
;; the eval-when-compile which is needed during other times.
(defun c-filter-ops (ops opgroup-filter op-filter &optional xlate)
  ;; See cc-langs.el, a direct copy.
  (unless (listp (car-safe ops))
    (setq ops (list ops)))
  (cond ((eq opgroup-filter t)
	 (setq opgroup-filter (lambda (opgroup) t)))
	((not (functionp opgroup-filter))
	 (setq opgroup-filter `(lambda (opgroup)
				 (memq opgroup ',opgroup-filter)))))
  (cond ((eq op-filter t)
	 (setq op-filter (lambda (op) t)))
	((stringp op-filter)
	 (setq op-filter `(lambda (op)
			    (string-match ,op-filter op)))))
  (unless xlate
    (setq xlate 'identity))
  (c-with-syntax-table (c-lang-const c-mode-syntax-table)
    (delete-duplicates
     (mapcan (lambda (opgroup)
	       (when (if (symbolp (car opgroup))
			 (when (funcall opgroup-filter (car opgroup))
			   (setq opgroup (cdr opgroup))
			   t)
		       t)
		 (mapcan (lambda (op)
			   (when (funcall op-filter op)
			     (let ((res (funcall xlate op)))
			       (if (listp res) res (list res)))))
			 opgroup)))
	     ops)
     :test 'equal)))

;; This inserts the bulk of the code.
(require 'cc-mode)

;; These are only required at compile time to get the sources for the
;; language constants.  (The cc-fonts require and the font-lock
;; related constants could additionally be put inside an
;; (eval-after-load "font-lock" ...) but then some trickery is
;; necessary to get them compiled.)
(eval-when-compile
  (let ((load-path
	 (if (and (boundp 'byte-compile-dest-file)
		  (stringp byte-compile-dest-file))
	     (cons (file-name-directory byte-compile-dest-file) load-path)
	   load-path)))
    (load "cc-mode" nil t)
    (load "cc-fonts" nil t)
    (load "cc-langs" nil t)))

(eval-and-compile
  ;; Make our mode known to the language constant system.  Use Java
  ;; mode as the fallback for the constants we don't change here.
  ;; This needs to be done also at compile time since the language
  ;; constants are evaluated then.
  (c-add-language 'konoha-mode 'java-mode))

;; Java uses a series of regexes to change the font-lock for class
;; references. The problem comes in because Java uses Pascal (leading
;; space in names, SomeClass) for class and package names, but
;; Camel-casing (initial lowercase, upper case in words,
;; i.e. someVariable) for variables.
;;(error (byte-compile-dest-file))
;;(error (c-get-current-file))
(c-lang-defconst c-opt-after-id-concat-key
  konoha (if (c-lang-const c-opt-identifier-concat-key)
	   (c-lang-const c-symbol-start)))

(c-lang-defconst c-basic-matchers-before
  konoha `(
;;;; Font-lock the attributes by searching for the
;;;; appropriate regex and marking it as TODO.
	 ;;,`(,(concat "\\(" konoha-attribute-regex "\\)")
	 ;;   0 font-lock-function-name-face)	   

	 ;; Put a warning face on the opener of unclosed strings that
	 ;; can't span lines.  Later font
	 ;; lock packages have a `font-lock-syntactic-face-function' for
	 ;; this, but it doesn't give the control we want since any
	 ;; fontification done inside the function will be
	 ;; unconditionally overridden.
	 ,(c-make-font-lock-search-function
	   ;; Match a char before the string starter to make
	   ;; `c-skip-comments-and-strings' work correctly.
	   (concat ".\\(" c-string-limit-regexp "\\)")
	   '((c-font-lock-invalid-string)))
	   
	 ;; Fontify keyword constants.
	 ,@(when (c-lang-const c-constant-kwds)
	     (let ((re (c-make-keywords-re nil
			 (c-lang-const c-constant-kwds))))
	       `((eval . (list ,(concat "\\<\\(" re "\\)\\>")
			       1 c-constant-face-name)))))
	   
	 ;; Fontify all keywords except the primitive types.
	 ,`(,(concat "\\<" (c-lang-const c-regular-keywords-regexp))
	    1 font-lock-keyword-face)

	 ;; Fontify leading identifiers in fully
	 ;; qualified names like "Foo.Bar".
	 ,@(when (c-lang-const c-opt-identifier-concat-key)
	     `((,(byte-compile
		  `(lambda (limit)
		     (while (re-search-forward
			     ,(concat "\\(\\<" ; 1
				      "\\(" (c-lang-const c-symbol-key)
				      "\\)" ; 2
				      "[ \t\n\r\f\v]*"
				      (c-lang-const
				       c-opt-identifier-concat-key)
				      "[ \t\n\r\f\v]*"
				      "\\)"
				      "\\("
				      (c-lang-const
				       c-opt-after-id-concat-key)
				      "\\)")
			     limit t)
		       (unless (progn
				 (goto-char (match-beginning 0))
				 (c-skip-comments-and-strings limit))
			 (or (get-text-property (match-beginning 2) 'face)
			     (c-put-font-lock-face (match-beginning 2)
						   (match-end 2)
						   c-reference-face-name))
			 (goto-char (match-end 1)))))))))
	 ))

;; Konoha does not allow a leading qualifier operator. It also doesn't
;; allow the ".*" construct of Java. So, we redo this regex without
;; the "\\|\\*" regex.
(c-lang-defconst c-identifier-key
  konoha (concat "\\(" (c-lang-const c-symbol-key) "\\)" ; 1
	       (concat "\\("
		       "[ \t\n\r\f\v]*"
		       (c-lang-const c-opt-identifier-concat-key)
		       "[ \t\n\r\f\v]*"
		       (concat "\\("
			       "\\(" (c-lang-const c-symbol-key) "\\)"
			       "\\)")
		       "\\)*")))


;; konoha directives ?
;; (c-lang-defconst c-opt-cpp-prefix
;;   csharp "^\\s *#.*")


;; konoha uses the following assignment operators
(c-lang-defconst c-assignment-operators
  konoha '("=" "*=" "/=" "%=" "+=" "-=" "<<=" ">>=" 
	   "&=" "^=" "|=" "++" "--"))

;; This defines the primative types for Vala
(c-lang-defconst c-primitive-type-kwds
  konoha '("dynamic" "void" "boolean" "int" "float" "double" "string"))

;; The keywords that define that the following is a type, such as a
;; class definition.
;;(c-lang-defconst c-type-prefix-kwds
;;  konoha '("class" "@Native" "try" "catch"))

;; Type modifier keywords. They appear anywhere in types, but modifiy
;; instead create one.
;;(c-lang-defconst c-type-modifier-kwds
;;  konoha '("const" "@Message" "@Private" "@Public" "@Hidden" "@Native"))

;; Structures that are similiar to classes.
(c-lang-defconst c-class-decl-kwds
  konoha '("class" "interface" "assure"))

;; The various modifiers used for class and method descriptions.
(c-lang-defconst c-modifier-kwds
  konoha '("@Public" "@Private" "@Fastcall" "@Abstract"
	 "@Hidden" "in" "out" "static" "@Virtual" "@Native"
	 "@Override"))

;; We don't use the protection level stuff because it breaks the
;; method indenting. Not sure why, though.
(c-lang-defconst c-protection-kwds
  konoha nil)

;; Define the keywords that can have something following after them.
(c-lang-defconst c-type-list-kwds
  konoha '("class" "interface" "is" "as"
	 "delegate" "set" "get" 
	 "callback" "var"))

;; This allows the classes after the : in the class declartion to be
;; fontified. 
(c-lang-defconst c-typeless-decl-kwds
  konoha '(":"))

;; We need to remove Java's package keyword
(c-lang-defconst c-ref-list-kwds
  konoha '("include" "using" "namespace"))

;; Follow-on blocks that don't require a brace
(c-lang-defconst c-block-stmt-2-kwds
  konoha '("for" "if" "switch" "while" "catch" "foreach" "do"))

;; Statements that break out of braces
(c-lang-defconst c-simple-stmt-kwds
  konoha '("return" "continue" "break" "throw"))

;; Statements that allow a label
;; TODO?
(c-lang-defconst c-before-label-kwds
  konoha nil)

;; Constant keywords
(c-lang-defconst c-constant-kwds
  konoha '("true" "false" "null"))

;; Keywords that start "primary expressions."
(c-lang-defconst c-primary-expr-kwds
  konoha '("this"))

;; We need to treat namespace as an outer block to class indenting
;; works properly.
(c-lang-defconst c-other-block-decl-kwds
  konoha '("namespace"))

;; We need to include the "in" for the foreach
(c-lang-defconst c-other-kwds
  konoha '("in?" "instanceof" "typeof" "assert"))

(require 'cc-awk)

(c-lang-defconst c-at-vsemi-p-fn
  konoha 'c-awk-at-vsemi-p)

(defcustom konoha-font-lock-extra-types nil
  "*List of extra types (aside from the type keywords) to recognize in Konoha mode.
Each list item should be a regexp matching a single identifier.")

(defconst konoha-font-lock-keywords-1 (c-lang-const c-matchers-1 konoha)
  "Minimal highlighting for Konoha mode.")

(defconst konoha-font-lock-keywords-2 (c-lang-const c-matchers-2 konoha)
  "Fast normal highlighting for Konoha mode.")

(defconst konoha-font-lock-keywords-3 (c-lang-const c-matchers-3 konoha)
  "Accurate normal highlighting for Konoha mode.")

(defvar konoha-font-lock-keywords konoha-font-lock-keywords-3
  "Default expressions to highlight in Konoha mode.")

(defvar konoha-mode-syntax-table
  nil
  "Syntax table used in konoha-mode buffers.")
(or konoha-mode-syntax-table
    (setq konoha-mode-syntax-table
	  (funcall (c-lang-const c-make-mode-syntax-table konoha))))

(defvar konoha-mode-abbrev-table nil
  "Abbreviation table used in konoha-mode buffers.")
(c-define-abbrev-table 'konoha-mode-abbrev-table
  ;; Keywords that if they occur first on a line
  ;; might alter the syntactic context, and which
  ;; therefore should trig reindentation when
  ;; they are completed.
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)
    ("catch" "catch" c-electric-continued-statement 0)
    ("finally" "finally" c-electric-continued-statement 0)))

(defvar konoha-mode-map (let ((map (c-make-inherited-keymap)))
			;; Add bindings which are only useful for Konoha
			map)
  "Keymap used in konoha-mode buffers.")

;;(easy-menu-define konoha-menu konoha-mode-map "Konoha Mode Commands"
;;		  ;; Can use `konoha' as the language for `c-mode-menu'
;;		  ;; since its definition covers any language.  In
;;		  ;; this case the language is used to adapt to the
;;		  ;; nonexistence of a cpp pass and thus removing some
;;		  ;; irrelevant menu alternatives.
;;		  (cons "Konoha" (c-lang-const c-mode-menu konoha)))

;;; Autoload mode trigger
(add-to-list 'auto-mode-alist '("\\.k$" . konoha-mode))

;; Custom variables
(defcustom konoha-mode-hook nil
  "*Hook called by `konoha-mode'."
  :type 'hook
  :group 'c)

;;; The entry point into the mode
;;;###autoload
(defun konoha-mode ()
  "Major mode for editing Konoha code.
This is a simple example of a separate mode derived from CC Mode
to support a language with syntax similar to
C#/C/C++/ObjC/Java/IDL/Pike.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `konoha-mode-hook'.

Key bindings:
\\{konoha-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table konoha-mode-syntax-table)
  (setq major-mode 'konoha-mode
	mode-name "Konoha"
	local-abbrev-table konoha-mode-abbrev-table
	abbrev-mode t)
  (use-local-map c-mode-map)
  ;; `c-init-language-vars' is a macro that is expanded at compile
  ;; time to a large `setq' with all the language variables and their
  ;; customized values for our language.
  (c-init-language-vars konoha-mode)
  ;; `c-common-init' initializes most of the components of a CC Mode
  ;; buffer, including setup of the mode menu, font-lock, etc.
  ;; There's also a lower level routine `c-basic-common-init' that
  ;; only makes the necessary initialization to get the syntactic
  ;; analysis and similar things working.
  (c-common-init 'konoha-mode)
  ;;(easy-menu-add konoha-menu)
  (c-set-style "linux")
  (setq indent-tabs-mode t)
  (setq c-basic-offset 4)
  (c-toggle-auto-newline -1)
  (c-toggle-hungry-state -1)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'konoha-mode-hook)
  (c-update-modeline))

(provide 'konoha-mode)

;;; konoha-mode.el ends here
