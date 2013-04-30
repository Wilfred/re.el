;;; re.el --- a friendly regexp library

;; Copyright (C) 2013 Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Version: 0.3
;; Keywords: regexp, regular expression
;; Package-Requires: ((dash "1.2.0"))

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

;; I find Emacs' built-in regexp library tricky to use. Maybe you do
;; too.

;; re.el functions are pure: you simply call them. They don't modify
;; any global variables (existing match data is unchanged) and don't
;; require you to set any global variables to configure them.

;; re.el provides functions that just return a match string rather
;; than using several different variables for all the different match
;; details.

;;; Code:

;; todo: let-bind search-whitespace-regexp in all these functions

;;; search functions
(defun re-find (string pattern &optional ignore-case))

(defun re-find-all (string pattern &optional ignore-case)
  "Find all matches of REGEXP in STRING, return a list of the
matching substrings. Case sensitive unless IGNORE-CASE is non-nil."
  (save-match-data
    (re--find-all pattern string 0 ignore-case)))

;; fixme: potential stack overflow
(defun re--find-all (pattern string offset ignore-case)
  "Recursively find all matches of regexp PATTERN in STRING starting at OFFSET.
Return a list of the matching substrings."
  (let* ((case-fold-search ignore-case)
         (match-start-index (string-match pattern string offset)))
    (when match-start-index
      (cons
       (substring string (match-beginning 0) (match-end 0))
       (re--find-all pattern string (match-end 0) ignore-case)))))

(defun re-find-p (string pattern &optional ignore-case))

;;; match metadata
(defun re-match-text (match))
(defun re-match-start (match))
(defun re-match-end (match))

;;; miscellaneous
(defun re-strip-properties (string))

(defun re-from-pcre (pattern)
  "Given a perl-compatible regular expression PATTERN, convert it to an Emacs regexp.

Not all PCRE patterns can be converted to Emacs regexps, and
`re-from-pcre' will throw an error in those situations. There is
also no Emacs equivalent of PCRE's 'dotall', so . never matches
newlines in Emacs.

Note that if you use this in core emacs functions, you will need to set `case-fold-search'
and `search-whitespace-regexp'."
  (-> pattern
    (re--invert-quoting ?\()
    (re--invert-quoting ?\))
    (re--invert-quoting ?|)))

(defun re--invert-quoting (string char)
  "Replace all quoted instances of CHAR with \\CHAR and vice versa in STRING."
  (let ((inverted "")
        (index 0)
        current-char
        next-char)
        (loop until (equal index (length string)) do
          (setq current-char (elt string index))
          (setq next-char
                (unless (equal (1+ index) (length string))
                  (elt string (1+ index))))

          (cond
           ;; unescape if it's escaped
           ((and (equal current-char ?\\) (equal next-char char))
            (re--concat! inverted char)
            (incf index 2))
           ;; escape if it's unescaped
           ((equal current-char char)
            (re--concat! inverted (list ?\\ char))
            (incf index))
           ;; any other character
           (t
            (re--concat! inverted current-char)
            (incf index))))
        inverted))

(defun re--paren-p (char)
  "Is CHAR an open or closing parenthesis?"
  (or (equal char ?\() (equal char ?\))))

(defmacro re--concat! (string suffix)
  "Set STRING to the concatenation of STRING and string or character SUFFIX."
  `(setq ,string
         (concat ,string
                 (if (characterp ,suffix) (list ,suffix) ,suffix))))

(defun re-quote (string))

(defun re-pcre-quote (string)
  "Return a PCRE regexp that matches exactly STRING and nothing else.
Useful for passing arguments to external commands.")

;;; string to string functions
(defun re-split (string pattern &optional ignore-case))
(defun re-replace (string pattern &optional ignore-case))

(provide 're)
;;; re.el ends here
