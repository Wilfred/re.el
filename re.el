;;; re.el --- a friendly regexp library

;;; Commentary

;; I find Emacs' built-in regexp library tricky to use. Maybe you do
;; too.

;; re.el provides functions that just return a match string rather
;; than using several different variables for all the different match
;; details.

;;; search functions
(defun re-find (string pattern &optional ignore-case))
(defun re-find-all (string pattern &optional ignore-case))
(defun re-find-p (string pattern &optional ignore-case))

;;; match metadata
(defun re-match-text (match))
(defun re-match-start (match))
(defun re-match-end (match))

;;; miscellaneous
(defun re-strip-properties (string))
(defun re-from-pcre (pattern))

;;; string to string functions
(defun re-split (string pattern &optional ignore-case))
(defun re-replace (string pattern &optional ignore-case))
