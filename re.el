;;; re.el --- a friendly regexp library

;;; Commentary

;; I find Emacs' built-in regexp library tricky to use. Maybe you do
;; too.

;; re.el provides functions that just return a match string rather
;; than using several different variables for all the different match
;; details.

;;; search functions
(defun re-find (pattern string))
(defun re-find-all (pattern string))
(defun re-find-p (pattern string))

;;; match metadata
(defun re-match-text (match))
(defun re-match-start (match))
(defun re-match-end (match))

;;; miscellaneous
(defun re-strip-properties (string))
(defun re-from-pcre (pattern))

;;; string to string functions
(defun re-split (pattern string))
(defun re-replace (pattern string))
